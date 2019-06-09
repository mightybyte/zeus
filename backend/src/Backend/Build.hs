{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Build where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Error
import qualified Control.Exception as C
import           Control.Lens
import           Control.Monad
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.RNG
import           Data.String.Conv
import qualified Data.Text as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Mem.Weak
import           System.Process
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Types.ServerEnv
import           Backend.WsCmds
import           Common.Api
import           Common.Types.ConnectedAccount
import           Backend.Types.ConnRepo
import           Common.Types.BuildJob
import           Common.Types.BuildMsg
import           Common.Types.JobStatus
import           Common.Types.ProcMsg
import           Common.Types.Repo
import           Common.Types.RepoBuildInfo
import           Which
------------------------------------------------------------------------------

buildManagerThread :: ServerEnv -> IO ()
buildManagerThread se = do
  let dbConn = _serverEnv_db se
      buildQueue = _serverEnv_buildQueue se
  rng <- mkRNG
  forever $ do
    msg <- atomically $ readTQueue buildQueue
    let rbi = _buildMsg_repoBuildInfo msg
    case _rbi_repoEvent rbi of
      RepoPullRequest ->
        printf "Ignoring pull request message on %s commit %s\n"
               (_rbi_repoFullName rbi) (_rbi_commitHash rbi)
      RepoPush -> do
        printf "Got push message on %s\n"
               (_rbi_repoFullName rbi)
        printf "Pushed commit %s to %s\n"
               (_rbi_commitHash rbi) (_rbi_gitRef rbi)
        repos <- runBeamSqlite dbConn $
          runSelectReturningList $ select $ do
            account <- all_ (_ciDb_connectedAccounts ciDb)
            repo <- all_ (_ciDb_repos ciDb)
            guard_ (_repo_fullName repo ==. val_ (_rbi_repoFullName rbi))
            guard_ (account ^. connectedAccount_id ==. repo ^. repo_owner)
            return repo
        case repos of
          [] -> putStrLn "Warning: Got a webhook for a repo that is not in our DB.  Is the DB corrupted?"
          [r] -> runBuild se rng r msg
          _ -> printf "Warning: Got more repos than expected.  Why isn't %s unique?\n" (_rbi_repoFullName rbi)

runBuild
  :: ServerEnv
  -> RNG
  -> Repo
  -> BuildMsg
  -> IO ()
runBuild se rng repo msg = do
  let dbConn = _serverEnv_db se
      connRepo = _serverEnv_connRepo se
  start <- getCurrentTime
  runBeamSqliteDebug putStrLn dbConn $ do
    runUpdate $
      update (_ciDb_buildJobs ciDb)
             (\job -> mconcat
                        [ job ^. buildJob_startedAt <-. val_ (Just start)
                        , job ^. buildJob_status <-. val_ JobInProgress ])
             (\job -> _buildJob_id job ==. val_ (_buildMsg_jobId msg))
    return ()
  broadcastJobs dbConn connRepo

  ecMVar <- newEmptyMVar
  wtid <- mkWeakThreadId =<< forkIO (buildThread se ecMVar rng repo msg)
  let jobId = fromIntegral $ _buildMsg_jobId msg
  atomicModifyIORef (_serverEnv_buildThreads se) $ \m ->
    (M.insert jobId wtid m, ())
  jobStatus <- threadWatcher
    (_serverEnv_buildThreads se)
    start
    (fromIntegral $ _repo_timeout repo)
    ecMVar wtid jobId

  end <- getCurrentTime
  runBeamSqliteDebug putStrLn dbConn $ do
    runUpdate $
      update (_ciDb_buildJobs ciDb)
             (\job -> mconcat
                        [ job ^. buildJob_endedAt <-. val_ (Just end)
                        , job ^. buildJob_status <-. val_ jobStatus ])
             (\job -> _buildJob_id job ==. val_ (_buildMsg_jobId msg))

  broadcastJobs dbConn connRepo
  return ()

threadWatcher
  :: IORef (Map Int (Weak ThreadId))
  -> UTCTime
  -> NominalDiffTime
  -- ^ The build timeout in seconds
  -> MVar ExitCode
  -> Weak ThreadId
  -> Int
  -> IO JobStatus
threadWatcher buildThreads start timeout ecMVar wtid jobId = go
  where
    go = do
      mec <- tryReadMVar ecMVar
      case mec of
        Nothing -> do
          now <- getCurrentTime
          if diffUTCTime now start > timeout
            then do
              mtid <- deRefWeak wtid
              case mtid of
                Nothing -> return JobVanished
                Just tid -> do
                  atomicModifyIORef buildThreads $ \m ->
                    (M.delete jobId m, ())
                  killThread tid
                  return JobTimedOut
            else do

              threadDelay 5000000 >> go
        Just ec -> return $ exitCodeToStatus ec

gitBinary :: String
gitBinary = $(staticWhich "git")

nixBuildBinary :: String
nixBuildBinary = $(staticWhich "nix-build")

microsSince :: UTCTime -> IO Int
microsSince start = do
    -- Calculates how much to delay to get to the timeout
    now <- getCurrentTime
    return $ round $ realToFrac (diffUTCTime now start) * (1000000 :: Double)

microsToDelay
  :: Int
  -- ^ Timeout in seconds
  -> UTCTime
  -> IO Int
microsToDelay timeout start = do
    micros <- microsSince start
    return $ timeout * 1000000 - micros

buildOutputDir :: String
buildOutputDir = "log/builds"

sendOutput :: ServerEnv -> Int -> ProcMsg -> IO ()
sendOutput se jid msg = do
    listeners <- readIORef (_serverEnv_buildListeners se)
    case M.lookup jid listeners of
      Nothing -> return ()
      Just connIds -> do
        forM_ connIds $ \connId ->
          sendToConnId (_serverEnv_connRepo se) connId (Down_JobNewOutput $ (BuildJobId jid, [msg]))

buildThread
  :: ServerEnv
  -> MVar ExitCode
  -> RNG
  -> Repo
  -> BuildMsg
  -> IO ()
buildThread se ecMVar rng repo bm = do
  start <- getCurrentTime
  let rbi = _buildMsg_repoBuildInfo bm
  tok <- randomToken 8 rng
  let url = case _repo_cloneMethod repo of
              SshClone -> _rbi_cloneUrlSsh rbi
              HttpClone -> _rbi_cloneUrlHttp rbi
  let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S" start
  let buildId = printf "build-%s-%s" timeStr (toS tok :: String) :: String
  let cloneDir = printf "/tmp/zeus-builds/%s" buildId :: String
  createDirectoryIfMissing True cloneDir
  createDirectoryIfMissing True buildOutputDir
  let outputFile = printf "%s/%d.output" buildOutputDir (_buildMsg_jobId bm)
  printf "Writing build output to %s\n" outputFile
  withLogHandle outputFile $ \lh  -> do
    let cloneCmd = printf "%s clone %s" gitBinary url
        saveAndSendStr msgTy msg = do
          !t <- getCurrentTime
          let pm = ProcMsg t msgTy msg
          saveAndSend pm
        saveAndSend pm = do
          hPutStrLn lh $! prettyProcMsg pm
          sendOutput se (_buildMsg_jobId bm) pm

    saveAndSendStr CiMsg $ T.pack $ printf "Cloning %s to %s" (_repo_fullName repo) cloneDir
    let handleCloneOutput inH pm@(ProcMsg _ _ m) = do
          if
            | "Username" `T.isPrefixOf` m -> do
              saveAndSendStr CiMsg $ T.pack $ printf "Got login prompt: %s" m
              maybe (return ()) (\h -> hPutStrLn h "mightybyte") inH -- TODO Replace with correct username
            | "Password" `T.isPrefixOf` m -> do
              saveAndSendStr CiMsg $ T.pack $ printf "Got password prompt: %s" m
              maybe (return ()) (\h -> hPutStrLn h "blah") inH -- TODO Replace with correct username
            | otherwise -> return ()
          saveAndSend pm
    let handleBuildOutput :: Maybe Handle -> ProcMsg -> IO ()
        handleBuildOutput _ pm = do
          saveAndSend pm
          return ()
    res <- runExceptT $ do
      _ <- runCmd (microsToDelay 3600 start) cloneCmd cloneDir Nothing handleCloneOutput
      let repoDir = cloneDir </> toS (_repo_name repo)
      let checkout = printf "%s checkout %s" gitBinary (_rbi_commitHash rbi)
      _ <- runCmd (microsToDelay 3600 start) checkout repoDir Nothing handleBuildOutput
      let buildCmd = printf "%s --show-trace %s" nixBuildBinary (_repo_buildNixFile repo)
      e <- liftIO getEnvironment
      let buildEnv = M.toList $ M.insert "NIX_PATH" (toS $ _repo_nixPath repo) $ M.fromList e
      _ <- runCmd (microsToDelay 3600 start) buildCmd repoDir (Just buildEnv) handleBuildOutput
      end <- liftIO getCurrentTime
      let finishMsg = printf "Build finished in %.3f seconds" (realToFrac (diffUTCTime end start) :: Double)
      liftIO $ saveAndSendStr CiMsg $ T.pack (finishMsg ++ "\n")
      liftIO $ putStrLn finishMsg
      return ExitSuccess
    case res of
      Left ec -> do
        liftIO $ printf "Build failed with code %s\n" (show ec)
        putMVar ecMVar ec
      Right ec -> do
        liftIO $ printf "Build succeeded with %s\n" (show ec)
        putMVar ecMVar ec

data GitMsgType = UsernameMessage String | PasswordMessage String | OtherMessage

exitCodeToStatus :: ExitCode -> JobStatus
exitCodeToStatus ExitSuccess = JobSucceeded
exitCodeToStatus (ExitFailure _) = JobFailed

withLogHandle :: FilePath -> (Handle -> IO a) -> IO a
withLogHandle fp action = withFile fp AppendMode $ \h -> do
  hSetBuffering h NoBuffering
  action h

withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
    C.bracket (createProcess_ fun c) cleanup
              (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)
  where
    cleanup (_, _, _, ph) = terminateProcess ph


--runInDirWithEnv :: Handle -> String -> FilePath -> Maybe [(String, String)] -> IO ExitCode
--runInDirWithEnv lh cmd dir e = do
--  let cp = (shell cmd)
--        { cwd = Just dir
--        , env = e
--        , std_out = UseHandle lh
--        , std_err = UseHandle lh
--        }
--  hPutStrLn lh $ printf "Executing command from %s:" (show $ cwd cp)
--  logWithTimestamp lh $ getCommand cp
--  exitCode <- withCreateProcess_ "runInDirWithEnv" cp $ \inh outh errh ph -> do
--    -- TODO Properly handle stdout and stderr
--    ec <- waitForProcess ph
--    -- TODO Terminate process if it runs too long
--    return ec
--
--  hPutStrLn lh $ "Exited with: " ++ show exitCode
--  return exitCode

runCmd
  :: IO Int
  -- ^ Timeout in seconds
  -> String
  -> FilePath
  -> Maybe [(String, String)]
  -> (Maybe Handle
     -- ^ File handle for sending to the stdin for the process
   -> ProcMsg
     -- ^ A message the process printed to stdout or stderr
   -> IO ())
  -> ExceptT ExitCode IO ExitCode
runCmd calcDelay cmd dir e action = do
  start <- liftIO getCurrentTime
  let cp = (shell cmd)
        { cwd = Just dir
        , env = e
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  liftIO $ action Nothing $ ProcMsg start BuildCommandMsg $ T.pack cmd
  exitCode <- liftIO $ withCreateProcess_ "runCmd" cp (collectOutput calcDelay action)
  case exitCode of
    ExitFailure _ -> hoistEither $ Left exitCode
    ExitSuccess -> do
      t <- liftIO $ getCurrentTime
      liftIO $ printf "ExitSuccess in %.2f seconds" (realToFrac (diffUTCTime t start) :: Double)
      hoistEither $ Right exitCode

data ThreadType = TimerThread | ProcessThread

catchEOF :: C.SomeException -> IO ()
catchEOF _ = return ()

collectOutput
  :: (IO Int)
  -- ^ Function that calculates the number of microseconds to delay
  -> (Maybe Handle -> ProcMsg -> IO ())
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> ProcessHandle
  -> IO ExitCode
collectOutput calcDelay action (Just hIn) (Just hOut) (Just hErr) ph = do
    msgQueue <- atomically newTQueue
    let watchForOutput src h = do
          hSetBuffering h LineBuffering
          _ <- C.catch (void $ hWaitForInput h (-1)) catchEOF
          t <- getCurrentTime
          msg <- C.handle (\e -> catchEOF e >> return "") $ hGetLine h
          when (not $ null msg) $ atomically $ writeTQueue msgQueue $ ProcMsg t src (T.pack msg)
          watchForOutput src h
        handleOutput = do
          procMsg <- atomically (readTQueue msgQueue)
          action (Just hIn) procMsg
          handleOutput
    otid <- forkIO $ watchForOutput StdoutMsg hOut
    etid <- forkIO $ watchForOutput StderrMsg hErr
    atid <- forkIO handleOutput

    ecMVar <- newEmptyMVar
    ttid <- forkIO $ do
      d <- calcDelay
      threadDelay d
      putMVar ecMVar Nothing
    ptid <- forkIO $ do
      ec <- waitForProcess ph
      putMVar ecMVar $ Just ec

    mec <- takeMVar ecMVar
    ec <- case mec of
      Nothing -> do
        -- Timeout exceeded
        killThread ptid
        terminateProcess ph
        return $ ExitFailure 99
      Just ec -> do
        -- Process finished
        killThread ttid
        return ec
    threadDelay 1000000

    killThread otid
    killThread etid
    killThread atid
    return ec
collectOutput _ _ _ _ _ _ = do
  putStrLn "WARN: collectOutput file descriptors are wonky!"
  return $ ExitFailure 101


getCommand :: CreateProcess -> String
getCommand cp =
    case cmdspec cp of
      ShellCommand cmd -> cmd
      RawCommand cmd args -> unwords (cmd : args)
