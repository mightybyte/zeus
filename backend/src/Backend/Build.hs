{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Build where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Error
import qualified Control.Exception as C
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as CB
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.RNG
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import qualified Turtle as Turtle
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.ExecutablePaths
import           Backend.Types.ServerEnv
import           Backend.WsCmds
import           Common.Api
import           Common.Types.ConnectedAccount
import           Backend.Types.ConnRepo
import           Common.Types.BuildJob
import           Common.Types.CiSettings
import           Common.Types.JobStatus
import           Common.Types.ProcMsg
import           Common.Types.Repo
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

getNextJob :: ServerEnv -> IO (Maybe BuildJob)
getNextJob se = do
  beamQueryConn (_serverEnv_db se) $
    runSelectReturningOne $
    select $
    orderBy_ (asc_ . _buildJob_id) $ do
      job <- all_ (_ciDb_buildJobs ciDb)
      guard_ (job ^. buildJob_status ==. (val_ JobPending))
      return job


buildManagerThread :: ServerEnv -> IO ()
buildManagerThread se = do
  let dbConn = _serverEnv_db se
  rng <- mkRNG
  forever $ do
    mjob <- getNextJob se
    case mjob of
      Nothing -> threadDelay 5000000
      Just job -> do
        let rbi = _buildJob_repoBuildInfo job
        case _rbi_repoEvent rbi of
          RepoPullRequest ->
            printf "Ignoring pull request message on %s/%s commit %s\n"
                   (_rbi_repoNamespace rbi) (_rbi_repoName rbi) (_rbi_commitHash rbi)
          RepoPush -> do
            printf "Got push message on %s/%s\n"
                   (_rbi_repoNamespace rbi) (_rbi_repoName rbi)
            printf "Pushed commit %s to %s\n"
                   (_rbi_commitHash rbi) (_rbi_gitRef rbi)
            ras <- runBeamSqlite dbConn $
              runSelectReturningList $ select $ do
                account <- all_ (_ciDb_connectedAccounts ciDb)
                repo <- all_ (_ciDb_repos ciDb)
                guard_ (_repo_name repo ==. val_ (_rbi_repoName rbi))
                guard_ (account ^. connectedAccount_id ==. repo ^. repo_accessAccount)
                return (repo, account)
            case ras of
              [] -> putStrLn "Warning: Got a webhook for a repo that is not in our DB.  Is the DB corrupted?"
              [(r,a)] -> runBuild se rng r a job
              _ -> printf "Warning: Got more repos than expected.  Why isn't %s/%s unique?\n"
                          (_rbi_repoNamespace rbi) (_rbi_repoName rbi)

runBuild
  :: ServerEnv
  -> RNG
  -> Repo
  -> ConnectedAccount
  -> BuildJob
  -> IO ()
runBuild se rng repo ca incomingJob = do
  let dbConn = _serverEnv_db se
      connRepo = _serverEnv_connRepo se
  start <- getCurrentTime
  runBeamSqlite dbConn $ do
    runUpdate $
      update (_ciDb_buildJobs ciDb)
             (\job -> mconcat
                        [ job ^. buildJob_startedAt <-. val_ (Just start)
                        , job ^. buildJob_status <-. val_ JobInProgress ])
             (\job -> _buildJob_id job ==. val_ (_buildJob_id incomingJob))
    return ()
  broadcastJobs dbConn connRepo

  ecMVar <- newEmptyMVar
  wtid <- mkWeakThreadId =<< forkIO (buildThread se ecMVar rng repo ca incomingJob)
  let jobId = fromIntegral $ _buildJob_id incomingJob
  atomicModifyIORef (_serverEnv_buildThreads se) $ \m ->
    (M.insert jobId wtid m, ())
  jobStatus <- threadWatcher
    (_serverEnv_buildThreads se)
    start
    (fromIntegral $ _repo_timeout repo)
    ecMVar wtid jobId

  end <- getCurrentTime
  runBeamSqlite dbConn $ do
    runUpdate $
      update (_ciDb_buildJobs ciDb)
             (\job -> mconcat
                        [ job ^. buildJob_endedAt <-. val_ (Just end)
                        , job ^. buildJob_status <-. val_ jobStatus ])
             (\job -> _buildJob_id job ==. val_ (_buildJob_id incomingJob))

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

addCloneCreds :: Text -> Text -> Text -> Text
addCloneCreds user pass url =
  T.replace "://" ("://" <> user <> ":" <> pass <> "@") url

buildThread
  :: ServerEnv
  -> MVar ExitCode
  -> RNG
  -> Repo
  -> ConnectedAccount
  -> BuildJob
  -> IO ()
buildThread se ecMVar rng repo ca job = do
  start <- getCurrentTime
  let rbi = _buildJob_repoBuildInfo job
  let jid = _buildJob_id job
  tok <- randomToken 8 rng
  let user = _connectedAccount_name ca
  let pass = _connectedAccount_accessToken ca
  let url = addCloneCreds user pass (_rbi_cloneUrlHttp rbi)
  let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S" start
  let buildId = printf "build-%s-%s" timeStr (toS tok :: String) :: String
  let cloneDir = printf "/tmp/zeus-builds/%s" buildId :: String
  createDirectoryIfMissing True cloneDir
  createDirectoryIfMissing True buildOutputDir
  let outputFile = printf "%s/%d.txt" buildOutputDir jid
  printf "Writing build output to %s\n" outputFile
  withLogHandle outputFile $ \lh  -> do
    let cloneCmd = printf "%s clone %s" gitBinary url
        saveAndSendStr msgTy msg = do
          !t <- getCurrentTime
          let pm = ProcMsg t msgTy msg
          saveAndSend pm
        saveAndSend pm = do
          hPutStrLn lh $! prettyProcMsg pm
          sendOutput se jid pm

    res <- runExceptT $ do
      liftIO $ saveAndSendStr CiMsg $ T.pack $ printf "Cloning %s/%s to %s"
        (_repo_namespace repo) (_repo_name repo) cloneDir
      _ <- runCmd2 cloneCmd cloneDir Nothing saveAndSend
      let repoDir = cloneDir </> toS (_repo_name repo)
      let checkout = printf "%s checkout %s" gitBinary (_rbi_commitHash rbi)
      liftIO $ saveAndSendStr CiMsg (toS checkout)
      _ <- runCmd2 checkout repoDir Nothing saveAndSend
      let buildCmd = printf "%s --show-trace %s" nixBuildBinary (_repo_buildNixFile repo)
      e <- liftIO getEnvironment
      cs <- liftIO $ readIORef (_serverEnv_ciSettings se)
      let buildEnv = M.toList $ M.insert "NIX_PATH" (toS $ _ciSettings_nixPath cs) $ M.fromList e
      liftIO $ saveAndSendStr CiMsg $ "Building with the following environment:"
      liftIO $ saveAndSendStr CiMsg $ T.pack $ show buildEnv
      let buildArgs =
            [ T.unpack (_repo_buildNixFile repo)
            , "--show-trace"
            , "--option"
            , "build-keep-log"
            , "true"
            , "--keep-going"
            ]
      liftIO $ saveAndSendStr CiMsg (T.pack $ unwords (buildCmd : buildArgs))
      _ <- runProc nixBuildBinary buildArgs repoDir (Just buildEnv) saveAndSend
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

toBS :: String -> CB.ByteString
toBS = toS

runCmd2
  :: String
  -> FilePath
  -> Maybe [(String, String)]
  -> (ProcMsg -> IO ())
  -> ExceptT ExitCode IO ExitCode
runCmd2 cmd dir envVars action = do
  let cp = (shell cmd)
        { cwd = Just dir
        , env = envVars
        }
  runCP cp action

runProc
  :: String
  -> [String]
  -> FilePath
  -> Maybe [(String, String)]
  -> (ProcMsg -> IO ())
  -> ExceptT ExitCode IO ExitCode
runProc cmd args dir envVars action = do
  let cp = (proc cmd args)
        { cwd = Just dir
        , env = envVars
        }
  runCP cp action

runCP
  :: CreateProcess
  -> (ProcMsg -> IO ())
  -> ExceptT ExitCode IO ExitCode
runCP cp action = do
  res <- liftIO $ C.try
    (Turtle.foldShell (Turtle.streamWithErr cp (return mempty)) (shellHandler action))
  case res of
    Left e -> ExceptT $ return $ Left e
    Right _ -> return ExitSuccess

shellHandler
  :: (ProcMsg -> IO ())
  -> Turtle.FoldShell (Either Turtle.Line Turtle.Line) ()
shellHandler action = Turtle.FoldShell step () return
  where
    step _ a = do
      t <- getCurrentTime
      let pm = case a of
                Left m -> ProcMsg t StderrMsg (Turtle.lineToText m)
                Right m -> ProcMsg t StdoutMsg (Turtle.lineToText m)
      action pm

runCmd
  :: Int
  -- ^ Job ID (for debugging)
  -> IO Int
  -- ^ Timeout in seconds
  -> String
  -> FilePath
  -> Maybe [(String, String)]
  -> (Maybe Handle -- File handle for sending to the stdin for the process
      -> ProcMsg -- message the process printed to stdout or stderr
      -> IO ())
  -> ExceptT ExitCode IO ExitCode
runCmd jid calcDelay cmd dir e action = do
  start <- liftIO getCurrentTime
  let cp = (shell cmd)
        { cwd = Just dir
        , env = e
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  liftIO $ action Nothing $ ProcMsg start CiMsg $ "With env " <> T.pack (show e)
  liftIO $ action Nothing $ ProcMsg start BuildCommandMsg $ T.pack cmd
  exitCode <- liftIO $ withCreateProcess_ "runCmd" cp (collectOutput jid calcDelay action)
  case exitCode of
    ExitFailure _ -> hoistEither $ Left exitCode
    ExitSuccess -> do
      t <- liftIO $ getCurrentTime
      liftIO $ clog $ printf "ExitSuccess in %.2f seconds" (realToFrac (diffUTCTime t start) :: Double)
      hoistEither $ Right exitCode

data ThreadType = TimerThread | ProcessThread

-- Thread-safe console logging function
clog :: String -> IO ()
clog = CB.putStrLn . toS


catchEOF :: String -> C.SomeException -> IO ()
catchEOF msg e = clog $ printf "Caught exception in %s: %s" msg (show e)

collectOutput
  :: Int
  -> (IO Int)
  -- ^ Function that calculates the number of microseconds to delay
  -> (Maybe Handle -> ProcMsg -> IO ())
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> ProcessHandle
  -> IO ExitCode
collectOutput jid calcDelay action (Just hIn) (Just hOut) (Just hErr) ph = do
    msgQueue <- atomically newTQueue
    hSetBuffering hOut LineBuffering
    hSetBuffering hErr LineBuffering
    let watchForOutput src h = do
          t <- getCurrentTime
          clog $ "calling hGetSome " <> show src
          msg <- B.hGetSome h 4096
          --msg <- B.hGetLine h
          --msg <- C.handle (\e -> (catchEOF $ "hGetLine " <> show src) e >> return "") $ B.hGetLine h
          clog $ printf "hGetSome %s returned %d bytes" (show src) (B.length msg)

          when (not $ B.null msg) $ atomically $ writeTQueue msgQueue $ ProcMsg t src (T.decodeUtf8 msg)
          watchForOutput src h
        handleOutput = do
          clog $ printf "Job %d handling output\n" jid
          procMsg <- atomically (readTQueue msgQueue)
          action (Just hIn) procMsg
          handleOutput
    otid <- forkIO $ watchForOutput StdoutMsg hOut
    etid <- forkIO $ watchForOutput StderrMsg hErr
    atid <- forkIO handleOutput

    ecMVar <- newEmptyMVar
    ttid <- forkIO $ do
      d <- calcDelay
      clog $ printf "Job %d delaying for %d microseconds\n" jid d
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
        threadDelay 1000000
        killThread ttid
        return ec

    killThread otid
    killThread etid
    killThread atid
    threadDelay 100000
    clog $ printf "Job %d finished and killed all child threads\n" jid
    return ec
collectOutput _ _ _ _ _ _ _ = do
  clog "WARN: collectOutput file descriptors are wonky!"
  return $ ExitFailure 101


getCommand :: CreateProcess -> String
getCommand cp =
    case cmdspec cp of
      ShellCommand cmd -> cmd
      RawCommand cmd args -> unwords (cmd : args)
