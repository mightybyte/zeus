{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Build where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception as C
import           Control.Lens
import           Control.Monad
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.RNG
import           Data.String.Conv
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
import           System.Directory
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
import           Common.Types.ConnectedAccount
import           Common.Types.BuildJob
import           Common.Types.BuildMsg
import           Common.Types.JobStatus
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
  wtid <- mkWeakThreadId =<< forkIO (buildThread ecMVar rng repo msg)
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

buildThread
  :: MVar ExitCode
  -> RNG
  -> Repo
  -> BuildMsg
  -> IO ()
buildThread ecMVar rng repo msg = do
  start <- getCurrentTime
  let rbi = _buildMsg_repoBuildInfo msg
  tok <- randomToken 8 rng
  let url = case _repo_cloneMethod repo of
              SshClone -> _rbi_cloneUrlSsh rbi
              HttpClone -> _rbi_cloneUrlHttp rbi
  let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S" start
  let buildId = printf "build-%s-%s" timeStr (toS tok :: String) :: String
  let cloneDir = printf "/tmp/zeus-builds/%s" buildId :: String
  createDirectoryIfMissing True cloneDir
  let outputDir = "log/builds"
  createDirectoryIfMissing True outputDir
  let outputFile = printf "%s/%s.output" outputDir buildId
  printf "Writing build output to %s\n" outputFile
  withLogHandle outputFile $ \lh  -> do
    let cloneCmd = printf "%s clone %s" gitBinary url
    logWithTimestamp lh cloneCmd

    logWithTimestamp lh $ printf "Cloning %s to %s" (_repo_fullName repo) cloneDir
    _ <- runInDirWithEnv lh cloneCmd cloneDir Nothing
    let repoDir = cloneDir </> toS (_repo_name repo)
    let checkout = printf "%s checkout %s" gitBinary (_rbi_commitHash rbi)
    _ <- runInDirWithEnv lh checkout repoDir Nothing
    exitCode <- runInDirWithEnv lh (toS $ _repo_buildCmd repo) repoDir Nothing
    end <- getCurrentTime
    let finishMsg = printf "Build finished in %.3f seconds with exit code %s" (realToFrac (diffUTCTime end start) :: Double) (show exitCode)
    logWithTimestamp lh (finishMsg ++ "\n")
    putStrLn finishMsg
    putMVar ecMVar exitCode

exitCodeToStatus :: ExitCode -> JobStatus
exitCodeToStatus ExitSuccess = JobSucceeded
exitCodeToStatus (ExitFailure _) = JobFailed

withLogHandle :: FilePath -> (Handle -> IO a) -> IO a
withLogHandle fp action = withFile fp AppendMode $ \h -> do
  hSetBuffering h NoBuffering
  action h

logWithTimestamp :: Handle -> String -> IO ()
logWithTimestamp lh msg = do
  !t <- getCurrentTime
  hPutStrLn lh $! "TIMESTAMP " <> show t <> ", " <> msg

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


runInDirWithEnv :: Handle -> String -> FilePath -> Maybe [(String, String)] -> IO ExitCode
runInDirWithEnv lh cmd dir e = do
  let cp = (shell cmd)
        { cwd = Just dir
        , env = e
        , std_out = UseHandle lh
        , std_err = UseHandle lh
        }
  hPutStrLn lh $ printf "Executing command from %s:" (show $ cwd cp)
  logWithTimestamp lh $ getCommand cp
  exitCode <- withCreateProcess_ "runInDirWithEnv" cp $ \_ _ _ ph -> do
    -- TODO Properly handle stdout and stderr
    ec <- waitForProcess ph
    -- TODO Terminate process if it runs too long
    return ec

  hPutStrLn lh $ "Exited with: " ++ show exitCode
  return exitCode

getCommand :: CreateProcess -> String
getCommand cp =
    case cmdspec cp of
      ShellCommand cmd -> cmd
      RawCommand cmd args -> unwords (cmd : args)
