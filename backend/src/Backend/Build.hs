{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Build where

------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Data.RNG
import           Data.String.Conv
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.Db
import           Common.Types.ConnectedAccount
import           Common.Types.BuildJob
import           Common.Types.BuildMsg
import           Common.Types.Repo
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

buildThread :: Connection -> TQueue BuildMsg -> IO ()
buildThread conn buildQueue = do
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
        repos <- runBeamSqlite conn $
          runSelectReturningList $ select $ do
            account <- all_ (_ciDb_connectedAccounts ciDb)
            repo <- all_ (_ciDb_repos ciDb)
            guard_ (_repo_fullName repo ==. val_ (_rbi_repoFullName rbi))
            guard_ (account ^. connectedAccount_id ==. repo ^. repo_owner)
            return repo
        case repos of
          [] -> putStrLn "Warning: Got a webhook for a repo that is not in our DB.  Is the DB corrupted?"
          [r] -> runBuild conn rng r msg
          _ -> printf "Warning: Got more repos than expected.  Why isn't %s unique?\n" (_rbi_repoFullName rbi)

runBuild
  :: Connection
  -> RNG
  -> Repo
  -> BuildMsg
  -> IO ()
runBuild conn rng r msg = do
  let rbi = _buildMsg_repoBuildInfo msg
  start <- getCurrentTime
  runBeamSqliteDebug putStrLn conn $ do
    runUpdate $
      update (_ciDb_buildJobs ciDb)
             (\job -> mconcat
                        [ job ^. buildJob_startedAt <-. val_ (Just start)
                        , job ^. buildJob_status <-. val_ JobInProgress ])
             (\job -> _buildJob_id job ==. val_ (_buildMsg_jobId msg))

    return ()
  tok <- randomToken 8 rng
  let url = case _repo_cloneMethod r of
              SshClone -> _rbi_cloneUrlSsh rbi
              HttpClone -> _rbi_cloneUrlHttp rbi
  let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S" start
  let buildId = printf "build-%s-%s" timeStr (toS tok :: String) :: String
  let cloneDir = printf "/tmp/simple-ci-builds/%s" buildId :: String
  createDirectoryIfMissing True cloneDir
  let outputDir = "log/builds"
  createDirectoryIfMissing True outputDir
  let outputFile = printf "%s/%s.output" outputDir buildId
  printf "Writing build output to %s\n" outputFile
  withLogHandle outputFile $ \lh  -> do
    let cloneCmd = printf "git clone %s" url
    logWithTimestamp lh cloneCmd

    logWithTimestamp lh $ printf "Cloning %s to %s" (_repo_fullName r) cloneDir
    _ <- runInDirWithEnv lh cloneCmd cloneDir Nothing
    let repoDir = cloneDir </> toS (_repo_name r)
    let checkout = printf "git checkout %s" (_rbi_commitHash rbi)
    _ <- runInDirWithEnv lh checkout repoDir Nothing
    exitCode <- runInDirWithEnv lh (toS $ _repo_buildCmd r) repoDir Nothing
    end <- getCurrentTime
    let finishMsg = printf "Build finished in %.3f seconds" (realToFrac (diffUTCTime end start) :: Double)
    logWithTimestamp lh (finishMsg ++ "\n")
    runBeamSqliteDebug putStrLn conn $ do
      runUpdate $
        update (_ciDb_buildJobs ciDb)
               (\job -> mconcat
                          [ job ^. buildJob_endedAt <-. val_ (Just end)
                          , job ^. buildJob_status <-. val_ (exitCodeToStatus exitCode) ])
               (\job -> _buildJob_id job ==. val_ (_buildMsg_jobId msg))

    putStrLn finishMsg
  return ()

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
  -- TODO Properly handle stdout and stderr
  (_, _, _, ph) <- createProcess_ cmd cp
  -- TODO Terminate process if it runs too long
  exitCode <- waitForProcess ph
  hPutStrLn lh $ "Exited with: " ++ show exitCode
  return exitCode

getCommand :: CreateProcess -> String
getCommand cp =
    case cmdspec cp of
      ShellCommand cmd -> cmd
      RawCommand cmd args -> unwords (cmd : args)
