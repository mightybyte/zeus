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
import           Control.Error
import qualified Control.Exception as C
import           Control.Lens
import           Control.Monad
import qualified Data.ByteString.Char8 as CB
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.RNG
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Mem.Weak
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.DbLib
import           Backend.ExecutablePaths
import           Backend.Logger
import           Backend.Process
import           Backend.Types.ServerEnv
import           Backend.WsCmds
import           Common.Api
import           Common.Types.ConnectedAccount
import           Backend.Types.ConnRepo
import           Common.Types.BinaryCache
import           Common.Types.BuildJob
import           Common.Types.Builder
import           Common.Types.CacheJob
import           Common.Types.CiSettings
import           Common.Types.JobStatus
import           Common.Types.Platform
import           Common.Types.ProcMsg
import           Common.Types.Repo
import           Common.Types.RepoBuildInfo
import           Common.Types.ZeusMsg
------------------------------------------------------------------------------

getNextJob :: ServerEnv -> Maybe Platform -> IO (Maybe BuildJob)
getNextJob se Nothing = do
  beamQueryConn (_serverEnv_db se) $
    runSelectReturningOne $
    select $
    orderBy_ (asc_ . _buildJob_id) $ limit_ 1 $ do
      job <- all_ (_ciDb_buildJobs ciDb)
      guard_ ( _buildJob_status job ==. val_ JobPending )
      return job
getNextJob se (Just plat) = do
  beamQueryConn (_serverEnv_db se) $
    runSelectReturningOne $
    select $
    orderBy_ (asc_ . _buildJob_id) $ limit_ 1 $ do
      job <- all_ (_ciDb_buildJobs ciDb)
      guard_ ( _buildJob_status job ==. val_ JobPending &&.
               _buildJob_platform job ==. val_ plat
             )
      return job

setJobStatus :: Connection -> UTCTime -> JobStatus -> BuildJob -> IO ()
setJobStatus dbConn t status incomingJob = do
  runBeamSqlite dbConn $ do
    runUpdate $
      update (_ciDb_buildJobs ciDb)
             (\job -> case status of
                JobPending -> mconcat
                  [ job ^. buildJob_receivedAt <-. val_ t
                  , job ^. buildJob_status <-. val_ status
                  ]
                JobInProgress -> mconcat
                  [ job ^. buildJob_startedAt <-. val_ (Just t)
                  , job ^. buildJob_status <-. val_ status
                  ]
                _ -> mconcat
                  [ job ^. buildJob_endedAt <-. val_ (Just t)
                  , job ^. buildJob_status <-. val_ status
                  ])
             (\job -> _buildJob_id job ==. val_ (_buildJob_id incomingJob))
    return ()

getBid :: Maybe Builder -> MBuilderId
getBid Nothing = BuilderId Nothing
getBid (Just b) = BuilderId (Just $ _builder_id b)

-- Each remote builder gets its own thread.  Nothing means that there are no
-- remote builders and the build is running on the Zeus master node.
buildManagerThread :: ServerEnv -> Maybe Builder -> IO ()
buildManagerThread se mbuilder = do
  let plat = _builder_platform <$> mbuilder
  let dbConn = _serverEnv_db se
  rng <- mkRNG
  forever $ do
    mjob <- getNextJob se plat
    case mjob of
      Nothing -> threadDelay 5000000
      Just job -> do
        let rbi = _buildJob_repoBuildInfo job
        case _rbi_repoEvent rbi of
          RepoPullRequest -> logStr dbConn Info (getBid mbuilder) $
            printf "Ignoring pull request message on %s/%s commit %s\n"
                   (_rbi_repoNamespace rbi) (_rbi_repoName rbi) (_rbi_commitHash rbi)
          RepoPush -> do
            logStr dbConn Info (getBid mbuilder) $ printf "Got push message on %s/%s\n"
                   (_rbi_repoNamespace rbi) (_rbi_repoName rbi)
            logStr dbConn Info (getBid mbuilder) $ printf "Pushed commit %s to %s\n"
                   (_rbi_commitHash rbi) (_rbi_gitRef rbi)
            ras <- runBeamSqlite dbConn $
              runSelectReturningList $ select $ do
                account <- all_ (_ciDb_connectedAccounts ciDb)
                repo <- all_ (_ciDb_repos ciDb)
                guard_ (_repo_name repo ==. val_ (_rbi_repoName rbi))
                guard_ (account ^. connectedAccount_id ==. repo ^. repo_accessAccount)
                return (repo, account)
            case ras of
              [] -> do
                logStr dbConn Warn (getBid mbuilder) $ "Warning: Got a webhook for a repo that is not in our DB.  Is the DB corrupted?"
                beamQueryConn (_serverEnv_db se) $
                  runDelete $ delete (_ciDb_buildJobs ciDb)
                    (\j -> j ^. buildJob_id ==. val_ (_buildJob_id job))
                return ()
              [(r,a)] -> runBuild se mbuilder rng r a job
              _ -> logStr dbConn Warn (getBid mbuilder) $ printf "Warning: Got more repos than expected.  Why isn't %s/%s unique?\n"
                          (_rbi_repoNamespace rbi) (_rbi_repoName rbi)

runBuild
  :: ServerEnv
  -> Maybe Builder
  -> RNG
  -> Repo
  -> ConnectedAccount
  -> BuildJob
  -> IO ()
runBuild se mbuilder rng repo ca incomingJob = do
  let dbConn = _serverEnv_db se
      connRepo = _serverEnv_connRepo se
  start <- getCurrentTime
  setJobStatus dbConn start JobInProgress incomingJob
  broadcastJobs dbConn connRepo

  ecMVar <- newEmptyMVar
  wtid <- mkWeakThreadId =<< forkIO (buildThread se mbuilder ecMVar rng repo ca incomingJob)
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
                  appendFile (buildLogFilePath jobId) . prettyProcMsg
                    =<< textProcMsg "Build timed out"
                  return JobTimedOut
            else do

              threadDelay 5000000 >> go
        Just ec -> return $ exitCodeToStatus ec

appendToJobLog :: Int -> String -> IO ()
appendToJobLog jid msg = do
  let outputFile = printf "%s/%d.txt" buildOutputDir jid
  appendFile outputFile msg

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

buildLogFilePath :: Int -> FilePath
buildLogFilePath jid = printf "%s/%d.txt" buildOutputDir jid

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

isStorePath :: Text -> Bool
isStorePath t = T.isPrefixOf "/nix/store" t && not (T.isInfixOf " " t)

addCacheJob :: ServerEnv -> PrimaryKey BinaryCacheT (Nullable Identity) -> Text -> IO ()
addCacheJob se cacheId sp = do
  T.putStrLn $ "Adding job to cache " <> sp
  beamQuery se $ do
    runInsert $ insert (_ciDb_cacheJobs ciDb) $ insertExpressions
      [CacheJob default_ (val_ sp) (val_ cacheId) (val_ Nothing) (val_ Nothing) (val_ JobPending)]

buildThread
  :: ServerEnv
  -> Maybe Builder
  -> MVar ExitCode
  -> RNG
  -> Repo
  -> ConnectedAccount
  -> BuildJob
  -> IO ()
buildThread se mbuilder ecMVar rng repo ca job = do
  start <- getCurrentTime
  let dbConn = _serverEnv_db se
  let rbi = _buildJob_repoBuildInfo job
  let jid = _buildJob_id job
  tok <- randomToken 8 rng
  let user = _connectedAccount_name ca
  let pass = _connectedAccount_accessToken ca
  let url = addCloneCreds user pass (_rbi_cloneUrlHttp rbi)
  let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S" start
  let buildId = printf "build-%s-%s" timeStr (toS tok :: String) :: String
  let cloneDir = printf "/tmp/zeus-builds/%s" buildId :: String
  let repoDir = cloneDir </> toS (_repo_name repo)
  createDirectoryIfMissing True cloneDir
  createDirectoryIfMissing True buildOutputDir
  let outputFile = buildLogFilePath jid
  logStr dbConn Info (getBid mbuilder) $ printf "Writing build output to %s\n" outputFile

  e <- liftIO getEnvironment
  Just cs <- liftIO $ getCiSettings dbConn
  let buildEnv = M.toList $ M.insert "NIX_PATH" (toS $ _ciSettings_nixPath cs) $ M.fromList e

  withLogHandle outputFile $ \lh  -> do
    let cloneCmd = printf "%s clone --recurse-submodules %s" gitBinary url
        saveAndSendStr msgTy msg = do
          !t <- getCurrentTime
          let pm = ProcMsg t msgTy msg
          saveAndSend pm
        saveAndSend pm = do
          hPutStrLn lh $! prettyProcMsg pm
          sendOutput se jid pm
        cachingSaveAndSend pm = do
          let msg = _procMsg_msg pm
          if isStorePath msg && _procMsg_source pm == StdoutMsg
            then addCacheJob se (_repo_cache repo) msg
            else return ()
          hPutStrLn lh $! prettyProcMsg pm
          sendOutput se jid pm

    res <- runExceptT $ do
      liftIO $ saveAndSendStr CiMsg $ T.pack $ printf "Cloning %s/%s to %s"
        (_repo_namespace repo) (_repo_name repo) cloneDir
      _ <- runCmd2 cloneCmd cloneDir Nothing saveAndSend
      let checkout = printf "%s checkout %s" gitBinary (_rbi_commitHash rbi)
      liftIO $ saveAndSendStr CiMsg (toS checkout)
      _ <- runCmd2 checkout repoDir Nothing saveAndSend
      liftIO $ saveAndSendStr CiMsg $ "Building with the following environment:"
      liftIO $ saveAndSendStr CiMsg $ T.pack $ show buildEnv

    let buildSingle mattr = runExceptT $ do
          liftIO $ saveAndSendStr CiMsg $ "Building subjob for attribute " <> T.pack (show mattr)
          let instantiateArgs =
                T.unpack (_repo_buildNixFile repo) :
                maybe [] (\a -> ["-A", T.unpack a]) mattr
          runProc nixInstantiate instantiateArgs repoDir (Just buildEnv) cachingSaveAndSend

          let buildArgs = instantiateArgs ++
                [ "--show-trace"
                , "--option"
                , "build-keep-log"
                , "true"
                , "--keep-going"
                ]
          runProc nixBuildBinary buildArgs repoDir (Just buildEnv) saveAndSend
          end <- liftIO getCurrentTime
          let finishMsg = printf "Build finished in %.3f seconds" (realToFrac (diffUTCTime end start) :: Double)
          liftIO $ saveAndSendStr CiMsg $ T.pack finishMsg
          liftIO $ putStrLn finishMsg

          msp <- liftIO $ getSymlinkTarget (repoDir </> "result")
          case msp of
            Nothing -> liftIO $ logStr dbConn Warn (getBid mbuilder) "No build outputs to cache"
            Just sp -> do
              liftIO $ addCacheJob se (_repo_cache repo) $ T.pack sp
              liftIO $ logStr dbConn Info (getBid mbuilder) $ printf "Subjob '%s' succeeded with result storepath %s\n" (show mattr) (show sp)

    case res of
      Left ec -> do
        liftIO $ logStr dbConn Error (getBid mbuilder) $ printf "Build failed with code %s\n" (show ec)
        putMVar ecMVar ec
      Right _ -> do
        let attrs = unAttrList $ _repo_attributesToBuild repo
        liftIO $ saveAndSendStr CiMsg $ "Running build for each of the following attributes: " <> (T.pack $ show attrs)
        results <- case attrs of
          [] -> (:[]) <$> buildSingle Nothing
          _ -> forM attrs $ \attr -> buildSingle (Just attr)
        let bad = lefts results
        if null bad
          then putMVar ecMVar ExitSuccess
          else do
            liftIO $ saveAndSendStr CiMsg $ "Some jobs failed"
            mapM_ (liftIO . saveAndSendStr CiMsg . T.pack . show) $ zip attrs results
            putMVar ecMVar (ExitFailure (length bad))


getSymlinkTarget :: FilePath -> IO (Maybe FilePath)
getSymlinkTarget nm = do
    exists <- doesPathExist nm
    if exists
      then do
        isLink <- pathIsSymbolicLink nm
        if isLink
          then Just <$> getSymbolicLinkTarget nm
          else return Nothing
      else return Nothing

data GitMsgType = UsernameMessage String | PasswordMessage String | OtherMessage

toBS :: String -> CB.ByteString
toBS = toS

data ThreadType = TimerThread | ProcessThread

-- Thread-safe console logging function
clog :: String -> IO ()
clog = CB.putStrLn . toS

catchEOF :: String -> C.SomeException -> IO ()
catchEOF msg e = clog $ printf "Caught exception in %s: %s" msg (show e)

