{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Error
import qualified Control.Exception as E
import           Control.Lens
import qualified Data.Map as M
import           Control.Monad
import           Control.Monad.Trans
import           Data.Dependent.Sum (DSum ((:=>)))
import           Data.IORef
import           Data.RNG
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Migrate
import           Database.Beam.Migrate.Simple
import           Database.SQLite.Simple
import           GitHub.Auth
import qualified Network.WebSockets as WS
import           Obelisk.Backend
import           Obelisk.Route
import           Scrub
import           Snap.Core
import           System.Directory
import           System.Mem.Weak
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.Build
import           Backend.Db
import           Backend.Github
import           Backend.Types.ConnRepo
import           Backend.Types.ServerEnv
import           Backend.WsCmds
import           Backend.WsUtils
import           Common.Api
import           Common.Route
import           Common.Types.BuildJob
import           Common.Types.BuildMsg
import           Common.Types.ConnectedAccount
import           Common.Types.JobStatus
import           Common.Types.Repo
------------------------------------------------------------------------------

getSecretToken :: IO Text
getSecretToken = do
  let secretFile = "zeus-access-token"
  secretExists <- doesFileExist secretFile
  if secretExists
    then T.strip <$> T.readFile secretFile
    else do
      rng <- mkRNG
      tok <- toS <$> randomToken 32 rng
      T.writeFile secretFile tok
      return tok

dbConnectInfo :: String
dbConnectInfo = "zeus.db"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- TODO Probably switch to a connection pool a some point, but we don't
      -- expect a large volume of requests for awhile so this is probably a
      -- very low priority.
      dbConn <- open dbConnectInfo
      runBeamSqlite dbConn $ autoMigrate migrationBackend ciDbChecked
      appRoute <- getAppRoute
      secretToken <- getSecretToken
      buildQueue <- atomically newTQueue
      connRepo <- newConnRepo
      buildThreads <- newIORef mempty
      let env = ServerEnv appRoute secretToken dbConn buildQueue connRepo buildThreads
      _ <- forkIO $ buildManagerThread env
      serve $ serveBackendRoute env
  , _backend_routeEncoder = backendRouteEncoder
  }

-- | Serve our dynconfigs file.
serveBackendRoute :: ServerEnv -> R BackendRoute -> Snap ()
serveBackendRoute env = \case
  BackendRoute_GithubHook :=> _ -> hookHandler env
  BackendRoute_Ping :=> _ -> writeText "PONG\nPONG\nPONG\n"
  BackendRoute_Websocket :=> _ -> wsHandler $ \conn -> do
      cid <- addConnection conn (_serverEnv_connRepo env)
      talkClient env cid conn
  BackendRoute_Missing :=> _ -> do
    liftIO $ putStrLn "Unknown backend route"
    writeText "Unknown backend route"

talkClient :: ServerEnv -> ConnId -> WS.Connection -> IO ()
talkClient env cid conn = do
    E.handle cleanup $ forever $ do
      clientCmd <- wsReceive conn
      case clientCmd of
        Left e -> do
          putStrLn $ "************************************************"
          putStrLn $ "ERROR: websocketHandler couldn't decode message:"
          putStrLn e
        Right Up_ListAccounts -> listAccounts env conn
        Right (Up_ConnectAccount cas) -> mapM_ (connectAccount env conn) cas
        Right (Up_DelAccounts cas) -> delAccounts env conn cas
        Right Up_ListRepos -> listRepos env conn
        Right (Up_AddRepo rs) -> mapM_ (addRepo env conn) rs
        Right Up_GetJobs -> do
          let dbConn = _serverEnv_db env
          jobs <- getJobsFromDb dbConn
          wsSend conn $ Down_Jobs jobs
        Right (Up_CancelJobs jids) -> do
          mapM_ (cancelJobAndRemove env) jids
        Right (Up_RerunJobs jids) -> do
          mapM_ (rerunJob env) jids
  where
    cRepo = _serverEnv_connRepo env
    cleanup :: E.SomeException -> IO ()
    cleanup _ = do
      removeConnection cid cRepo

rerunJob :: ServerEnv -> BuildJobId -> IO ()
rerunJob env (BuildJobId jid) = do
    let dbConn = _serverEnv_db env
    mjob <- beamQueryConn (_serverEnv_db env) $
      runSelectReturningOne $ select $ do
        job <- all_ (_ciDb_buildJobs ciDb)
        guard_ (job ^. buildJob_id ==. (val_ jid))
        return job
    case mjob of
      Nothing -> printf "Job ID %d does not exist\n" jid
      Just job -> do
        printf "Re-running job %d\n" jid
        void $ atomically $ writeTQueue (_serverEnv_buildQueue env) $
          BuildMsg (_buildJob_id job) (_buildJob_repoBuildInfo job)

cancelJobAndRemove :: ServerEnv -> BuildJobId -> IO ()
cancelJobAndRemove env (BuildJobId jid) = do
    mwtid <- atomicModifyIORef (_serverEnv_buildThreads env) $ \m ->
      let (v,m2) = M.updateLookupWithKey (\_ _ -> Nothing) jid m
       in (m2,v)
    -- This could be golfed, but probably not worth it
    case mwtid of
      Nothing -> updateJobStatus env jid JobVanished
      Just wtid -> do
        mtid <- deRefWeak wtid
        case mtid of
          Nothing ->
            updateJobStatus env jid JobVanished
          Just tid -> do
            killThread tid
            updateJobStatus env jid JobCanceled
    broadcastJobs (_serverEnv_db env) (_serverEnv_connRepo env)

updateJobStatus :: ServerEnv -> Int -> JobStatus -> IO ()
updateJobStatus env jid status =
    runBeamSqliteDebug putStrLn (_serverEnv_db env) $ do
      runUpdate $
        update (_ciDb_buildJobs ciDb)
               (\job -> job ^. buildJob_status <-. val_ status)
               (\job -> _buildJob_id job ==. val_ jid)
      return ()

connectAccount
  :: ServerEnv
  -> WS.Connection
  -> ConnectedAccountT Maybe
  -> IO ()
connectAccount env wsConn (ConnectedAccount _ n a pr) = do
  beamQuery env $ do
    runInsert $ insert (_ciDb_connectedAccounts ciDb) $ insertExpressions
           $ maybeToList $ ConnectedAccount default_
              <$> (val_ <$> n)
              <*> (val_ <$> a)
              <*> (val_ <$> pr)
  as <- beamQuery env $ do
    runSelectReturningList $ select $ all_ (_ciDb_connectedAccounts ciDb)
  wsSend wsConn $ Down_ConnectedAccounts $ map (getScrubbed . scrub) as

listAccounts :: ServerEnv -> WS.Connection -> IO ()
listAccounts env wsConn = do
  accounts <- beamQuery env $
    runSelectReturningList $ select $ do
      all_ (_ciDb_connectedAccounts ciDb)
  wsSend wsConn $ Down_ConnectedAccounts $ map (getScrubbed . scrub) accounts

listRepos :: ServerEnv -> WS.Connection -> IO ()
listRepos env wsConn = do
  accounts <- beamQuery env $
    runSelectReturningList $ select $ do
      all_ (_ciDb_repos ciDb)
  wsSend wsConn $ Down_Repos accounts

addRepo
  :: ServerEnv
  -> WS.Connection
  -> RepoT Maybe
  -> IO ()
addRepo env wsConn (Repo _ (Just fn) (ConnectedAccountId (Just o)) (Just n) (Just c) (Just b) (Just t)) = do
  mca <- beamQuery env $ do
    runSelectReturningOne $ select $ do
      account <- all_ (ciDb ^. ciDb_connectedAccounts)
      guard_ (account ^. connectedAccount_id ==. (val_ o))
      return account
  case mca of
    Nothing -> return ()
    Just ca -> do
      putStrLn $ "Setting up new webhook for " <> show ca
      erw <- setupWebhook
        (toS $ _serverEnv_publicUrl env)
        (OAuth $ toS $ _connectedAccount_accessToken ca)
        (_connectedAccount_name ca) n (_serverEnv_secretToken env) AllowInsecure
      case erw of
        Left e -> wsSend wsConn $ Down_Alert $ "Error setting up webhook: " <> (T.pack $ show e)
        Right _ -> do
          putStrLn "Repository hook setup successful"
          beamQuery env $ do
            runInsert $ insert (_ciDb_repos ciDb) $ insertExpressions
              [Repo default_
                    (val_ fn)
                    (ConnectedAccountId $ val_ o)
                    (val_ n)
                    (val_ c)
                    (val_ b)
                    (val_ t)
              ]
          as <- beamQuery env $ do
            runSelectReturningList $ select $ all_ (_ciDb_repos ciDb)
          wsSend wsConn $ Down_Repos as
addRepo _ _ _ = putStrLn "AddRepo got bad argument"

delAccounts :: ServerEnv -> WS.Connection -> [ConnectedAccountId] -> IO ()
delAccounts env wsConn cas = do
  beamQuery env $
    runDelete $ delete (_ciDb_connectedAccounts ciDb) $
        (\ca -> ca ^. connectedAccount_id `in_` map (val_ . caKeyToInt) cas)
  listAccounts env wsConn
