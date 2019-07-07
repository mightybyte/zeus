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
import qualified Data.Aeson as A
import           Data.Dependent.Sum (DSum ((:=>)))
import           Data.IORef
import           Data.RNG
import qualified Data.Set as S
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Migrate
import           Database.Beam.Migrate.Simple
import           Database.SQLite.Simple
import           GitHub.Auth
import           GitHub.Data.Name
import           GitHub.Data.Id
import           GitHub.Data.Webhooks
import           GitHub.Endpoints.Repos.Webhooks
import qualified Network.WebSockets as WS
import           Obelisk.Backend
import qualified Obelisk.ExecutableConfig as ObConfig
import           Obelisk.Route
import           Scrub
import           Snap.Core
import           Snap.Util.FileServe
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Mem.Weak
import           System.Process (rawSystem)
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.Build
import           Backend.CacheServer
import           Backend.Common
import           Backend.Db
import           Backend.ExecutablePaths
import           Backend.Github
import           Backend.Gitlab
import           Backend.Types.BackendSettings
import           Backend.Types.ConnRepo
import           Backend.Types.NixCacheKeyPair
import           Backend.Types.ServerEnv
import           Backend.WsCmds
import           Backend.WsUtils
import           Common.Api
import           Common.Route
import           Common.Types.BuildJob
import           Common.Types.BuildMsg
import           Common.Types.CiSettings
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

------------------------------------------------------------------------------
-- | Generates a signing key for the Zeus nix cache.  If there is no key, this
-- function generates a new one.  The key name parameter is recommended to be
-- the domain name followed by "-1" (or other number) to allow for key
-- rotation.
getSigningKey :: String -> IO NixCacheKeyPair
getSigningKey keyName = do
  let baseName = "zeus-cache-key"
      secretFile = baseName <> ".sec"
      publicFile = baseName <> ".pub"
      secretPath = "config/backend/" <> secretFile
      publicPath = "config/common/" <> publicFile
  secretExists <- doesFileExist secretPath
  publicExists <- doesFileExist publicPath
  when (not $ secretExists && publicExists) $ do
    let args =
          [ "--generate-binary-cache-key"
          , keyName
          , secretFile
          , publicFile
          ]
    putStrLn "Generating cache signing key"
    putStrLn $ unwords (nixStore : args)
    ec <- rawSystem nixStore args
    case ec of
      ExitFailure c -> error $ printf "Error %d: Could not generate nix cache key" c
      ExitSuccess -> return ()
    renameFile secretFile secretPath
    renameFile publicFile publicPath

  Right secret <- readKeyFile secretPath
  Right public <- readKeyFile publicPath
  return $ NixCacheKeyPair secret public

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
      let settingsFile = "config/backend/settings.json" :: String
      settings <- ObConfig.get (toS settingsFile) >>= \case
        Nothing -> return $ BackendSettings Nothing []
        Just bs -> do
          case A.decode $ toS bs of
            Nothing -> error ("Error parsing " <> settingsFile)
            Just s -> return s
      putStrLn $ "read settings: " <> show settings
      listeners <- newIORef mempty
      cs <- newIORef $ CiSettings "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      let appDomain = T.takeWhile (\c -> c /= ':' && c /= '/') $ T.drop 3 $ snd $ T.breakOn "://" appRoute
      keyPair <- getSigningKey $ T.unpack $ appDomain <> "-1"
      let env = ServerEnv appRoute settings secretToken dbConn buildQueue
                          connRepo buildThreads listeners cs keyPair
      _ <- forkIO $ buildManagerThread env
      serve $ serveBackendRoute env
  , _backend_routeEncoder = backendRouteEncoder
  }

enforceIpWhitelist :: [Cidr] -> Snap ()
enforceIpWhitelist [] = return ()
enforceIpWhitelist whitelist = do
  addr <- getsRequest rqClientAddr
  case parseIp (toS addr) of
    Left e -> do
      serverError $ "Couldn't parse IP returned by Snap: " <> toS e
    Right ip ->
      when (not $ any (matchesCidr ip) whitelist) $ do
        liftIO $ putStrLn $ "Rejecting connection from " <> toS addr
        notFound "Not found"

-- | Serve our dynconfigs file.
serveBackendRoute :: ServerEnv -> R BackendRoute -> Snap ()
serveBackendRoute env = \case
  BackendRoute_Cache :=> Identity ps -> do
    enforceIpWhitelist (_beSettings_ipWhitelist $ _serverEnv_settings env)
    nixCacheRoutes env ps
  BackendRoute_Hook :=> Identity hr -> case hr of
    Hook_GitHub :=> _ -> githubHandler env
    Hook_GitLab :=> _ -> gitlabHandler env
  BackendRoute_Ping :=> _ -> do
    addr <- getsRequest rqClientAddr
    writeText $ "CLIENT ADDR: " <> toS addr <> "\n"
    writeText "PONG\nPONG\nPONG\n"
  BackendRoute_RawBuildOut :=> Identity outputFile -> do
    modifyResponse (addHeader "Content-Disposition" "inline")
    serveFileAs "text/plain" (T.unpack $ "log/builds/" <> outputFile)
  BackendRoute_Websocket :=> _ -> do
    enforceIpWhitelist (_beSettings_ipWhitelist $ _serverEnv_settings env)
    wsHandler $ \conn -> do
      cid <- addConnection conn (_serverEnv_connRepo env)
      putStrLn $ "Established websocket connection with connId " <> show cid
      listJobs env conn
      listAccounts env conn
      listRepos env conn
      talkClient env cid conn
  BackendRoute_Missing :=> _ -> do
    liftIO $ putStrLn "Unknown backend route"
    writeText "Unknown backend route"

talkClient :: ServerEnv -> ConnId -> WS.Connection -> IO ()
talkClient env cid conn = do
    E.handle cleanup $ forever $ do
      clientCmd <- wsReceive conn
      putStrLn "================================="
      putStrLn $ "Got Up_ message " <> show clientCmd
      case clientCmd of
        Left e -> do
          putStrLn $ "************************************************"
          putStrLn $ "ERROR: websocketHandler couldn't decode message:"
          putStrLn e
        Right Up_ListAccounts -> listAccounts env conn
        Right (Up_ConnectAccount cas) -> mapM_ (connectAccount env) cas
        Right (Up_DelAccounts cas) -> delAccounts env conn cas
        Right Up_ListRepos -> listRepos env conn
        Right (Up_AddRepo rs) -> mapM_ (addRepo env conn) rs
        Right (Up_DelRepos rs) -> mapM_ (deleteRepo env) rs
        Right Up_GetJobs -> listJobs env conn
        Right (Up_SubscribeJobOutput jids) -> mapM_ (subscribeJob env cid) jids
        Right (Up_UnsubscribeJobOutput jids) -> mapM_ (unsubscribeJob env cid) jids
        Right (Up_CancelJobs jids) -> do
          mapM_ (cancelJobAndRemove env) jids
        Right (Up_RerunJobs jids) -> do
          mapM_ (rerunJob env) jids
        Right (Up_GetCiSettings) -> do
          cs <- readIORef (_serverEnv_ciSettings env)
          wsSend conn (Down_CiSettings cs)
        Right (Up_UpdateCiSettings cs) -> do
          writeIORef (_serverEnv_ciSettings env) cs
  where
    cRepo = _serverEnv_connRepo env
    cleanup :: E.SomeException -> IO ()
    cleanup _ = do
      removeConnection cid cRepo

subscribeJob :: ServerEnv -> ConnId -> BuildJobId -> IO ()
subscribeJob env connId jid@(BuildJobId jidInt) = do
    output <- liftIO $ T.readFile (buildOutputDir </> show jidInt <> ".txt")
    sendToConnId (_serverEnv_connRepo env) connId $ Down_JobOutput (jid, output)
    atomicModifyIORef' (_serverEnv_buildListeners env) $ \m ->
      (M.adjust (S.insert connId) jidInt m, ())

unsubscribeJob :: ServerEnv -> ConnId -> BuildJobId -> IO ()
unsubscribeJob env connId (BuildJobId jidInt) = do
    atomicModifyIORef' (_serverEnv_buildListeners env) $ \m ->
      (M.adjust (S.delete connId) jidInt m, ())

rerunJob :: ServerEnv -> BuildJobId -> IO ()
rerunJob se (BuildJobId jid) = do
    let dbConn = _serverEnv_db se
    mjob <- beamQueryConn (_serverEnv_db se) $
      runSelectReturningOne $ select $ do
        job <- all_ (_ciDb_buildJobs ciDb)
        guard_ (job ^. buildJob_id ==. (val_ jid))
        return job
    case mjob of
      Nothing -> printf "Job ID %d does not exist\n" jid
      Just job -> do
        printf "Re-running job %d\n" jid
        t <- getCurrentTime
        runBeamSqliteDebug putStrLn dbConn $ do
          runUpdate $
            update (_ciDb_buildJobs ciDb)
                   (\j -> mconcat
                            [ j ^. buildJob_receivedAt <-. val_ t
                            , j ^. buildJob_startedAt <-. val_ Nothing
                            , j ^. buildJob_endedAt <-. val_ Nothing
                            , j ^. buildJob_status <-. val_ JobPending ])
                   (\j -> _buildJob_id j ==. val_ jid)
          return ()
        void $ atomically $ writeTQueue (_serverEnv_buildQueue se) $
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
        maybe (return ()) killThread mtid
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
  -> ConnectedAccountT Maybe
  -> IO ()
connectAccount env (ConnectedAccount _ n a pr) = do
  beamQuery env $ do
    runInsert $ insert (_ciDb_connectedAccounts ciDb) $ insertExpressions
           $ maybeToList $ ConnectedAccount default_
              <$> (val_ <$> n)
              <*> (val_ <$> a)
              <*> (val_ <$> pr)
  as <- beamQuery env $ do
    runSelectReturningList $ select $ all_ (_ciDb_connectedAccounts ciDb)
  broadcast (_serverEnv_connRepo env) $ Down_ConnectedAccounts $ map (getScrubbed . scrub) as

listAccounts :: ServerEnv -> WS.Connection -> IO ()
listAccounts env wsConn = do
  accounts <- beamQuery env $
    runSelectReturningList $ select $ do
      all_ (_ciDb_connectedAccounts ciDb)
  wsSend wsConn $ Down_ConnectedAccounts $ map (getScrubbed . scrub) accounts


queryAllRepos :: ServerEnv -> IO [Repo]
queryAllRepos env =
  beamQuery env $
    runSelectReturningList $ select $ do
      all_ (_ciDb_repos ciDb)

listRepos :: ServerEnv -> WS.Connection -> IO ()
listRepos env wsConn = do
  repos <- queryAllRepos env
  wsSend wsConn $ Down_Repos repos

listJobs :: ServerEnv -> WS.Connection -> IO ()
listJobs env conn = do
  jobs <- getJobsFromDb (_serverEnv_db env)
  wsSend conn $ Down_Jobs jobs

addRepo
  :: ServerEnv
  -> WS.Connection
  -> RepoT Maybe
  -> IO ()
addRepo env wsConn
        (Repo _ (ConnectedAccountId (Just o)) (Just n) (Just ns)
              (Just c) (Just nf) (Just t) _) = do
  mca <- beamQuery env $ do
    runSelectReturningOne $ select $ do
      account <- all_ (ciDb ^. ciDb_connectedAccounts)
      guard_ (account ^. connectedAccount_id ==. (val_ o))
      return account
  let insertRepo hid = do
        putStrLn "Repository hook setup successful"
        beamQuery env $ do
          runInsert $ insert (_ciDb_repos ciDb) $ insertExpressions
            [Repo default_
                  (ConnectedAccountId $ val_ o)
                  (val_ n)
                  (val_ ns)
                  (val_ c)
                  (val_ nf)
                  (val_ t)
                  (val_ hid)
            ]
  case mca of
    Nothing -> return ()
    Just ca -> do
      putStrLn $ "Setting up new webhook for " <> show ca
      let wbu = fromMaybe (_serverEnv_publicUrl env)
                          (_beSettings_webhookBaseUrl $ _serverEnv_settings env)
      case _connectedAccount_provider ca of
        GitHub -> do
          erw <- setupGithubWebhook
            wbu
            (OAuth $ toS $ _connectedAccount_accessToken ca)
            ns n (_serverEnv_secretToken env) AllowInsecure
          case erw of
            Left e -> wsSend wsConn $ Down_Alert $ "Error setting up webhook: " <> (T.pack $ show e)
            Right rw -> do
              let Id hid = repoWebhookId rw
              insertRepo hid
        GitLab -> do
          mhid <- setupGitlabWebhook
            wbu
            ns
            n
            (_connectedAccount_accessToken ca)
            (_serverEnv_secretToken env)
          case mhid of
            Nothing -> putStrLn "Didn't get a hook ID"
            Just hid -> insertRepo $ fromIntegral hid
      as <- beamQuery env $ do
        runSelectReturningList $ select $ all_ (_ciDb_repos ciDb)
      broadcast (_serverEnv_connRepo env) $ Down_Repos as
addRepo _ _ _ = putStrLn "AddRepo got bad argument"

deleteRepo :: ServerEnv -> RepoId -> IO ()
deleteRepo env rid = do
  mrepo <- beamQuery env $
    runSelectReturningOne $ select $ do
      repo <- all_ (_ciDb_repos ciDb)
      accessAccount <- all_ (_ciDb_connectedAccounts ciDb)
      guard_ (repo ^. repo_id ==. (val_ $ repoKeyToInt rid))
      guard_ (_repo_accessAccount repo `references_` accessAccount)
      return (repo, accessAccount)

  case mrepo of
    Nothing -> return ()
    Just (repo,accessAccount) -> do
      case _connectedAccount_provider accessAccount of
        GitHub -> do
          _ <- deleteRepoWebhook'
            (OAuth $ toS $ _connectedAccount_accessToken accessAccount)
            (N $ _repo_namespace repo)
            (N $ _repo_name repo)
            (Id $ _repo_hookId repo)
          return ()
        GitLab -> do
          deleteGitlabWebhook
            (_repo_namespace repo)
            (_repo_name repo)
            (_connectedAccount_accessToken accessAccount)
            (_repo_hookId repo)

      beamQuery env $
        runDelete $ delete (_ciDb_repos ciDb) $
            (\r -> r ^. repo_id ==. val_ (repoKeyToInt rid))

      as <- beamQuery env $
        runSelectReturningList $ select $
          all_ (_ciDb_repos ciDb)
      broadcast (_serverEnv_connRepo env) $ Down_Repos as
      return ()

delAccounts :: ServerEnv -> WS.Connection -> [ConnectedAccountId] -> IO ()
delAccounts env wsConn cas = do
  beamQuery env $
    runDelete $ delete (_ciDb_connectedAccounts ciDb) $
        (\ca -> ca ^. connectedAccount_id `in_` map (val_ . caKeyToInt) cas)
  listAccounts env wsConn
