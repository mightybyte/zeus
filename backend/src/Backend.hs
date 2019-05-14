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
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Dependent.Sum (DSum ((:=>)))
import           Data.Map (Map)
import qualified Data.Map as M
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
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import           Obelisk.Backend
import           Obelisk.Route
import           Scrub
import           Snap.Core
import           System.Directory
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
import           Common.Types.ConnectedAccount
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
      _ <- forkIO (buildThread dbConn connRepo buildQueue)

      let env = ServerEnv appRoute secretToken dbConn buildQueue connRepo
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
        Right Up_GetJobs -> do
          let dbConn = _serverEnv_db env
          jobs <- getJobsFromDb dbConn
          sendJobs jobs conn
  where
    cRepo = _serverEnv_connRepo env
    cleanup :: E.SomeException -> IO ()
    cleanup _ = do
      removeConnection cid cRepo

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
  wsSend wsConn $ Down_ConnectedAccounts $ map (getScrubbed . scrub . caToMaybe) as

listAccounts :: ServerEnv -> WS.Connection -> IO ()
listAccounts env wsConn = do
  accounts <- beamQuery env $
    runSelectReturningList $ select $ do
      all_ (_ciDb_connectedAccounts ciDb)
  putStrLn "--------------"
  putStrLn "Sending list of accounts:"
  print accounts
  wsSend wsConn $ Down_ConnectedAccounts $ map (getScrubbed . scrub) $ caToMaybe <$> accounts

delAccounts :: ServerEnv -> WS.Connection -> [Int] -> IO ()
delAccounts env wsConn cas = do
  beamQuery env $
    runDelete $ delete (_ciDb_connectedAccounts ciDb) $
        (\ca -> _connectedAccount_id ca `in_` map val_ cas)
  listAccounts env wsConn
