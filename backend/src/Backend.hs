{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Error
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.Aeson as A
import           Data.Dependent.Sum (DSum ((:=>)))
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
import           Network.WebSockets
import qualified Network.WebSockets as WS
import           Network.WebSockets.Snap
import           Obelisk.Backend
import           Obelisk.Route
import           Scrub
import           Snap.Core
import           System.Directory
------------------------------------------------------------------------------
import           Backend.Build
import           Backend.Db
import           Backend.Github
import           Backend.Types.ServerEnv
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
      conn <- open dbConnectInfo
      runBeamSqlite conn $ autoMigrate migrationBackend ciDbChecked
      appRoute <- getAppRoute
      secretToken <- getSecretToken
      buildQueue <- atomically newTQueue
      _ <- forkIO (buildThread conn buildQueue)

      let env = ServerEnv appRoute secretToken conn buildQueue
      serve $ serveBackendRoute env
  , _backend_routeEncoder = backendRouteEncoder
  }

-- | Serve our dynconfigs file.
serveBackendRoute :: ServerEnv -> R BackendRoute -> Snap ()
serveBackendRoute env = \case
  BackendRoute_GithubHook :=> _ -> hookHandler env
  BackendRoute_Ping :=> _ -> writeText "PONG\nPONG\nPONG\n"
  BackendRoute_Websocket :=> _ -> wsHandler env
  BackendRoute_Missing :=> _ -> do
    liftIO $ putStrLn "Unknown backend route"
    writeText "Unknown backend route"

beamQuery :: ServerEnv -> SqliteM a -> IO a
beamQuery env f = do
    runBeamSqliteDebug putStrLn (_serverEnv_db env) f

wsSendJson :: ToJSON a => WS.Connection -> a -> IO ()
wsSendJson conn a =
    sendDataMessage conn $ Text bs (Just $ toS bs)
  where
    bs = A.encode a

wsHandler :: ServerEnv -> Snap ()
wsHandler env = do
    res <- runExceptT $ do
      runWebSocketsSnap $ \pc -> do
        conn <- acceptRequest pc
        forever $ do
          bs <- receiveDataMessage conn >>= \case
            Text bs _ -> return bs
            Binary bs -> return bs
          case eitherDecodeStrict (toS bs) of
            Left e -> do
              putStrLn $ "************************************************"
              putStrLn $ "ERROR: websocketHandler couldn't decode message:"
              putStrLn e
            Right Up_ListAccounts -> listAccounts env conn
            Right (Up_ConnectAccount cas) -> mapM_ (connectAccount env conn) cas
            Right (Up_DelAccounts cas) -> delAccounts env conn cas
    case res of
      Left e -> liftIO $ T.putStrLn $ "User hasn't linked their acccount: " <> e
      Right _ -> return ()

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
  wsSendJson wsConn $ Down_ConnectedAccounts $ map (getScrubbed . scrub . caToMaybe) as

listAccounts :: ServerEnv -> WS.Connection -> IO ()
listAccounts env wsConn = do
  accounts <- beamQuery env $
    runSelectReturningList $ select $ do
      all_ (_ciDb_connectedAccounts ciDb)
  putStrLn "--------------"
  putStrLn "Sending list of accounts:"
  print accounts
  wsSendJson wsConn $ Down_ConnectedAccounts $ map (getScrubbed . scrub) $ caToMaybe <$> accounts

delAccounts :: ServerEnv -> WS.Connection -> [Int] -> IO ()
delAccounts env wsConn cas = do
  beamQuery env $
    runDelete $ delete (_ciDb_connectedAccounts ciDb) $
        (\ca -> _connectedAccount_id ca `in_` map val_ cas)
  listAccounts env wsConn
