{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.Types.ServerEnv where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM.TQueue
import           Data.IORef
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Text (Text)
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           System.Mem.Weak
------------------------------------------------------------------------------
import           Backend.Types.ConnRepo
import           Common.Types.BuildMsg
------------------------------------------------------------------------------

data ServerEnv = ServerEnv
  { _serverEnv_publicUrl :: Text
  -- ^ The public URL at which this CI server can be reached
  , _serverEnv_webhookBaseUrl :: Text
  -- ^ Sometimes this needs to be different from publicUrl, for instance when
  -- doing local development behind NAT and you want to test the webhooks via
  -- reverse port forwarding or similar.  This field defaults to be the same
  -- as publicUrl unless an override is found in a particular file in the cwd.
  -- See Backend.hs for more information.
  , _serverEnv_secretToken :: Text
  -- ^ The secret token this server requires to determine legitimacy of
  -- incoming requests
  , _serverEnv_db :: Connection
  , _serverEnv_buildQueue :: TQueue BuildMsg
  , _serverEnv_connRepo :: ConnRepo
  -- ^ Websocket connection repo that allows job updates to be pushed
  , _serverEnv_buildThreads :: IORef (Map Int (Weak ThreadId))
  , _serverEnv_buildListeners :: IORef (Map Int (Set ConnId))
  }

beamQuery :: ServerEnv -> SqliteM a -> IO a
beamQuery env = beamQueryConn (_serverEnv_db env)

beamQueryConn :: Connection -> SqliteM a -> IO a
beamQueryConn conn f = runBeamSqliteDebug putStrLn conn f
