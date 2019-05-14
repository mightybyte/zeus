{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.Types.ServerEnv where

------------------------------------------------------------------------------
import           Control.Concurrent.STM.TQueue
import           Data.Text (Text)
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
------------------------------------------------------------------------------
import           Backend.Types.ConnRepo
import           Common.Types.BuildMsg
------------------------------------------------------------------------------

data ServerEnv = ServerEnv
  { _serverEnv_publicUrl :: Text
  -- ^ The public URL at which this CI server can be reached
  , _serverEnv_secretToken :: Text
  -- ^ The secret token this server requires to determine legitimacy of
  -- incoming requests
  , _serverEnv_db :: Connection
  , _serverEnv_buildQueue :: TQueue BuildMsg
  , _serverEnv_connRepo :: ConnRepo
  }

beamQuery :: ServerEnv -> SqliteM a -> IO a
beamQuery env = beamQueryConn (_serverEnv_db env)

beamQueryConn :: Connection -> SqliteM a -> IO a
beamQueryConn conn f = runBeamSqliteDebug putStrLn conn f
