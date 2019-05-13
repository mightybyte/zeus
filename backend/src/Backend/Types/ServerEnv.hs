module Backend.Types.ServerEnv where

------------------------------------------------------------------------------
import           Control.Concurrent.STM.TQueue
import           Data.Text (Text)
import           Database.SQLite.Simple
------------------------------------------------------------------------------
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
  }
