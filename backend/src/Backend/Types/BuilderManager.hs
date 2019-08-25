{-| Manages the startup, teardown, monitering, etc of all the builders and
 - their associated threads.
 -}

module Backend.Types.BuilderManager where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Data.IORef
import           Data.Map (Map)
------------------------------------------------------------------------------
import           Common.Types.Builder
------------------------------------------------------------------------------

newtype BuilderManager = BuilderManager
  { _builderManager_threads :: IORef (Map MBuilderId ThreadId)
  }

