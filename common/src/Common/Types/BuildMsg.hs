{-# LANGUAGE DeriveGeneric #-}

module Common.Types.BuildMsg where

------------------------------------------------------------------------------
import           GHC.Generics
------------------------------------------------------------------------------
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

------------------------------------------------------------------------------
data BuildMsg = BuildMsg
  { _buildMsg_jobId :: Int
  , _buildMsg_repoBuildInfo :: RepoBuildInfo
  } deriving (Eq,Generic)
