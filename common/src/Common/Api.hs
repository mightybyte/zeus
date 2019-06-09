{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Api where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import           Database.Beam
------------------------------------------------------------------------------
import           Common.Types.BuildJob
import           Common.Types.ConnectedAccount
import           Common.Types.ProcMsg
import           Common.Types.Repo
------------------------------------------------------------------------------

type Batch a = [a]

batchOne :: a -> Batch a
batchOne a = [a]

--batchMaybe :: FunctorMaybe f => (a -> Batch b) -> f a -> f b
--batchMaybe f = fmapMaybe (listToMaybe . f)

data Up
  = Up_ListAccounts
  | Up_ConnectAccount (Batch (ConnectedAccountT Maybe))
  | Up_DelAccounts (Batch ConnectedAccountId)
  | Up_ListRepos
  | Up_AddRepo (Batch (RepoT Maybe))
  | Up_DelRepos (Batch RepoId)
  | Up_GetJobs
  | Up_SubscribeJobOutput (Batch BuildJobId)
  | Up_UnsubscribeJobOutput (Batch BuildJobId)
  | Up_CancelJobs (Batch BuildJobId)
  | Up_RerunJobs (Batch BuildJobId)
  deriving (Show,Generic)

data Down
  = Down_Alert Text
  | Down_ConnectedAccounts [ConnectedAccount]
  | Down_Repos [Repo]
  | Down_Jobs [BuildJob]
  | Down_JobOutput (BuildJobId, Text)
  | Down_JobNewOutput (BuildJobId, [ProcMsg])
  deriving (Generic)

instance ToJSON Up where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Up

instance ToJSON Down where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Down

makePrisms ''Up
makePrisms ''Down
