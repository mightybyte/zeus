{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Api where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import           Database.Beam
import           Scrub
------------------------------------------------------------------------------
import           Common.Types.BinaryCache
import           Common.Types.BuildJob
import           Common.Types.Builder
import           Common.Types.CiSettings
import           Common.Types.ConnectedAccount
import           Common.Types.ProcMsg
import           Common.Types.Repo
------------------------------------------------------------------------------

type Batch a = [a]

batchOne :: a -> Batch a
batchOne a = [a]

instance ToJSON a => ToJSON (Scrubbed a) where
  toJSON = toJSON . getScrubbed

instance FromJSON a => FromJSON (Scrubbed a) where
  parseJSON = fmap Scrubbed . parseJSON

--batchMaybe :: FunctorMaybe f => (a -> Batch b) -> f a -> f b
--batchMaybe f = fmapMaybe (listToMaybe . f)

--data CrudUpMsg (entityT :: (* -> *) -> Type) where
--  CrudUp_List :: CrudUpMsg entityT
--  CrudUp_Create :: Batch (entityT Maybe) -> CrudUpMsg entityT
--  CrudUp_Update :: Batch (entityT Identity) -> CrudUpMsg entityT
--  CrudUp_Delete :: Batch (PrimaryKey entityT Identity) -> CrudUpMsg entityT
--  deriving Generic
--
--instance Show (CrudUpMsg eT) where
--  show CrudUp_List = "list"
--  show (CrudUp_Create _) = "new"
--  show (CrudUp_Update _) = "edit"
--  show (CrudUp_Delete _) = "delete"
--
--instance ToJSON (CrudUpMsg f) where
--    toEncoding = genericToEncoding defaultOptions
--
--instance FromJSON (CrudUpMsg f)

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
  | Up_GetCiSettings
  | Up_UpdateCiSettings CiSettings
  | Up_GetCiInfo

  | Up_ListCaches
  | Up_AddCache (Batch (BinaryCacheT Maybe))
  | Up_DelCaches (Batch BinaryCacheId)

  | Up_ListBuilders
  | Up_CreateBuilders (Batch (BuilderT Maybe))
  | Up_DelBuilders (Batch BuilderId)

  deriving (Show,Generic)

data Down
  = Down_Alert Text
  | Down_ConnectedAccounts [ConnectedAccount]
  | Down_Repos [Repo]
  | Down_Jobs [BuildJob]
  | Down_JobOutput (BuildJobId, Text)
  | Down_JobNewOutput (BuildJobId, [ProcMsg])
  | Down_CiSettings (Scrubbed CiSettings)
  | Down_CiInfo Text
  | Down_Caches [BinaryCache]
  | Down_Builders [Builder]
  deriving (Generic)

instance ToJSON Up where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Up

instance ToJSON Down where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Down

makePrisms ''Up
makePrisms ''Down
