{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Api where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson
import           Database.Beam
------------------------------------------------------------------------------
import           Common.Types.BuildJob
import           Common.Types.ConnectedAccount
------------------------------------------------------------------------------

type Batch a = [a]

batchOne :: a -> Batch a
batchOne a = [a]

--batchMaybe :: FunctorMaybe f => (a -> Batch b) -> f a -> f b
--batchMaybe f = fmapMaybe (listToMaybe . f)

data Up
  = Up_ListAccounts
  | Up_ConnectAccount (Batch (ConnectedAccountT Maybe))
  | Up_DelAccounts (Batch Int)
  | Up_GetJobs
  deriving (Generic)

data Down
  = Down_ConnectedAccounts [ConnectedAccountT Maybe]
  | Down_Jobs [BuildJob]
  deriving (Generic)

instance ToJSON Up where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Up

instance ToJSON Down where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Down

makePrisms ''Up
makePrisms ''Down
