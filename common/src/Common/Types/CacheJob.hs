{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Types.CacheJob where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time
import           Database.Beam
------------------------------------------------------------------------------
import           Common.Types.BinaryCache
import           Common.Types.JobStatus
------------------------------------------------------------------------------

------------------------------------------------------------------------------
data CacheJobT f = CacheJob
  { _cacheJob_id :: C f Int
  , _cacheJob_storePath :: C f Text
  , _cacheJob_cache :: PrimaryKey BinaryCacheT (Nullable f)
  , _cacheJob_startedAt :: C f (Maybe UTCTime)
  , _cacheJob_endedAt :: C f (Maybe UTCTime)
  , _cacheJob_status :: C f JobStatus
  } deriving (Generic)

CacheJob
  (LensFor cacheJob_id)
  (LensFor cacheJob_nixPath)
  (BinaryCacheId (LensFor cacheJob_cache))
  (LensFor cacheJob_startedAt)
  (LensFor cacheJob_endedAt)
  (LensFor cacheJob_status)
  = tableLenses

type CacheJob = CacheJobT Identity
type CacheJobId = PrimaryKey CacheJobT Identity

deriving instance Eq (PrimaryKey CacheJobT Identity)
deriving instance Eq CacheJob
deriving instance Ord (PrimaryKey CacheJobT Identity)
deriving instance Ord CacheJob
deriving instance Show (PrimaryKey CacheJobT Identity)
deriving instance Show CacheJob

instance ToJSON (PrimaryKey CacheJobT Identity) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey CacheJobT Identity)

instance ToJSON CacheJob where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CacheJob

instance Beamable CacheJobT

instance Table CacheJobT where
  data PrimaryKey CacheJobT f = CacheJobId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = CacheJobId . _cacheJob_id
