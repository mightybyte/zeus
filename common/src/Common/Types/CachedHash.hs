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

module Common.Types.CachedHash where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import           Data.Time
import           Database.Beam
------------------------------------------------------------------------------
import           Common.Types.BinaryCache
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data CachedHashT f = CachedHash
  { _cachedHash_hash :: C f Text
  , _cachedHash_cache :: PrimaryKey BinaryCacheT f
  , _cachedHash_time :: C f UTCTime
  } deriving Generic

CachedHash
  (LensFor cachedHash_hash)
  (BinaryCacheId (LensFor cachedHash_cache))
  (LensFor cachedHash_time)
  = tableLenses

type CachedHash = CachedHashT Identity

deriving instance Eq (PrimaryKey CachedHashT Identity)
deriving instance Eq (PrimaryKey CachedHashT Maybe)
deriving instance Eq CachedHash
deriving instance Show (PrimaryKey CachedHashT Identity)
deriving instance Show (PrimaryKey CachedHashT Maybe)
deriving instance Show CachedHash
deriving instance Show (CachedHashT Maybe)
deriving instance Ord (PrimaryKey CachedHashT Identity)
deriving instance Ord (PrimaryKey CachedHashT Maybe)
deriving instance Ord CachedHash

instance ToJSON (CachedHashT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (CachedHashT Identity)

instance ToJSON (CachedHashT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (CachedHashT Maybe)

instance Beamable CachedHashT

instance Table CachedHashT where
  data PrimaryKey CachedHashT f = CachedHashId (PrimaryKey BinaryCacheT f) (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey ch = CachedHashId (_cachedHash_cache ch) (_cachedHash_hash ch)
