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

module Common.Types.BinaryCache where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Int
--import           Data.Text (Text)
import           Database.Beam
------------------------------------------------------------------------------
import           Common.Types.S3Cache
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data BinaryCacheT f = BinaryCache
  { _binaryCache_id :: C f Int32
--  , _binaryCache_url :: C f Text
--  , _binaryCache_storeDir :: C f Text
  , _binaryCache_s3Cache :: C f S3Cache
  } deriving Generic

BinaryCache
  (LensFor binaryCache_id)
--  (LensFor binaryCache_url)
--  (LensFor binaryCache_storeDir)
  (LensFor binaryCache_s3Cache)
  = tableLenses

bcToMaybe :: BinaryCacheT Identity -> BinaryCacheT Maybe
bcToMaybe (BinaryCache i c) = BinaryCache (Just i) (Just c)

type BinaryCache = BinaryCacheT Identity
type BinaryCacheId = PrimaryKey BinaryCacheT Identity

deriving instance Eq (PrimaryKey BinaryCacheT (Nullable Identity))
deriving instance Eq (PrimaryKey BinaryCacheT (Nullable Maybe))
deriving instance Eq (PrimaryKey BinaryCacheT Identity)
deriving instance Eq (PrimaryKey BinaryCacheT Maybe)
deriving instance Eq BinaryCache
deriving instance Show (PrimaryKey BinaryCacheT (Nullable Identity))
deriving instance Show (PrimaryKey BinaryCacheT (Nullable Maybe))
deriving instance Show (PrimaryKey BinaryCacheT Identity)
deriving instance Show (PrimaryKey BinaryCacheT Maybe)
deriving instance Show BinaryCache
deriving instance Show (BinaryCacheT Maybe)
deriving instance Ord (PrimaryKey BinaryCacheT (Nullable Identity))
deriving instance Ord (PrimaryKey BinaryCacheT (Nullable Maybe))
deriving instance Ord (PrimaryKey BinaryCacheT Identity)
deriving instance Ord (PrimaryKey BinaryCacheT Maybe)
deriving instance Ord BinaryCache


instance ToJSON (PrimaryKey BinaryCacheT (Nullable Identity)) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey BinaryCacheT (Nullable Identity))

instance ToJSON (PrimaryKey BinaryCacheT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey BinaryCacheT Identity)

instance ToJSON (PrimaryKey BinaryCacheT (Nullable Maybe)) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey BinaryCacheT (Nullable Maybe))

instance ToJSON (PrimaryKey BinaryCacheT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey BinaryCacheT Maybe)

instance ToJSON (BinaryCacheT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (BinaryCacheT Identity)

instance ToJSON (BinaryCacheT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (BinaryCacheT Maybe)

instance Beamable BinaryCacheT

instance Table BinaryCacheT where
  data PrimaryKey BinaryCacheT f = BinaryCacheId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = BinaryCacheId . _binaryCache_id

binaryCacheKeyToInt :: PrimaryKey BinaryCacheT Identity -> Int32
binaryCacheKeyToInt (BinaryCacheId k) = k
