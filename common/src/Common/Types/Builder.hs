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

module Common.Types.Builder where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import           Database.Beam
------------------------------------------------------------------------------
import           Common.Types.Platform
------------------------------------------------------------------------------

data BuilderT f = Builder
  { _builder_id :: C f Int
  , _builder_user :: C f Text -- ^ The user to connect as
  , _builder_host :: C f Text -- ^ Hostname or IP address
  , _builder_platform :: C f Platform
  , _builder_maxBuilds :: C f Int
  , _builder_speedFactor :: C f Int
  } deriving Generic

type Builder = BuilderT Identity
type BuilderId = PrimaryKey BuilderT Identity

Builder
  (LensFor builder_id)
  (LensFor builder_user)
  (LensFor builder_host)
  (LensFor builder_platform)
  (LensFor builder_maxBuilds)
  (LensFor builder_speedFactor)
  = tableLenses

deriving instance Eq Builder
deriving instance Eq (BuilderT Maybe)
deriving instance Eq (PrimaryKey BuilderT Identity)
deriving instance Eq (PrimaryKey BuilderT Maybe)
deriving instance Ord Builder
deriving instance Ord (BuilderT Maybe)
deriving instance Ord (PrimaryKey BuilderT Identity)
deriving instance Ord (PrimaryKey BuilderT Maybe)
deriving instance Show Builder
deriving instance Show (BuilderT Maybe)
deriving instance Show (PrimaryKey BuilderT Identity)
deriving instance Show (PrimaryKey BuilderT Maybe)

instance ToJSON (PrimaryKey BuilderT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey BuilderT Identity)

instance ToJSON (PrimaryKey BuilderT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey BuilderT Maybe)

instance ToJSON (BuilderT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (BuilderT Identity)

instance ToJSON (BuilderT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (BuilderT Maybe)

instance Beamable BuilderT

instance Table BuilderT where
  data PrimaryKey BuilderT f = BuilderId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = BuilderId . _builder_id

builderKeyToInt :: PrimaryKey BuilderT Identity -> Int
builderKeyToInt (BuilderId k) = k
