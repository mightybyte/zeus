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

module Common.Types.Builder where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
------------------------------------------------------------------------------
import           Common.Types.Platform
------------------------------------------------------------------------------

data BuilderStatus
  = BuilderWorking
  | BuilderIdle
  | BuilderUnavailable
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)

instance BeamMigrateSqlBackend be => HasSqlEqualityCheck be BuilderStatus

instance HasSqlValueSyntax be String => HasSqlValueSyntax be BuilderStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be BuilderStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be BuilderStatus where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance ToJSON BuilderStatus where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BuilderStatus


data BuilderT f = Builder
  { _builder_id :: C f Int
  , _builder_user :: C f Text -- ^ The user to connect as
  , _builder_host :: C f Text -- ^ Hostname or IP address
  , _builder_port :: C f Int -- ^ Port to connect to
  , _builder_platform :: C f Platform
  , _builder_maxBuilds :: C f Int
  , _builder_speedFactor :: C f Int
  , _builder_status :: C f BuilderStatus
  , _builder_lastStatusUpdate :: C f UTCTime
  } deriving Generic

type Builder = BuilderT Identity
type BuilderId = PrimaryKey BuilderT Identity
type MBuilderId = PrimaryKey BuilderT (Nullable Identity)

mkeyToNullable :: Maybe BuilderId -> MBuilderId
mkeyToNullable Nothing = BuilderId Nothing
mkeyToNullable (Just (BuilderId i)) = BuilderId $ Just i

Builder
  (LensFor builder_id)
  (LensFor builder_user)
  (LensFor builder_host)
  (LensFor builder_port)
  (LensFor builder_platform)
  (LensFor builder_maxBuilds)
  (LensFor builder_speedFactor)
  (LensFor builder_status)
  (LensFor builder_lastStatusUpdate)
  = tableLenses

deriving instance Eq Builder
deriving instance Eq (BuilderT Maybe)
deriving instance Eq (PrimaryKey BuilderT Identity)
deriving instance Eq (PrimaryKey BuilderT Maybe)
deriving instance Eq (PrimaryKey BuilderT (Nullable Identity))
deriving instance Eq (PrimaryKey BuilderT (Nullable Maybe))
deriving instance Ord Builder
deriving instance Ord (BuilderT Maybe)
deriving instance Ord (PrimaryKey BuilderT Identity)
deriving instance Ord (PrimaryKey BuilderT Maybe)
deriving instance Ord (PrimaryKey BuilderT (Nullable Identity))
deriving instance Ord (PrimaryKey BuilderT (Nullable Maybe))
deriving instance Show Builder
deriving instance Show (BuilderT Maybe)
deriving instance Show (PrimaryKey BuilderT Identity)
deriving instance Show (PrimaryKey BuilderT Maybe)
deriving instance Show (PrimaryKey BuilderT (Nullable Identity))
deriving instance Show (PrimaryKey BuilderT (Nullable Maybe))

instance ToJSON (PrimaryKey BuilderT (Nullable Identity)) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey BuilderT (Nullable Identity))

instance ToJSON (PrimaryKey BuilderT (Nullable Maybe)) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey BuilderT (Nullable Maybe))

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
  data PrimaryKey BuilderT f = BuilderId { unBuilderId :: Columnar f Int }
    deriving (Generic, Beamable)
  primaryKey = BuilderId . _builder_id

builderKeyToInt :: PrimaryKey BuilderT Identity -> Int
builderKeyToInt (BuilderId k) = k
