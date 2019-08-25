{-
 - This is the DB type for Zeus internal log messages.
-}
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

module Common.Types.ZeusMsg where

------------------------------------------------------------------------------
import           Control.Lens
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
import           Common.Types.BinaryCache
import           Common.Types.Builder
import           Common.Types.JobStatus
------------------------------------------------------------------------------

data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  | Fatal
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)

instance BeamMigrateSqlBackend be => HasSqlEqualityCheck be LogLevel

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LogLevel where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be LogLevel where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be LogLevel where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance ToJSON LogLevel where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LogLevel

------------------------------------------------------------------------------
-- | Currently used to log messages from builders, hence the BuilderId.  This
-- may be expanded in the future.
data ZeusMsgT f = ZeusMsg
  { _zeusMsg_id :: C f Int
  , _zeusMsg_builder :: PrimaryKey BuilderT (Nullable f)
  , _zeusMsg_timestamp :: C f UTCTime
  , _zeusMsg_level :: C f LogLevel
  , _zeusMsg_msg :: C f Text
  } deriving (Generic)

ZeusMsg
  (LensFor zeusMsg_id)
  (BuilderId (LensFor zeusMsg_builder))
  (LensFor zeusMsg_timestamp)
  (LensFor zeusMsg_level)
  (LensFor zeusMsg_msg)
  = tableLenses

type ZeusMsg = ZeusMsgT Identity
type ZeusMsgId = PrimaryKey ZeusMsgT Identity

deriving instance Eq (PrimaryKey ZeusMsgT (Nullable Identity))
deriving instance Eq (PrimaryKey ZeusMsgT (Nullable Maybe))
deriving instance Eq (PrimaryKey ZeusMsgT Identity)
deriving instance Eq ZeusMsg
deriving instance Ord (PrimaryKey ZeusMsgT (Nullable Identity))
deriving instance Ord (PrimaryKey ZeusMsgT (Nullable Maybe))
deriving instance Ord (PrimaryKey ZeusMsgT Identity)
deriving instance Ord ZeusMsg
deriving instance Show (PrimaryKey ZeusMsgT (Nullable Identity))
deriving instance Show (PrimaryKey ZeusMsgT (Nullable Maybe))
deriving instance Show (PrimaryKey ZeusMsgT Identity)
deriving instance Show ZeusMsg

instance ToJSON (PrimaryKey ZeusMsgT (Nullable Identity)) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey ZeusMsgT (Nullable Identity))

instance ToJSON (PrimaryKey ZeusMsgT (Nullable Maybe)) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey ZeusMsgT (Nullable Maybe))

instance ToJSON (PrimaryKey ZeusMsgT Identity) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey ZeusMsgT Identity)

instance ToJSON ZeusMsg where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ZeusMsg

instance Beamable ZeusMsgT

instance Table ZeusMsgT where
  data PrimaryKey ZeusMsgT f = ZeusMsgId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ZeusMsgId . _zeusMsg_id

