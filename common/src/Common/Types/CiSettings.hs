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

module Common.Types.CiSettings where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson
import           Data.String.Conv
import           Data.Text (Text)
import           Data.Text.Encoding
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
import           Scrub
------------------------------------------------------------------------------

data S3Cache = S3Cache
  { _s3Cache_bucket :: Text
  , _s3Cache_region :: Text
  , _s3Cache_accessKey :: Text
  , _s3Cache_secretKey :: Text
  } deriving (Eq,Ord,Show,Read,Generic)

instance Scrub S3Cache where
  scrub c = Scrubbed $ c { _s3Cache_secretKey = "" }

instance ToJSON S3Cache where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON S3Cache

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be S3Cache where
  sqlValueSyntax = sqlValueSyntax . decodeUtf8 . toS . encode

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be S3Cache where
  fromBackendRow = maybe (fail "Could not parse S3Cache") return . decodeStrict . encodeUtf8 =<< fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be S3Cache where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

------------------------------------------------------------------------------
data CiSettingsT f = CiSettings
  { _ciSettings_id :: C f Int
  , _ciSettings_nixPath :: C f Text
  , _ciSettings_s3Cache :: C f (Maybe S3Cache)
  , _ciSettings_serveLocalCache :: C f Bool
  } deriving (Generic)

instance Scrub (CiSettingsT Identity) where
  scrub s = Scrubbed $ s { _ciSettings_s3Cache = getScrubbed $ scrub (_ciSettings_s3Cache s) }

CiSettings
  (LensFor ciSettings_id)
  (LensFor ciSettings_nixPath)
  (LensFor ciSettings_s3Cache)
  (LensFor ciSettings_serveLocalCache)
  = tableLenses

type CiSettings = CiSettingsT Identity
type CiSettingsId = PrimaryKey CiSettingsT Identity

deriving instance Eq (PrimaryKey CiSettingsT Identity)
deriving instance Eq CiSettings
deriving instance Ord (PrimaryKey CiSettingsT Identity)
deriving instance Ord CiSettings
deriving instance Show (PrimaryKey CiSettingsT Identity)
deriving instance Show CiSettings

instance ToJSON (PrimaryKey CiSettingsT Identity) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey CiSettingsT Identity)

instance ToJSON CiSettings where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CiSettings

instance Beamable CiSettingsT

instance Table CiSettingsT where
  data PrimaryKey CiSettingsT f = CiSettingsId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = CiSettingsId . _ciSettings_id
