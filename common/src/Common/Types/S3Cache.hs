{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Types.S3Cache where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import           Data.Readable
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

-- Copying this code from amazonka to avoid the dep in GHCJS
data Region
    = NorthVirginia   -- ^ US East ('us-east-1').
    | Ohio            -- ^ US East ('us-east-2').
    | NorthCalifornia -- ^ US West ('us-west-1').
    | Oregon          -- ^ US West ('us-west-2').
    | Montreal        -- ^ Canada ('ca-central-1').
    | Tokyo           -- ^ Asia Pacific ('ap-northeast-1').
    | Seoul           -- ^ Asia Pacific ('ap-northeast-2').
    | Mumbai          -- ^ Asia Pacific ('ap-south-1').
    | Singapore       -- ^ Asia Pacific ('ap-southeast-1').
    | Sydney          -- ^ Asia Pacific ('ap-southeast-2').
    | SaoPaulo        -- ^ South America ('sa-east-1').
    | Ireland         -- ^ EU ('eu-west-1').
    | London          -- ^ EU ('eu-west-2').
    | Frankfurt       -- ^ EU ('eu-central-1').
    | GovCloud        -- ^ US GovCloud ('us-gov-west-1').
    | GovCloudFIPS    -- ^ US GovCloud FIPS (S3 Only, 'fips-us-gov-west-1').
    | Beijing         -- ^ China ('cn-north-1').
      deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Readable Region where
    fromText = \case
        "us-east-1"          -> pure NorthVirginia
        "us-east-2"          -> pure Ohio
        "us-west-1"          -> pure NorthCalifornia
        "us-west-2"          -> pure Oregon
        "ca-central-1"       -> pure Montreal
        "ap-northeast-1"     -> pure Tokyo
        "ap-northeast-2"     -> pure Seoul
        "ap-south-1"         -> pure Mumbai
        "ap-southeast-1"     -> pure Singapore
        "ap-southeast-2"     -> pure Sydney
        "sa-east-1"          -> pure SaoPaulo
        "eu-west-1"          -> pure Ireland
        "eu-west-2"          -> pure London
        "eu-central-1"       -> pure Frankfurt
        "us-gov-west-1"      -> pure GovCloud
        "fips-us-gov-west-1" -> pure GovCloudFIPS
        "cn-north-1"         -> pure Beijing
        _                    -> mzero

regionText :: Region -> Text
regionText = \case
    NorthVirginia   -> "us-east-1"
    Ohio            -> "us-east-2"
    NorthCalifornia -> "us-west-1"
    Oregon          -> "us-west-2"
    Montreal        -> "ca-central-1"
    Tokyo           -> "ap-northeast-1"
    Seoul           -> "ap-northeast-2"
    Mumbai          -> "ap-south-1"
    Singapore       -> "ap-southeast-1"
    Sydney          -> "ap-southeast-2"
    SaoPaulo        -> "sa-east-1"
    Ireland         -> "eu-west-1"
    London          -> "eu-west-2"
    Frankfurt       -> "eu-central-1"
    GovCloud        -> "us-gov-west-1"
    GovCloudFIPS    -> "fips-us-gov-west-1"
    Beijing         -> "cn-north-1"

instance ToJSON Region where
    toJSON = String . regionText
    toEncoding = toEncoding . regionText

instance FromJSON Region where
    parseJSON = withText "Region" (maybe (fail "Invalid Region format") pure . fromText)

data S3Cache = S3Cache
  { _s3Cache_bucket :: Text
  , _s3Cache_region :: Region
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
