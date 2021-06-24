{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Common.Types.ConnectedAccount where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Default
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import qualified Database.Beam.AutoMigrate as BA
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
import           Scrub
------------------------------------------------------------------------------

data AccountProvider = GitHub | GitLab
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)

providerUrl :: AccountProvider -> Text
providerUrl GitHub = "https://github.com"
providerUrl GitLab = "https://gitlab.com"

instance ToJSON AccountProvider where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AccountProvider

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AccountProvider where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be AccountProvider where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be AccountProvider where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance BA.HasColumnType AccountProvider where
  defaultColumnType _ = BA.SqlStdType $ varCharType Nothing Nothing

--data WebhookInfo f = WebhookInfo
--  { _webhookInfo_url :: C f Text
--  , _webhookInfo_testUrl :: C f Text
--  , _webhookInfo_id :: C f Int32
--  , _webhookInfo_name :: C f Text
--  , _webhookInfo_active :: C f Bool
--  } deriving Generic

------------------------------------------------------------------------------
data ConnectedAccountT f = ConnectedAccount
  { _connectedAccount_id :: C f Int32
  , _connectedAccount_name :: C f Text
  -- ^ Name of the account (or organization) that owns the repositories that
  -- you are connecting to.  This should NOT be the username of the account
  -- you authenticate to github with.
  , _connectedAccount_accessToken :: C f Text
  -- ^ The access token to the account that grants you access to the repo
  -- (even if it is different from the owner of the repo).
  , _connectedAccount_provider :: C f AccountProvider
  } deriving (Generic)

--deriving anyclass (FunctorB, TraversableB, ProductB, ConstraintsB, ProductBC)

--deriving instance AllBF Show f Barbie => Show (Barbie f)
--deriving instance AllBF Eq   f Barbie => Eq   (Barbie f)

caToMaybe :: ConnectedAccountT Identity -> ConnectedAccountT Maybe
caToMaybe (ConnectedAccount i n a p) = ConnectedAccount (Just i) (Just n) (Just a) (Just p)

instance Scrub (ConnectedAccountT Maybe) where
  scrub ca = Scrubbed $ ca { _connectedAccount_accessToken = Nothing }

ConnectedAccount (LensFor connectedAccount_id) (LensFor connectedAccount_name)
          (LensFor connectedAccount_accessToken) (LensFor connectedAccount_provider)
          = tableLenses

type ConnectedAccount = ConnectedAccountT Identity
type ConnectedAccountId = PrimaryKey ConnectedAccountT Identity

deriving instance Eq (PrimaryKey ConnectedAccountT Identity)
deriving instance Eq (PrimaryKey ConnectedAccountT Maybe)
deriving instance Eq ConnectedAccount
deriving instance Show (PrimaryKey ConnectedAccountT Identity)
deriving instance Show (PrimaryKey ConnectedAccountT Maybe)
deriving instance Show ConnectedAccount
deriving instance Show (ConnectedAccountT Maybe)
deriving instance Default (ConnectedAccountT Maybe)

deriving instance Ord ConnectedAccount
deriving instance Ord (PrimaryKey ConnectedAccountT Identity)
deriving instance Ord (PrimaryKey ConnectedAccountT Maybe)

instance ToJSON (PrimaryKey ConnectedAccountT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey ConnectedAccountT Identity)

instance ToJSON (PrimaryKey ConnectedAccountT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey ConnectedAccountT Maybe)

instance ToJSON (ConnectedAccountT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (ConnectedAccountT Identity)

instance ToJSON (ConnectedAccountT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (ConnectedAccountT Maybe)

instance Beamable ConnectedAccountT

instance Table ConnectedAccountT where
  data PrimaryKey ConnectedAccountT f = ConnectedAccountId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = ConnectedAccountId . _connectedAccount_id

caKeyToInt :: PrimaryKey ConnectedAccountT Identity -> Int32
caKeyToInt (ConnectedAccountId k) = k

intToCaKey :: Int32 -> PrimaryKey ConnectedAccountT Identity
intToCaKey k = ConnectedAccountId k

caKeyIdToMaybe :: PrimaryKey ConnectedAccountT Identity -> PrimaryKey ConnectedAccountT Maybe
caKeyIdToMaybe (ConnectedAccountId k) = ConnectedAccountId (Just k)

caKeyMaybeToId :: PrimaryKey ConnectedAccountT Maybe -> Maybe (PrimaryKey ConnectedAccountT Identity)
caKeyMaybeToId (ConnectedAccountId (Just k)) = Just (ConnectedAccountId k)
caKeyMaybeToId (ConnectedAccountId Nothing) = Nothing
