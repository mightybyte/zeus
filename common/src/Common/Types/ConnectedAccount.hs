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
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
import           Scrub
------------------------------------------------------------------------------

data AccountProvider = GitHub | GitLab
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)

instance ToJSON AccountProvider where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AccountProvider

instance HasSqlValueSyntax be String => HasSqlValueSyntax be AccountProvider where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be AccountProvider where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be AccountProvider where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

--instance FunctorB (C f a)
--instance TraversableB (C f a)
--instance ProductB (C f a)
--instance ConstraintsB (C f a)
--instance ProductBC (C f a)

------------------------------------------------------------------------------
--data ConnectedAccountGT f = ConnectedAccountG
--  { _connectedAccount_id :: f Int
--  , _connectedAccount_name :: f Text
--  -- ^ Name of the account (or organization) that owns the repositories that
--  -- you are connecting to.  This should NOT be the username of the account
--  -- you authenticate to github with.
--  , _connectedAccount_accessToken :: f Text
--  -- ^ The access token to the account that grants you access to the repo
--  -- (even if it is different from the owner of the repo).
--  , _connectedAccount_provider :: f AccountProvider
--  } deriving (Generic)
--type ConnectedAccountT f = ConnectedAccountGT (C f)

------------------------------------------------------------------------------
data ConnectedAccountT f = ConnectedAccount
  { _connectedAccount_id :: C f Int
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
deriving instance Eq ConnectedAccount
deriving instance Show (PrimaryKey ConnectedAccountT Identity)
deriving instance Show ConnectedAccount
deriving instance Show (ConnectedAccountT Maybe)
deriving instance Default (ConnectedAccountT Maybe)

instance ToJSON (ConnectedAccountT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (ConnectedAccountT Identity)

instance ToJSON (ConnectedAccountT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (ConnectedAccountT Maybe)

instance Beamable ConnectedAccountT

instance Table ConnectedAccountT where
  data PrimaryKey ConnectedAccountT f = ConnectedAccountId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ConnectedAccountId . _connectedAccount_id
