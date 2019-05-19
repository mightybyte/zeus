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

module Common.Types.Repo where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate
------------------------------------------------------------------------------
import           Common.Types.ConnectedAccount
------------------------------------------------------------------------------

data CloneMethod = HttpClone | SshClone
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)

instance ToJSON CloneMethod where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CloneMethod

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CloneMethod where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be CloneMethod where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be CloneMethod where
  fromBackendRow = read . T.unpack <$> fromBackendRow

------------------------------------------------------------------------------
data RepoT f = Repo
  { _repo_id :: C f Int
  , _repo_fullName :: C f Text
  -- ^ For GitHub this is "owner/name".
  , _repo_owner :: PrimaryKey ConnectedAccountT f
  , _repo_name :: C f Text
  , _repo_cloneMethod :: C f CloneMethod
  , _repo_buildCmd :: C f Text
  , _repo_timeout :: C f Int
  -- ^ Build timeout in seconds
  } deriving Generic

repoToMaybe :: RepoT Identity -> RepoT Maybe
repoToMaybe (Repo i f (ConnectedAccountId o) n c b t) = Repo (Just i) (Just f)
    (ConnectedAccountId $ Just o) (Just n) (Just c) (Just b) (Just t)

Repo (LensFor repo_id) (LensFor repo_fullName)
     (ConnectedAccountId (LensFor repo_owner))
     (LensFor repo_name) (LensFor rep_cloneMethod) (LensFor repo_buildCmd)
     (LensFor repo_timeout) =
     tableLenses

type Repo = RepoT Identity
type RepoId = PrimaryKey RepoT Identity

deriving instance Eq (PrimaryKey RepoT Identity)
deriving instance Eq (PrimaryKey RepoT Maybe)
deriving instance Eq Repo
deriving instance Show (PrimaryKey RepoT Identity)
deriving instance Show (PrimaryKey RepoT Maybe)
deriving instance Show Repo
deriving instance Ord (PrimaryKey RepoT Identity)
deriving instance Ord (PrimaryKey RepoT Maybe)

instance ToJSON (PrimaryKey RepoT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey RepoT Identity)

instance ToJSON (PrimaryKey RepoT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey RepoT Maybe)

instance ToJSON (RepoT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (RepoT Identity)

instance ToJSON (RepoT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (RepoT Maybe)

instance Beamable RepoT

instance Table RepoT where
  data PrimaryKey RepoT f = RepoId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = RepoId . _repo_id
