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
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate
------------------------------------------------------------------------------
import           Common.Types.ConnectedAccount
------------------------------------------------------------------------------

data CloneMethod = SshClone | HttpClone
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be CloneMethod where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be CloneMethod where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be CloneMethod where
  fromBackendRow = read . T.unpack <$> fromBackendRow

------------------------------------------------------------------------------
data RepoT f = Repo
  { _repo_fullName :: C f Text
  , _repo_owner :: PrimaryKey ConnectedAccountT f
  , _repo_name :: C f Text
  , _repo_cloneMethod :: C f CloneMethod
  , _repo_buildCmd :: C f Text
  } deriving Generic

Repo (LensFor repo_fullName) (ConnectedAccountId (LensFor repo_owner))
     (LensFor repo_name) (LensFor rep_cloneMethod) (LensFor repo_buildCmd) =
     tableLenses

type Repo = RepoT Identity
type RepoId = PrimaryKey RepoT Identity

deriving instance Eq (PrimaryKey RepoT Identity)
deriving instance Eq Repo
deriving instance Show (PrimaryKey RepoT Identity)
deriving instance Show Repo

instance Beamable RepoT

instance Table RepoT where
  data PrimaryKey RepoT f = RepoId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = RepoId . _repo_fullName
