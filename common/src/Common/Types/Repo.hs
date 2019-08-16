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
import           Database.Beam
------------------------------------------------------------------------------
import           Common.Types.BinaryCache
import           Common.Types.ConnectedAccount
------------------------------------------------------------------------------


------------------------------------------------------------------------------
data RepoT f = Repo
  { _repo_id :: C f Int
  -- ^ For GitHub this is "owner/name".
  , _repo_accessAccount :: PrimaryKey ConnectedAccountT f
  , _repo_name :: C f Text
  , _repo_namespace :: C f Text
  -- ^ With GitHub repos this is always the repository name.  With gitlab it
  -- can be a deeper nested path of groups /foo/bar/baz/repo
  , _repo_buildNixFile :: C f Text
  , _repo_timeout :: C f Int
  -- ^ Build timeout in seconds
  , _repo_cache :: PrimaryKey BinaryCacheT (Nullable f)
  , _repo_hookId :: C f Int
  -- ^ Allows us to delete the webhook
  } deriving Generic

repoFullName :: Repo -> Text
repoFullName r = _repo_namespace r <> "/" <> _repo_name r

repoToMaybe :: RepoT Identity -> RepoT Maybe
repoToMaybe (Repo i (ConnectedAccountId o) on rn bf t (BinaryCacheId c) h) = Repo (Just i)
    (ConnectedAccountId $ Just o) (Just on) (Just rn) (Just bf) (Just t) (BinaryCacheId $ Just c) (Just h)
--  where
--    f (BinaryCacheId i) = BinaryCacheId $ Just i

Repo
  (LensFor repo_id)
  (ConnectedAccountId (LensFor repo_accessAccount))
  (LensFor repo_name)
  (LensFor repo_namespace)
  (LensFor repo_buildNixFile)
  (LensFor repo_timeout)
  (BinaryCacheId (LensFor repo_cache))
  (LensFor repo_hookId)
  = tableLenses

type Repo = RepoT Identity
type RepoId = PrimaryKey RepoT Identity

deriving instance Eq (PrimaryKey RepoT Identity)
deriving instance Eq (PrimaryKey RepoT Maybe)
deriving instance Eq Repo
deriving instance Show (PrimaryKey RepoT Identity)
deriving instance Show (PrimaryKey RepoT Maybe)
deriving instance Show Repo
deriving instance Show (RepoT Maybe)
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

repoKeyToInt :: PrimaryKey RepoT Identity -> Int
repoKeyToInt (RepoId k) = k
