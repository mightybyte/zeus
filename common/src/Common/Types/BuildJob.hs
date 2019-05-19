{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

module Common.Types.BuildJob where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
------------------------------------------------------------------------------
import           Common.Types.JobStatus
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

data RepoEvent = RepoPushEvent | RepoPullRequestEvent
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be UTCTime where
  defaultSqlDataType _ _ _ = timestampType Nothing True


------------------------------------------------------------------------------
data BuildJobT f = BuildJob
  { _buildJob_id :: C f Int
  , _buildJob_repoBuildInfo :: RepoBuildInfoT f
  -- ^ Denormalizing this and putting it inline instead of using a foreign key
  -- to the repos table allows us to delete repos without violating foreign
  -- key constraints here.
  , _buildJob_receivedAt :: C f UTCTime
  , _buildJob_startedAt :: C f (Maybe UTCTime)
  , _buildJob_endedAt :: C f (Maybe UTCTime)
  , _buildJob_status :: C f JobStatus
  } deriving Generic

makeLenses 'BuildJob

type BuildJob = BuildJobT Identity
type BuildJobId = PrimaryKey BuildJobT Identity

deriving instance Eq (PrimaryKey BuildJobT Identity)
deriving instance Eq BuildJob
deriving instance Ord (PrimaryKey BuildJobT Identity)
deriving instance Ord BuildJob
deriving instance Show (PrimaryKey BuildJobT Identity)
deriving instance Show BuildJob

instance ToJSON (PrimaryKey BuildJobT Identity) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey BuildJobT Identity)

instance ToJSON (BuildJobT Identity) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (BuildJobT Identity)

bjKeyToInt :: PrimaryKey BuildJobT Identity -> Int
bjKeyToInt (BuildJobId k) = k

bjKeyIdToMaybe :: PrimaryKey BuildJobT Identity -> PrimaryKey BuildJobT Maybe
bjKeyIdToMaybe (BuildJobId k) = BuildJobId (Just k)

bjKeyMaybeToId :: PrimaryKey BuildJobT Maybe -> Maybe (PrimaryKey BuildJobT Identity)
bjKeyMaybeToId (BuildJobId (Just k)) = Just (BuildJobId k)
bjKeyMaybeToId (BuildJobId Nothing) = Nothing

instance Beamable BuildJobT

instance Table BuildJobT where
  data PrimaryKey BuildJobT f = BuildJobId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = BuildJobId . _buildJob_id
