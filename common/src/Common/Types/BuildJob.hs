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
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
------------------------------------------------------------------------------
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

data RepoEvent = RepoPushEvent | RepoPullRequestEvent
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

data JobStatus
  = JobPending
  | JobInProgress
  | JobCanceled
  | JobFailed
  | JobSucceeded
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)


instance HasSqlValueSyntax be String => HasSqlValueSyntax be JobStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be JobStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be JobStatus where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance ToJSON JobStatus where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JobStatus

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be UTCTime where
  defaultSqlDataType _ _ _ = timestampType Nothing True


------------------------------------------------------------------------------
data BuildJobT f = BuildJob
  { _buildJob_id :: C f Int
  , _buildJob_repoBuildInfo :: RepoBuildInfoT f
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
deriving instance Show (PrimaryKey BuildJobT Identity)
deriving instance Show BuildJob

instance ToJSON (BuildJobT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (BuildJobT Identity)

instance Beamable BuildJobT

instance Table BuildJobT where
  data PrimaryKey BuildJobT f = BuildJobId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = BuildJobId . _buildJob_id
