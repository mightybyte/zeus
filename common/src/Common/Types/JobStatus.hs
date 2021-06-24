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

module Common.Types.JobStatus where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import qualified Database.Beam.AutoMigrate as BA
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
------------------------------------------------------------------------------

data JobStatus
  = JobPending
  | JobInProgress
  | JobCanceled
  | JobTimedOut
  | JobVanished
  | JobFailed
  | JobSucceeded
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)

instance BeamMigrateSqlBackend be => HasSqlEqualityCheck be JobStatus

instance HasSqlValueSyntax be String => HasSqlValueSyntax be JobStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be JobStatus where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be JobStatus where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance BA.HasColumnType JobStatus where
  defaultColumnType _ = BA.SqlStdType $ varCharType Nothing Nothing

instance ToJSON JobStatus where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JobStatus
