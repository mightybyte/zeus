{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.Db where

------------------------------------------------------------------------------
import           Database.Beam
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite.Connection
------------------------------------------------------------------------------
import           Common.Types.BinaryCache
import           Common.Types.CacheJob
import           Common.Types.CachedHash
import           Common.Types.CiSettings
import           Common.Types.ConnectedAccount
import           Common.Types.Builder
import           Common.Types.BuildJob
import           Common.Types.Repo
import           Common.Types.ZeusMsg
------------------------------------------------------------------------------

------------------------------------------------------------------------------
data CiDb f = CiDb
  { _ciDb_connectedAccounts :: f (TableEntity ConnectedAccountT)
  , _ciDb_repos :: f (TableEntity RepoT)
  , _ciDb_builders :: f (TableEntity BuilderT)
  , _ciDb_buildJobs :: f (TableEntity BuildJobT)
  , _ciDb_ciSettings :: f (TableEntity CiSettingsT)
  , _ciDb_cacheJobs :: f (TableEntity CacheJobT)
  , _ciDb_binaryCaches :: f (TableEntity BinaryCacheT)
  , _ciDb_cachedHashes :: f (TableEntity CachedHashT)
  , _ciDb_zeusMsgs :: f (TableEntity ZeusMsgT)
  } deriving (Generic, Database be)

--ciDbChecked :: BeamMigrateSqlBackend be => CheckedDatabaseSettings be CiDb
--ciDbChecked = defaultMigratableDbSettings @_ @CiDb
ciDbChecked :: CheckedDatabaseSettings Sqlite CiDb
ciDbChecked = defaultMigratableDbSettings

--ciDb :: DatabaseSettings be CiDb
ciDb :: DatabaseSettings Sqlite CiDb
ciDb = unCheckDatabase ciDbChecked
--       `withDbModification`
--       renamingFields (snakify . T.takeWhileEnd (/= '_') . defaultFieldName)

CiDb (TableLens ciDb_connectedAccounts)
     (TableLens ciDb_repos)
     (TableLens ciDb_builders)
     (TableLens ciDb_buildJobs)
     (TableLens ciDb_ciSettings)
     (TableLens ciDb_cacheJobs)
     (TableLens ciDb_binaryCache)
     (TableLens ciDb_cachedHash)
     (TableLens ciDb_zeusMsgs)
     = dbLenses

