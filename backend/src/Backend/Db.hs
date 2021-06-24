{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
import           Data.Proxy
import           Data.Time
import           Database.Beam
import qualified Database.Beam.AutoMigrate as BA
import qualified Database.Beam.AutoMigrate.Annotated as BA
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite.Connection
import           Database.Beam.Postgres
import           Database.SQLite.Simple
------------------------------------------------------------------------------
import           Common.Types.BinaryCache
import           Common.Types.CacheJob
import           Common.Types.CachedHash
import           Common.Types.CiSettings
import           Common.Types.ConnectedAccount
import           Common.Types.Builder
import           Common.Types.BuildJob
import           Common.Types.JobStatus
import           Common.Types.Repo
import           Common.Types.RepoBuildInfo
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
  } deriving (Generic, Database be)

--ciDbChecked :: BeamMigrateSqlBackend be => CheckedDatabaseSettings be CiDb
--ciDbChecked = defaultMigratableDbSettings @_ @CiDb
ciDbChecked :: CheckedDatabaseSettings Sqlite CiDb
ciDbChecked = defaultMigratableDbSettings

--ciDb :: DatabaseSettings be CiDb
--ciDb :: DatabaseSettings be CiDb
--ciDb = unCheckDatabase ciDbChecked
--       `withDbModification`
--       renamingFields (snakify . T.takeWhileEnd (/= '_') . defaultFieldName)
ciDb :: DatabaseSettings be CiDb
ciDb = defaultDbSettings

CiDb (TableLens ciDb_connectedAccounts)
     (TableLens ciDb_repos)
     (TableLens ciDb_builders)
     (TableLens ciDb_buildJobs)
     (TableLens ciDb_ciSettings)
     (TableLens ciDb_cacheJobs)
     (TableLens ciDb_binaryCache)
     (TableLens ciDb_cachedHash)
     = dbLenses

--populateDb :: Connection -> IO ()
--populateDb conn = do
--  now <- getCurrentTime
----  let accounts =
----        [ ConnectedAccount default_ (val_ "mightybyte") (val_ "0000000000000000000000000000000000000000") ]
--  let rbi = RepoBuildInfo
--              "dummy" "mightybyte/dummy" RepoPush "ssh://..." "https://..." "1234"
--              "a8cd23" "Dummy commit" "Alice Coder"
--              (Just "https://secure.gravatar.com/avatar/0cece5abd2f9ad9056f5ac3830ac0bfe?s=80&d=identicon")
--      start = addUTCTime (-82) now
--  runBeamSqlite conn $ do
--    runInsert $ insert (_ciDb_buildJobs ciDb) $ insertExpressions
--      [ BuildJob default_ (val_ rbi) (val_ start) (val_ $ Just start) (val_ $ Just now) (val_ JobSucceeded)
--      ]

annotatedDb :: BA.AnnotatedDatabaseSettings Sqlite CiDb
annotatedDb = BA.defaultAnnotatedDbSettings ciDb

hsSchema :: BA.Schema
hsSchema = BA.fromAnnotatedDbSettings annotatedDb (Proxy @'[])

