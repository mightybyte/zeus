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
--import           Cases
--import qualified Data.Text as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.Simple
import           Database.Beam.Sqlite.Connection
import           Database.SQLite.Simple
------------------------------------------------------------------------------
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
     = dbLenses

populateDb :: Connection -> IO ()
populateDb conn = do
  now <- getCurrentTime
--  let accounts =
--        [ ConnectedAccount default_ (val_ "mightybyte") (val_ "0000000000000000000000000000000000000000") ]
  let rbi = RepoBuildInfo
              "dummy" "mightybyte/dummy" RepoPush "ssh://..." "https://..." "1234"
              "a8cd23" "Dummy commit" "Alice Coder"
              (Just "https://secure.gravatar.com/avatar/0cece5abd2f9ad9056f5ac3830ac0bfe?s=80&d=identicon")
      start = addUTCTime (-82) now
  runBeamSqlite conn $ do
    runInsert $ insert (_ciDb_buildJobs ciDb) $ insertExpressions
      [ BuildJob default_ (val_ rbi) (val_ start) (val_ $ Just start) (val_ $ Just now) (val_ JobSucceeded)
      ]

{-
Migration for removing clone method column

ALTER TABLE "ciDb_repos" RENAME TO "ciDb_repos_old_0";
CREATE TABLE IF NOT EXISTS "ciDb_repos"("repo_id" INTEGER NOT NULL , "repo_accessAccount__connectedAccount_id" INTEGER NOT NULL , "repo_name" VARCHAR NOT NULL , "repo_namespace" VARCHAR NOT NULL , "repo_buildNixFile" VARCHAR NOT NULL , "repo_timeout" INTEGER NOT NULL , "repo_hookId" INTEGER NOT NULL , PRIMARY KEY("repo_id"));
INSERT INTO "ciDb_repos" SELECT "repo_id", "repo_accessAccount__connectedAccount_id", "repo_name", "repo_namespace", "repo_buildNixFile", "repo_timeout", "repo_hookId" FROM "ciDb_repos_old_0";
DROP TABLE ciDb_repos_old_0;
-}
