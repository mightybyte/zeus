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
  let rbi = RepoBuildInfo "dummy" "mightybyte/dummy" RepoPush "ssh://..." "https://..." "1234" "a8cd23"
      start = addUTCTime (-82) now
  runBeamSqliteDebug putStrLn conn $ do
    runInsert $ insert (_ciDb_buildJobs ciDb) $ insertExpressions
      [ BuildJob default_ (val_ rbi) (val_ start) (val_ $ Just start) (val_ $ Just now) (val_ JobSucceeded)
      ]
--    [mb] <- runInsertReturningList $ insert (_ciDb_accounts ciDb) $ insertExpressions accounts
--    let repos =
--          [ Repo "mightybyte/test-project" (primaryKey mb) "test-project" SshClone "nix-build" ]
--        builders =
--          [ Builder default_ (val_ "192.168.0.103") (val_ X86_64_Darwin) (val_ 1) (val_ 1)
--          , Builder default_ (val_ "192.168.128.76") (val_ X86_64_Darwin) (val_ 2) (val_ 4)
--          , Builder default_ (val_ "95.216.75.94") (val_ X86_64_Linux) (val_ 2) (val_ 2)
--          ]
--    runInsert $ insert (_ciDb_repos ciDb) $ insertValues repos
--    runInsert $ insert (_ciDb_builders ciDb) $ insertExpressions builders
--    return ()
