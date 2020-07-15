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

module Common.Types.Builder where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Int
import           Data.Readable
import           Data.Text (Text)
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate
------------------------------------------------------------------------------

------------------------------------------------------------------------------

data Platform = X86_64_Darwin | X86_64_Linux | I686_Linux
  deriving (Eq,Ord,Enum,Bounded)

instance Show Platform where
  show X86_64_Darwin = "x86_64-darwin"
  show X86_64_Linux = "x86_64-linux"
  show I686_Linux = "i686-linux"

instance Readable Platform where
  fromText "x86_64-darwin" = return X86_64_Darwin
  fromText "x86_64-linux" = return X86_64_Linux
  fromText "i686-linux" = return I686_Linux
  fromText _ = mzero

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Platform where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Platform where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Platform where
  fromBackendRow = maybe (error "BeamRowParseError") id . fromText <$> fromBackendRow

data BuilderT f = Builder
  { _builder_id :: C f Int32
  , _builder_ip :: C f Text
  , _builder_platform :: C f Platform
  , _builder_maxBuilds :: C f Int32
  , _builder_speedFactor :: C f Int32
  } deriving Generic

type Builder = BuilderT Identity
type BuilderId = PrimaryKey BuilderT Identity

deriving instance Eq (PrimaryKey BuilderT Identity)
deriving instance Eq Builder
deriving instance Show (PrimaryKey BuilderT Identity)
deriving instance Show Builder

instance Beamable BuilderT

instance Table BuilderT where
  data PrimaryKey BuilderT f = BuilderId (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = BuilderId . _builder_id
