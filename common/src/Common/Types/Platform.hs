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

module Common.Types.Platform where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Readable
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate
------------------------------------------------------------------------------

data Platform = X86_64_Darwin | X86_64_Linux | I686_Linux
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)

instance BeamMigrateSqlBackend be => HasSqlEqualityCheck be Platform

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Platform where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Platform where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Platform where
  fromBackendRow = read . T.unpack <$> fromBackendRow

platformText :: Platform -> Text
platformText X86_64_Darwin = "x86_64-darwin"
platformText X86_64_Linux = "x86_64-linux"
platformText I686_Linux = "i686-linux"

instance Readable Platform where
  fromText "x86_64-darwin" = return X86_64_Darwin
  fromText "x86_64-linux" = return X86_64_Linux
  fromText "i686-linux" = return I686_Linux
  fromText _ = mzero

_Platform :: Prism' Text Platform
_Platform = prism' platformText fromText

instance ToJSON Platform where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Platform
