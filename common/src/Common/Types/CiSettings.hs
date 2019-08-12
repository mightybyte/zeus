{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Types.CiSettings where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import           Database.Beam
------------------------------------------------------------------------------

------------------------------------------------------------------------------
data CiSettingsT f = CiSettings
  { _ciSettings_id :: C f Int
  , _ciSettings_nixPath :: C f Text
  , _ciSettings_serveLocalCache :: C f Bool
  } deriving (Generic)

CiSettings
  (LensFor ciSettings_id)
  (LensFor ciSettings_nixPath)
  (LensFor ciSettings_serveLocalCache)
  = tableLenses

type CiSettings = CiSettingsT Identity
type CiSettingsId = PrimaryKey CiSettingsT Identity

deriving instance Eq (PrimaryKey CiSettingsT Identity)
deriving instance Eq CiSettings
deriving instance Ord (PrimaryKey CiSettingsT Identity)
deriving instance Ord CiSettings
deriving instance Show (PrimaryKey CiSettingsT Identity)
deriving instance Show CiSettings

instance ToJSON (PrimaryKey CiSettingsT Identity) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (PrimaryKey CiSettingsT Identity)

instance ToJSON CiSettings where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CiSettings

instance Beamable CiSettingsT

instance Table CiSettingsT where
  data PrimaryKey CiSettingsT f = CiSettingsId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = CiSettingsId . _ciSettings_id
