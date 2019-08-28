{-# LANGUAGE OverloadedStrings #-}

module Nix.Utils where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson.Lens
import           System.Process
------------------------------------------------------------------------------
import           Backend.ExecutablePaths
import           Common.Types.Platform
------------------------------------------------------------------------------

getNixSystem :: IO (Maybe Platform)
getNixSystem = do
    jsonCfg <- readProcess nixBinary ["show-config", "--json"] ""
    return $ jsonCfg ^? key "system" . key "value" . _String . _Platform
