{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Widgets.Settings where

------------------------------------------------------------------------------
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.App
------------------------------------------------------------------------------

settingsWidget
  :: MonadApp r t m
  => m ()
settingsWidget = do
  text "Settings Widget"
