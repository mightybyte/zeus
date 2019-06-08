{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

------------------------------------------------------------------------------
import           Control.Monad (forM_)
import           Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Some as Some
import           Data.Universe (universe)
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.App
import           Frontend.Common
------------------------------------------------------------------------------

nav
  :: forall t m. (MonadApp (R FrontendRoute) t m, Routed t (R FrontendRoute) m)
  => m ()
nav = do
  -- Get the current route, so that we can highlight the corresponding tab
  currentTab <- askRoute
  --logo
  let currentTabDemux = demux $ fmap (\(tab :=> _) -> Some.This tab) currentTab
  -- Iterate over all the top-level routes except Home
  -- Home is reached by clicking logo
  forM_ (filter (/= (Some.This FR_Home)) universe) $ \tab -> do
    -- Create a link that is highlighted if it is the current tab
    let thisTabIsSelected = demuxed currentTabDemux tab
        highlightAttrs = ffor thisTabIsSelected $ \case
          True -> "class" =: "active clickable item"
          False -> "class" =: "clickable item"
    internalLink (tabHomepage tab) $ \cfg ->
      elDynAttr' "span" highlightAttrs $ element "a" cfg (text $ tabTitle tab)
    return ()
