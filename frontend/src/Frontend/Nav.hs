{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
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
------------------------------------------------------------------------------

-- | Displays the logo and returns an event that fires when the logo is clicked
--logo :: (DomBuilder t m, SetRoute t (R FrontendRoute) m, RouteToUrl (R FrontendRoute) m) => m ()
--logo = do
--  let logoAttrs = mconcat
--        [ "class" =: "inverted header item"
--        , "alt" =: "Zeus CI"
--        ]
--  routeLink (FR_Home :/ ()) $ elAttr "div" logoAttrs $ text "Zeus CI"

-- | Build the nav's tabs
nav
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
nav = do
  -- Get the current route, so that we can highlight the corresponding tab
  currentTab <- askRoute
  --logo
  let currentTabDemux = demux $ fmap (\(tab :=> _) -> Some.This tab) currentTab
  -- Iterate over all the top-level routes except Home
  -- Home is reached by clicking logo
  forM_ (filter (/= (Some.This FR_Home)) universe) $ \tab -> do
    let thisTabIsSelected = demuxed currentTabDemux tab
        highlight = ffor thisTabIsSelected $ \case
          True -> "class" =: "active item"
          False -> "class" =: "item"
    elDynAttr "span" highlight $ routeLink (tabHomepage tab) $ text $ tabTitle tab
