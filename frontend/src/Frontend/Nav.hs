{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Nav (nav) where

------------------------------------------------------------------------------
import           Control.Lens hiding (element, universe)
import           Control.Monad (forM_)
import           Data.Dependent.Sum (DSum ((:=>)))
import           Data.Proxy
import qualified Data.Some as Some
import           Data.Universe (universe)
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.App
------------------------------------------------------------------------------

nav
  :: forall t m. MonadApp (R FrontendRoute) t m
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
        highlight = ffor thisTabIsSelected $ \case
          True -> "class" =: "active clickable item"
          False -> "class" =: "clickable item"
        r = tabHomepage tab
    enc <- askRouteToUrl
    let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
          & elementConfig_initialAttributes .~ "href" =: enc r
    (e, a) <- elDynAttr' "span" highlight $ fmap snd $ element "a" cfg $ text $ tabTitle tab
    setRoute $ r <$ domEvent Click e
    return a
