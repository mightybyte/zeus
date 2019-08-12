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
--import           Data.Universe (universe)
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.App
import           Frontend.Common
------------------------------------------------------------------------------

leftMenuItems :: [Some.Some FrontendRoute]
leftMenuItems =
  [ Some.This FR_Jobs
  , Some.This FR_Repos
  , Some.This FR_Accounts
  , Some.This FR_Caches
  ]

nav
  :: forall t m. (MonadApp (R FrontendRoute) t m, Routed t (R FrontendRoute) m)
  => m ()
nav = do
  -- Get the current route, so that we can highlight the corresponding tab
  currentTab <- askRoute

  -- Iterate over all the top-level routes except Home
  -- Home is reached by clicking logo
  forM_ leftMenuItems $ menuItem currentTab

  divClass "right menu" $ do
    menuItem currentTab (Some.This FR_Settings)
    -- _ <- elClass "span" "clickable item" $ element "a" def (text "Logout")
    return ()


-- Create a link that is highlighted if it is the current tab
menuItem
  :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m, PostBuild t m)
  => Dynamic t (DSum FrontendRoute f)
  -> Some.Some FrontendRoute
  -> m ()
menuItem currentTab tab = do
    let currentTabDemux = demux $ fmap (\(t :=> _) -> Some.This t) currentTab
    let thisTabIsSelected = demuxed currentTabDemux tab
        highlightAttrs = ffor thisTabIsSelected $ \case
          True -> "class" =: "active clickable item"
          False -> "class" =: "clickable item"
    internalLink (tabHomepage tab) $ \cfg ->
      elDynAttr' "span" highlightAttrs $ element "a" cfg (tabTitle tab)
    return ()
