{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Frontend where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Reader
import           Data.Map (Map)
import           Data.Text (Text)
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Contrib.Vanishing
import           Reflex.Dom.Contrib.Widgets.DynTabs
------------------------------------------------------------------------------
import           Common.Route
import           Humanizable
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Widgets.Accounts
import           Frontend.Widgets.Jobs
------------------------------------------------------------------------------


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = prerender_ blank appHead
  , _frontend_body = prerender_ (text "Loading...") $ runApp appBody
  }


appHead :: DomBuilder t m => m ()
appHead = do
    el "title" $ text "Zeus"
    --jsScript "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.3/jquery.min.js"
    jsScript (static @"jquery-3.1.1.min.js")
    jsScript (static @"semantic.min.js")
    css (static @"semantic.min.css")
    css (static @"css/custom.css")

css :: DomBuilder t m => Text -> m ()
css url = elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: url) blank

jsScript :: DomBuilder t m => Text -> m ()
jsScript url = elAttr "script" ("src" =: url <> "type" =: "text/javascript") blank

script :: MonadWidget t m =>  Text -> m ()
script code = elAttr "script" ("type" =: "text/javascript") $ text code

appBody :: MonadApp t m => m ()
appBody = mdo
  as <- ask
  pb <- getPostBuild
  trigger trigger_getAccounts pb
  tabs <- divClass "ui fixed menu" $ do
    elAttr "div" ("class" =: "inverted header item") $ text "Zeus"
    tabBar def
  let staticAttrs = "class" =: "ui tab"
  divClass "ui main container" $ do
    myTabPane staticAttrs (_tabBar_curTab tabs) JobsTab $ jobsWidget
    myTabPane staticAttrs (_tabBar_curTab tabs) BuildersTab $ text "Builders"
    myTabPane staticAttrs (_tabBar_curTab tabs) AccountsTab $ accountsWidget
  return ()

data MainTabs = JobsTab | BuildersTab | AccountsTab
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

instance Humanizable MainTabs where
  humanize JobsTab = "Jobs"
  humanize BuildersTab = "Builders"
  humanize AccountsTab = "Accounts"

instance MonadApp t m => Tab t m MainTabs where
  tabIndicator tab activeDyn = do
    let attrs = addClassWhen "active" activeDyn (singleClass "item")
    (e,_) <- elDynKlass' "div" attrs $ text $ humanize tab
    return $ domEvent Click e

myTabPane
    :: (MonadApp t m, Eq tab)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m a
myTabPane staticAttrs currentTab t child = do
    let attrs = addActiveClass ((==t) <$> currentTab) (constDyn staticAttrs)
    elDynAttr "div" attrs child

--modal isActive m = do
--  let dclass = addClassWhen (singleClass "active") isActive (manyClasses ["ui", "modal"])
--  elDynKlass "div" dclass m
--
--modalExample showEvent = mdo
--  modalOpen <- toggle False (leftmost [showEvent, cancel, ok])
--  (cancel, ok) <- modal modalOpen $ do
--    elClass "i" "close icon" $ text " "
--    divClass "header" $ text "Connect Account"
--    divClass "scrolling content" $
--      el "p" $ text "Very long content goes here"
--    divClass "actions" $ do
--      c <- SemUI.button def $ text "Cancel"
--      o <- SemUI.button def $ text "Connect"
--      return (c, o)
--  return ()
