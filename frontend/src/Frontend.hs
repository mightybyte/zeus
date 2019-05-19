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
import           Control.Monad.Reader
import           Data.Map (Map)
import           Data.Maybe
import           Data.Text (Text)
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Contrib.Widgets.DynTabs
import qualified Reflex.Dom.SemanticUI as SemUI
------------------------------------------------------------------------------
import           Common.Route
import           Humanizable
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Widgets.Accounts
import           Frontend.Widgets.Jobs
import           Frontend.Widgets.Repos
------------------------------------------------------------------------------


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = prerender_ blank appHead
  , _frontend_body = prerender_ (text "Loading...") $ runApp appBody
  }


appHead :: DomBuilder t m => m ()
appHead = do
    el "title" $ text "Zeus CI"
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
  pb <- getPostBuild
  trigger trigger_getAccounts pb
  trigger trigger_getJobs pb
  trigger trigger_getRepos pb
  tabs <- divClass "ui fixed menu" $ do
    elAttr "div" ("class" =: "inverted header item") $ text "Zeus CI"
    tabBar def
  let staticAttrs = "class" =: "ui tab"
  divClass "ui main container" $ do
    myTabPane staticAttrs (_tabBar_curTab tabs) JobsTab $ jobsWidget
    --myTabPane staticAttrs (_tabBar_curTab tabs) BuildersTab $ text "Builders"
    myTabPane staticAttrs (_tabBar_curTab tabs) ReposTab $ reposWidget
    myTabPane staticAttrs (_tabBar_curTab tabs) AccountsTab $ accountsWidget
  serverAlert <- asks _as_serverAlert
  modalExample serverAlert
  return ()

data MainTabs
  = JobsTab
  -- | BuildersTab
  | ReposTab
  | AccountsTab
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

instance Humanizable MainTabs where
  humanize JobsTab = "Jobs"
  --humanize BuildersTab = "Builders"
  humanize ReposTab = "Repos"
  humanize AccountsTab = "Accounts"

instance MonadApp t m => Tab t m MainTabs where
  tabIndicator tab activeDyn = do
    let attrs = addClassWhen "active" activeDyn (singleClass "item")
    (e,_) <- elDynKlass' "div" attrs $ text $ humanize tab
    return $ domEvent Click e

myTabPane
    :: (MonadWidget t m, Eq tab)
    => Map Text Text
    -> Dynamic t tab
    -> tab
    -> m a
    -> m a
myTabPane staticAttrs currentTab t child = do
    let attrs = addActiveClass ((==t) <$> currentTab) (constDyn staticAttrs)
    elDynAttr "div" attrs child

modal
  :: MonadWidget t m
  => Dynamic t Bool
  -> m a
  -> m a
modal isActive m = do
  let dclass = addClassWhen (singleClass "active") isActive (manyClasses ["ui", "modal"])
  elDynKlass "div" dclass m

modalExample
  :: MonadWidget t m
  => Event t Text
  -> m ()
modalExample showEvent = mdo
  modalMsg <- holdDyn Nothing $ leftmost
    [ Just <$> showEvent
    , Nothing <$ ok
    ]
  ok <- modal (isJust <$> modalMsg) $ do
    elClass "i" "close icon" $ text " "
    divClass "header" $ text "Alert from Server"
    divClass "scrolling content" $
      el "p" $ dynText (fromMaybe "" <$> modalMsg)
    divClass "actions" $ do
      SemUI.button def $ text "OK"
  return ()
