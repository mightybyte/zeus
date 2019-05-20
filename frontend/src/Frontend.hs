{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Dependent.Sum
import           Data.Map (Map)
import           Data.Maybe
import           Data.Some (Some)
import qualified Data.Some as Some
import           Data.Text (Text)
import           Data.Universe
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
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
  { _frontend_head = appHead
  , _frontend_body = do
      route <- liftIO getAppRoute
      runApp route appBody
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

script :: DomBuilder t m =>  Text -> m ()
script code = elAttr "script" ("type" =: "text/javascript") $ text code

-- TODO Remove prerender constraint after updating reflex-dom-contrib
appBody
  :: MonadApp r t m
  => m ()
appBody = do
  pb <- getPostBuild
  trigger trigger_getAccounts pb
  trigger trigger_getJobs pb
  trigger trigger_getRepos pb
  divClass "ui fixed menu" $ do
    elAttr "div" ("class" =: "inverted header item") $ text "Zeus CI"
    let aTab nm = elAttr "div" ("class" =: "inverted item") $ text $ humanize nm
    mapM_ aTab [JobsTab, ReposTab, AccountsTab]
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

--navItemIndicator
--  :: MonadApp r t m
--  => Dynamic t (R FrontendRoute)
--  -> FrontendRoute
--  -> m ()
--navItemIndicator activeTab tab = do
--  let attrs = addClassWhen "active" ((\(t :=> _) -> t == tab) <$> activeTab) (singleClass "clickable item")
--  (e,_) <- elDynKlass' "div" attrs $ text $ humanize tab
--  trigger trigger_getJobs pb
--  return $ domEvent Click e

modal
  :: MonadApp r t m
  => Dynamic t Bool
  -> m a
  -> m a
modal isActive m = do
  let dclass = addClassWhen (singleClass "active") isActive (manyClasses ["ui", "modal"])
  elDynKlass "div" dclass m

modalExample
  :: MonadApp r t m
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
      (e,_) <- el' "button" $ text "OK"
      return $ domEvent Click e
      -- TODO Deal with this
      --SemUI.button def $ text "OK"
  return ()
