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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Frontend where

------------------------------------------------------------------------------
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Types as DOM
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Network
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Contrib.Widgets.DynTabs
import qualified Reflex.Dom.SemanticUI as SemUI
------------------------------------------------------------------------------
import           Common.Route
import           Humanizable
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Nav
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

--pr :: PostBuild t n => m a -> n a -> m (Dynamic t a)
--pr a = prerender a a

-- TODO Remove prerender constraint after updating reflex-dom-contrib
appBody
  :: forall js t m. (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m,
      TriggerEvent t m, PerformEvent t m, MonadRef m,
      MonadSample t (Performable m), RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m, MonadIO m, MonadIO (Performable m),
      Prerender js t m
     )
  => App (R FrontendRoute) t m ()
appBody = do
  --dpb <- prerender (pure never) (getPostBuild)
  --let pb = switch $ current pb
  --pb <- switch . current <$> prerender (DOM.liftJSM getPostBuild) getPostBuild
  pb <- getPostBuild
  --performEvent_ (liftIO (putStrLn "appBody postBuild") <$ pb)
  --trigger trigger_getAccounts $ traceEvent "---postbuild---" pb
  --trigger trigger_getJobs pb
  --trigger trigger_getRepos pb
  divClass "ui fixed menu" $ do
    elAttr "div" ("class" =: "inverted header item") $ text "Zeus CI"
    nav
  divClass "ui main container" $ do
    subRoute_ $ \case
      FR_Home -> text "Wizard" >> setRoute ((FR_Accounts :/ Crud_List :/ ()) <$ pb)
      FR_Jobs -> jobsWidget
      FR_Repos -> reposWidget
      FR_Accounts -> accountsWidget
  serverAlert <- asks _as_serverAlert
  modalExample serverAlert
  return ()

--wizard :: (MonadApp r t m, SetRoute t (R FrontendRoute) m) => m ()
--wizard = do
--  repos <- asks _as_repos
--  accounts <- asks _as_accounts
--  pb <- getPostBuild
--  let action = ffor ((,) <$> accounts <*> repos) $ \(as,rs) -> return $
--        if M.null as
--          then setRoute $ (FR_Accounts :/ ()) <$ pb
--          else if M.null rs
--                 then setRoute $ (FR_Repos :/ ()) <$ pb
--                 else setRoute $ (FR_Jobs :/ ()) <$ pb
--  _ <- networkView action
--  return ()

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
