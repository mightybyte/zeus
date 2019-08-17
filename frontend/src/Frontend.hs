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
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Map (Map)
import           Data.Text (Text)
import           Obelisk.Configs
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.CssClass
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Nav
import           Frontend.Widgets.Accounts
import           Frontend.Widgets.Caches
import           Frontend.Widgets.Jobs
import           Frontend.Widgets.Repos
import           Frontend.Widgets.Settings
------------------------------------------------------------------------------


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = appHead
  , _frontend_body = do
      --route <- getAppRoute
      --cfgs <- getConfigs
      --runApp undefined (appBody undefined)
      runApp undefined $ do
        pb <- getPostBuild
        divClass "ui fixed menu" $ do
          elAttr "div" ("class" =: "inverted header item") $ text "Zeus CI"
          nav
        divClass "ui main container" $ do
          subRoute_ $ \case
            FR_Home -> setRoute ((FR_Jobs :/ Job_List :/ ()) <$ pb)
            FR_Jobs -> jobsWidget
            FR_Repos -> reposWidget
            FR_Accounts -> accountsWidget
            FR_Caches -> cachesWidget
            FR_Settings -> settingsWidget
        serverAlert <- asks _as_serverAlert
        modalExample serverAlert
        return ()
  }


appHead :: DomBuilder t m => m ()
appHead = do
    el "title" $ text "Zeus CI"
    elAttr "link" ("rel" =: "shortcut icon" <>
                   "href" =: "/static/favicon.svg" <>
                   "type" =: "image/svg+xml"
                  ) blank

    css (static @"semantic.min.css")
    css (static @"css/custom.css")
    --jsScript "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.3/jquery.min.js"
    jsScript (static @"jquery-3.1.1.min.js")
    jsScript (static @"semantic.min.js")

css :: DomBuilder t m => Text -> m ()
css url = elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: url) blank

jsScript :: DomBuilder t m => Text -> m ()
jsScript url = elAttr "script" ("src" =: url <> "type" =: "text/javascript") blank

script :: DomBuilder t m =>  Text -> m ()
script code = elAttr "script" ("type" =: "text/javascript") $ text code

-- TODO Remove prerender constraint after updating reflex-dom-contrib
appBody
  :: forall js t m. (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m,
      TriggerEvent t m, PerformEvent t m, MonadRef m,
      MonadSample t (Performable m), RouteToUrl (R FrontendRoute) m,
      SetRoute t (R FrontendRoute) m, MonadIO m, MonadIO (Performable m),
      Prerender js t m
     )
  => Map Text ByteString
  -> App (R FrontendRoute) t m ()
appBody cfgs = do
  pb <- getPostBuild
  divClass "ui fixed menu" $ do
    elAttr "div" ("class" =: "inverted header item") $ text "Zeus CI"
    nav
  divClass "ui main container" $ do
    subRoute_ $ \case
      FR_Home -> setRoute ((FR_Jobs :/ Job_List :/ ()) <$ pb)
      FR_Jobs -> jobsWidget
      FR_Repos -> reposWidget
      FR_Accounts -> accountsWidget
      FR_Caches -> cachesWidget
      FR_Settings -> settingsWidget
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
