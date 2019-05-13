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
module Frontend where

------------------------------------------------------------------------------
import           Data.Text (Text)
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.App
import           Frontend.AppState
import           Frontend.Widgets.Accounts
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
  pb <- getPostBuild
  trigger trigger_getAccounts pb
  zeusNav
  divClass "ui main container" appWidget
  return ()

zeusNav :: MonadApp t m => m ()
zeusNav = do
    divClass "ui fixed inverted menu" $ do
      elAttr "a" ("class" =: "header item") $ text "Zeus"
      elAttr "a" ("class" =: "item") $ text "Jobs"
      elAttr "a" ("class" =: "item") $ text "Accounts"
      --divClass "right menu" $
      --  elAttr "a" ("href" =: "/logout" <> "class" =: "item") $ text "Logout"

appWidget :: MonadApp t m => m ()
appWidget = do
  accountsWidget

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
