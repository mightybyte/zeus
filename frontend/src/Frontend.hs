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
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Data.Bool
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import           Data.Text (Text)
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Contrib.Vanishing
import           Reflex.Dom.SemanticUI
import qualified Reflex.Dom.SemanticUI as SemUI
------------------------------------------------------------------------------
import           Common.Route
import           Common.Types.ConnectedAccount
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Widgets.Common
import           Frontend.Widgets.Form
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

data ListState = EmptyPlaceholder | AddForm | ListTable
  deriving (Eq,Ord,Show,Read)

data TableAction t a = TableAction
  { tableAction_showAddForm :: Event t ()
  , tableAction_cancelAdd :: Event t ()
  }

accountsWidget :: MonadApp t m => m ()
accountsWidget = mdo
  as <- ask
  let listToState = bool ListTable EmptyPlaceholder . null
  listState <- holdDyn EmptyPlaceholder $ leftmost
    [ listToState <$> leftmost
        [ updated (_as_accounts as)
        , tag (current $ _as_accounts as) cancel]
    , AddForm <$ showForm
    ]
  let mkPair ca = (fromJust $ _connectedAccount_id ca, ca)
  let accountMap = M.fromList . map mkPair <$> _as_accounts as
  let widget s = case s of
        EmptyPlaceholder -> accountPlaceholder
        AddForm -> addAccount
        ListTable -> accountsList accountMap --(_as_accounts as)
  ee <- dyn (widget <$> listState)
  showForm <- switch <$> hold never (tableAction_showAddForm <$> ee)
  cancel <- switch <$> hold never (tableAction_cancelAdd <$> ee)
  return ()

addAccount
  :: MonadApp t m
  => m (TableAction t (ConnectedAccountT Maybe))
addAccount = do
  semuiForm $ do
    da <- newAccountForm Nothing never
    divClass "field" $ do
      (e1,_) <- elAttr' "button" ("class" =: "ui button") $ text "Connect Account"
      (e2,_) <- elAttr' "button" ("class" =: "ui button") $ text "Cancel"
      trigger trigger_connectAccount $ fmapMaybe id $ tag (current da) (domEvent Click e1)
      return $ TableAction never (domEvent Click e2)

accountsList
  :: MonadApp t m
  => Dynamic t (Map Int (ConnectedAccountT Maybe))
  -> m (TableAction t (ConnectedAccountT Maybe))
accountsList as = do
  divClass "ui segment" $ do
    elClass "h1" "ui header" $ text "Accounts"
    add <- SemUI.button def $ text "Add Account"
    let mkField f k v = el "td" $ dynText (f <$> v) >> return ()
    del <- genericRemovableTable as
      [ ("ID", mkField $ maybe "" tshow . _connectedAccount_id)
      , ("Name", mkField $ fromMaybe "" . _connectedAccount_name)
      , ("Provider", mkField $ maybe "" tshow . _connectedAccount_provider)
      --, ("", (\_ _ -> elClass "td" "right aligned collapsing" deleteButton))
      ]
    triggerBatch trigger_delAccounts $ M.keys <$> del
    return $ TableAction add never

accountPlaceholder :: MonadApp t m => m (TableAction t (ConnectedAccountT Maybe))
accountPlaceholder = mdo
  divClass "ui placeholder segment" $ do
    divClass "ui icon header" $ do
      elClass "i" "dont icon" blank
      text "You haven't connected any accounts yet"
    (e,_) <- elAttr' "div" ("class" =: "ui primary button") $ text "Connect Account"
    return $ TableAction (domEvent Click e) never

newAccountForm
  :: MonadApp t m
  => Maybe (ConnectedAccountT Maybe)
  -> Event t (Maybe (ConnectedAccountT Maybe))
  -> m (Dynamic t (Maybe (ConnectedAccountT Maybe)))
newAccountForm iv sv = do
  dn <- labelledAs "Name of the account that owns the repositories to test" $
    textField
    (fromMaybe "" $ _connectedAccount_name =<< iv)
    (fromMaybe "" . (_connectedAccount_name =<<) <$> sv)
  dat <- labelledAs "Access Token" $ textField
    (fromMaybe "" $ _connectedAccount_accessToken =<< iv)
    (fromMaybe "" . (_connectedAccount_accessToken =<<) <$> sv)
  dmp <- labelledAs "Provider" $ aForm
    (_connectedAccount_provider =<< iv)
    (fmap join $ _connectedAccount_provider <$$> sv)
  return $ do
    mp <- dmp
    case mp of
      Nothing -> pure Nothing
      Just p -> do
        n <- dn
        a <- dat
        pure $ Just $ ConnectedAccount Nothing (Just n) (Just a) mp


addClassWhen :: Monad (Dynamic t) => CssClass -> Dynamic t Bool -> CssClass -> Dynamic t CssClass
addClassWhen dynKlass dynBool staticKlass = do
  a <- dynBool
  return $ if a then dynKlass <> staticKlass else staticKlass

modal isActive m = do
  let dclass = addClassWhen (singleClass "active") isActive (manyClasses ["ui", "modal"])
  elDynKlass "div" dclass m

modalExample showEvent = mdo
  modalOpen <- toggle False (leftmost [showEvent, cancel, ok])
  (cancel, ok) <- modal modalOpen $ do
    elClass "i" "close icon" $ text " "
    divClass "header" $ text "Connect Account"
    divClass "scrolling content" $
      el "p" $ text "Very long content goes here"
    divClass "actions" $ do
      c <- SemUI.button def $ text "Cancel"
      o <- SemUI.button def $ text "Connect"
      return (c, o)
  return ()
