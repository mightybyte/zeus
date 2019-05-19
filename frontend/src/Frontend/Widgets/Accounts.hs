{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Widgets.Accounts where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import           Data.Bool
import qualified Data.Map as M
import           Data.Maybe
import           Database.Beam
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
import qualified Reflex.Dom.SemanticUI as SemUI
------------------------------------------------------------------------------
import           Common.Types.ConnectedAccount
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Widgets.Common
import           Frontend.Widgets.Form
------------------------------------------------------------------------------

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
  let widget s = case s of
        EmptyPlaceholder -> accountPlaceholder
        AddForm -> addAccount
        ListTable -> accountsList (_as_accounts as)
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
  => Dynamic t (BeamMap Identity ConnectedAccountT)
  -> m (TableAction t (ConnectedAccountT Maybe))
accountsList as = do
    add <- SemUI.button def $ text "Add Account"
    let mkField f _ v = el "td" $ dynText (f <$> v) >> return ()
    del <- genericRemovableTable as
      [ ("ID", mkField $ tshow . _connectedAccount_id)
      , ("Name", mkField $ _connectedAccount_name)
      , ("Provider", mkField $ tshow . _connectedAccount_provider)
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
  dp <- labelledAs "Provider" $ filledDropdown
    (fromMaybe GitHub $ _connectedAccount_provider =<< iv)
    (fmapMaybe id $ fmap join $ _connectedAccount_provider <$$> sv)
  return $ do
    n <- dn
    a <- dat
    p <- dp
    pure $ Just $ ConnectedAccount Nothing (Just n) (Just a) (Just p)
