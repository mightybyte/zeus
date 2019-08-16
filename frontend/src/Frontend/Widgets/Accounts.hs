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

module Frontend.Widgets.Accounts where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Maybe
import           Database.Beam
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
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

accountsWidget
  :: (MonadAppIO r t m, Prerender js t m)
  => RoutedT t (R CrudRoute) m ()
accountsWidget = mdo
  pb <- getPostBuild
  trigger trigger_getAccounts pb
  as <- ask
  subRoute_ $ \case
    Crud_List -> accountsList (_as_accounts as)
    Crud_Create -> addAccount
  return ()

addAccount
  :: (MonadApp r t m, Prerender js t m)
  => m ()
addAccount = do
  semuiForm $ do
    da <- newAccountForm Nothing never
    divClass "field" $ do
      (e1,_) <- elAttr' "button" ("class" =: "ui button") $ text "Connect Account"
      (e2,_) <- elAttr' "button" ("class" =: "ui button") $ text "Cancel"
      trigger trigger_connectAccount $ fmapMaybe id $ tag (current da) (domEvent Click e1)
      setRoute $ (FR_Accounts :/ Crud_List :/ ()) <$ leftmost
        [domEvent Click e1, domEvent Click e2]
      return ()

accountsList
  :: MonadAppIO r t m
  => Dynamic t (BeamMap Identity ConnectedAccountT)
  -> m ()
accountsList as = do
    let mkField f _ v = el "td" $ dynText (f <$> v) >> return never
        widget accountMap =
          if M.null accountMap
            then accountPlaceholder
            else do
              add <- SemUI.button def $ text "Add Account"
              setRoute $ (FR_Accounts :/ Crud_Create :/ ()) <$ add
              del <- genericTableG def (constDyn accountMap)
                [ ("ID", mkField $ tshow . _connectedAccount_id)
                , ("Name", mkField $ _connectedAccount_name)
                , ("Provider", mkField $ tshow . _connectedAccount_provider)
                , ("", (\k _ -> deleteColumn trigger_delAccounts k))
                ]
              triggerBatch trigger_delAccounts $ M.keys <$> del
    _ <- dyn (widget <$> as)
    return ()

accountPlaceholder :: MonadApp r t m => m ()
accountPlaceholder = mdo
  divClass "ui placeholder segment" $ do
    divClass "ui icon header" $ do
      elClass "i" "dont icon" blank
      text "You haven't connected any accounts yet"
    (e,_) <- elAttr' "div" ("class" =: "ui primary button") $ text "Connect Account"
    setRoute $ (FR_Accounts :/ Crud_Create :/ ()) <$ domEvent Click e

newAccountForm
  :: (MonadApp r t m, Prerender js t m)
  => Maybe (ConnectedAccountT Maybe)
  -> Event t (Maybe (ConnectedAccountT Maybe))
  -> m (Dynamic t (Maybe (ConnectedAccountT Maybe)))
newAccountForm iv sv = do
  dn <- labelledAs "Name of the account that you want to use to access the repositories to test" $
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
