{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Widgets.Settings where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified GHCJS.DOM.Types as DOM
import           Language.Javascript.JSaddle (MonadJSM)
import           Obelisk.Configs
import           Reflex.Dom
import           Reflex.Network
------------------------------------------------------------------------------
import           Common.Types.CiSettings
import           Frontend.App
import           Frontend.AppState
import           Frontend.Widgets.Common
import           Frontend.Widgets.Form
------------------------------------------------------------------------------

settingsWidget
  :: (MonadApp r t m, Prerender js t m, HasConfigs m)
  => m ()
settingsWidget = do
  pb <- getPostBuild
  trigger trigger_getCiSettings pb
  trigger trigger_getCiInfo pb
  el "h1" $ text "Settings"
  dynSettingsForm
  return ()

dynSettingsForm
  :: (MonadApp r t m, Prerender js t m, HasConfigs m)
  => m ()
dynSettingsForm = do
  dcs <- asks _as_ciSettings
  dci <- asks _as_ciInfo
  _ <- networkView $ ffor ((,) <$> dcs <*> dci) $ \pair -> do
    case pair of
      (Just cs, Just ci) -> do
        semuiForm $ do
          dcsNew <- settingsForm ci cs never
          let mkAttrs cs2 csNew =
                if cs2 == Just csNew
                  then ("class" =: "ui button disabled")
                  else ("class" =: "ui button")
          (e,_) <- divClass "field" $ elDynAttr' "button" (mkAttrs <$> dcs <*> dcsNew) $
            text "Update Settings"
          trigger trigger_updateCiSettings $ tag (current dcsNew) (domEvent Click e)
          return ()
      _ -> genericLoading
  return ()

settingsForm
  :: (MonadApp r t m, Prerender js t m, HasConfigs m)
  => Text
  -> CiSettings
  -> Event t CiSettings
  -> m (Dynamic t CiSettings)
settingsForm ciInfo iv sv = do
    dnp <- divClass "field" $ do
      el "label" $ text "Nix Path"
      ie <- inputElement $ def
        & inputElementConfig_initialValue .~ _ciSettings_nixPath iv
        & inputElementConfig_setValue .~ (_ciSettings_nixPath <$> sv)
      return $ value ie

    serveLocalCache <- divClass "field" $ do
      divClass "ui checkbox" $ do
        v <- checkbox (_ciSettings_serveLocalCache iv) $ def
          & setValue .~ (_ciSettings_serveLocalCache <$> sv)
        el "label" $ text "Serve Local Cache"
        return $ value v
    dynInfoWidget ciInfo serveLocalCache
    return (CiSettings 1 <$> dnp <*> serveLocalCache)

dynInfoWidget
  :: (MonadApp r t m, Prerender js t m, HasConfigs m)
  => Text
  -> Dynamic t Bool
  -> m ()
dynInfoWidget ciInfo serveLocalCache = do
  _ <- networkView $ ffor serveLocalCache $ infoWidget ciInfo
  return ()

infoWidget
  :: (MonadApp r t m, Prerender js t m, HasConfigs m)
  => Text
  -> Bool
  -> m ()
infoWidget pubkey True = divClass "ui segment" $ do
  mRootRoute <- getConfig "common/route"
  case mRootRoute of
    Nothing -> text "Can't find server address.  Server not configured properly."
    Just rootRoute -> do
      let route = T.strip (decodeUtf8 rootRoute) <> "/cache/"
      copyableValue "Cache Address" route
      copyableValue "Cache Public Key" pubkey
      el "h4" $ text "To use this cache, put the following in your /etc/nix/nix.conf:"
      elClass "pre" "ui segment" $ do
        text $ nixConfExample route pubkey
infoWidget _ False = blank

nixConfExample :: Text -> Text -> Text
nixConfExample addr pubkey = T.unlines
  [ "substituters = " <> addr <> " https://cache.nixos.org/"
  , "trusted-public-keys = " <> pubkey <> " cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
  ]

copyableValue
  :: (MonadApp r t m, Prerender js t m)
  => Text
  -> Text
  -> m ()
copyableValue label val = do
  el "h4" $ text label
  el "div" $ mdo
    -- _ <- prerender blank $ void $ liftJS $ copyButton (_element_raw e)
    (e,_) <- el' "span" $ text val
    return ()
