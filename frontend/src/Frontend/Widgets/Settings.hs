{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Widgets.Settings where

------------------------------------------------------------------------------
import           Control.Monad.Reader
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
  :: MonadAppIO r t m
  => m ()
settingsWidget = do
  pb <- delay 1 =<< getPostBuild
  trigger trigger_getCiSettings pb
  el "h1" $ text "Settings Widget"
  dynSettingsForm
  return ()

dynSettingsForm :: MonadApp r t m => m ()
dynSettingsForm = do
  dcs <- asks _as_ciSettings
  _ <- networkHold genericLoading $ ffor (fmapMaybe id $ updated dcs) $ \cs -> do
    semuiForm $ do
      dcsNew <- settingsForm cs never
      (e,_) <- divClass "field" $ elAttr' "button" ("class" =: "ui button") $
        text "Update Settings"
      trigger trigger_updateCiSettings $ tag (current dcsNew) (domEvent Click e)
      return ()
  return ()

--loadingSettings :: MonadApp r t m => m (Dynamic t (Maybe CiSettings))
--loadingSettings = do
--  genericLoading
--  return $ constDyn Nothing

settingsForm
  :: (MonadApp r t m)
  => CiSettings
  -> Event t CiSettings
  -> m (Dynamic t CiSettings)
settingsForm iv sv = do
    dnp <- divClass "field" $ do
      el "label" $ text "Nix Path"
      ie <- inputElement $ def
        & inputElementConfig_initialValue .~ _ciSettings_nixPath iv
        & inputElementConfig_setValue .~ (_ciSettings_nixPath <$> sv)
      return $ value ie
    return (CiSettings <$> dnp)
