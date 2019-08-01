{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Widgets.Settings where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Dom
import           Reflex.Network
------------------------------------------------------------------------------
import           Common.Types.CiSettings
import           Frontend.App
import           Frontend.AppState
import           Frontend.Widgets.Form
------------------------------------------------------------------------------

settingsWidget
  :: MonadAppIO r t m
  => m ()
settingsWidget = do
  pb <- delay 0.01 =<< getPostBuild
  trigger trigger_getCiSettings pb
  el "h1" $ text "Settings"
  dynSettingsForm
  return ()

dynSettingsForm :: MonadApp r t m => m ()
dynSettingsForm = do
  dcs <- asks _as_ciSettings
  _ <- networkHold blank $ ffor (fmapMaybe id $ updated dcs) $ \cs -> do
    semuiForm $ do
      dcsNew <- settingsForm cs never
      (e,_) <- divClass "field" $ elAttr' "button" ("class" =: "ui button") $
        text "Update Settings"
      trigger trigger_updateCiSettings $ tag (current dcsNew) (domEvent Click e)
      return ()
  return ()

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
    useS3Cache <- divClass "field" $ do
      divClass "ui checkbox" $ do
        v <- checkbox (isJust $ _ciSettings_s3Cache iv) $ def
          & setValue .~ (isJust . _ciSettings_s3Cache <$> sv)
        el "label" $ text "Use S3 Cache"
        return v
    res <- networkView (s3CacheWidget (_ciSettings_s3Cache iv) (_ciSettings_s3Cache <$> sv) <$> value useS3Cache)
    cache <- join <$> holdDyn (constDyn $ _ciSettings_s3Cache iv) res
    return (CiSettings 0 <$> dnp <*> cache)

s3CacheWidget
  :: MonadApp r t m
  => Maybe S3Cache
  -> Event t (Maybe S3Cache)
  -> Bool
  -> m (Dynamic t (Maybe S3Cache))
s3CacheWidget _ _ False = return $ constDyn Nothing
s3CacheWidget iv sv True = do
    db :: Dynamic t Text <- divClass "field" $ do
      el "label" $ text "Bucket"
      v <- inputElement $ def
        & inputElementConfig_initialValue .~ maybe "" _s3Cache_bucket iv
        & inputElementConfig_setValue .~ (maybe "" _s3Cache_bucket <$> sv)
      return $ value v
    dr <- divClass "field" $ do
      el "label" $ text "Region"
      v <- inputElement $ def
        & inputElementConfig_initialValue .~ maybe "" _s3Cache_region iv
        & inputElementConfig_setValue .~ (maybe "" _s3Cache_region <$> sv)
      return $ value v
    dak <- divClass "field" $ do
      el "label" $ text "Access Key"
      v <- inputElement $ def
        & inputElementConfig_initialValue .~ maybe "" _s3Cache_accessKey iv
        & inputElementConfig_setValue .~ (maybe "" _s3Cache_accessKey <$> sv)
      return $ value v
    dsk <- divClass "field" $ do
      el "label" $ do
        text "Secret Key "
        let tip = "Secret key not shown for security.  Leaving it empty will not clear it."
        elAttr "span" ("data-tooltip" =: tip <> "data-position" =: "top left") $
          elAttr "i" ("class" =: "info circle icon") blank

      v <- inputElement $ def
        & inputElementConfig_initialValue .~ maybe "" _s3Cache_secretKey iv
        & inputElementConfig_setValue .~ (maybe "" _s3Cache_secretKey <$> sv)
      return $ value v
    return $ do
      b <- db
      r <- dr
      ak <- dak
      sk <- dsk
      if any T.null [b, r, ak]
        then pure Nothing
        else pure $ Just $ S3Cache b r ak sk
