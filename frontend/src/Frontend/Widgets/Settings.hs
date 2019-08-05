{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Widgets.Settings where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Obelisk.ExecutableConfig as ObConfig
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
  :: (MonadAppIO r t m)
  => m ()
settingsWidget = do
  pb <- delay 0.01 =<< getPostBuild
  trigger trigger_getCiSettings pb
  el "h1" $ text "Settings"
  dynSettingsForm
  return ()

dynSettingsForm
  :: (MonadAppIO r t m)
  => m ()
dynSettingsForm = do
  dcs <- asks _as_ciSettings
  _ <- networkHold blank $ ffor (fmapMaybe id $ updated dcs) $ \cs -> do
    semuiForm $ do
      liftIO $ putStrLn $ "Starting with settings: " <> show cs
      dcsNew <- settingsForm cs never
      let mkAttrs cs2 csNew =
            if cs2 == Just csNew
              then ("class" =: "ui button disabled")
              else ("class" =: "ui button")
      (e,_) <- divClass "field" $ elDynAttr' "button" (mkAttrs <$> dcs <*> dcsNew) $
        text "Update Settings"
      trigger trigger_updateCiSettings $ tag (current dcsNew) (domEvent Click e)
      return ()
  return ()

settingsForm
  :: (MonadAppIO r t m)
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
        el "label" $ text "Push to S3 Cache"
        return v
    res <- networkView (s3CacheWidget (_ciSettings_s3Cache iv) (_ciSettings_s3Cache <$> sv) <$> value useS3Cache)
    cache <- join <$> holdDyn (constDyn $ _ciSettings_s3Cache iv) res

    serveLocalCache <- divClass "field" $ do
      divClass "ui checkbox" $ do
        v <- checkbox (_ciSettings_serveLocalCache iv) $ def
          & setValue .~ (isJust . _ciSettings_s3Cache <$> sv)
        el "label" $ text "Serve Local Cache"
        return $ value v
    infoWidget serveLocalCache
    return (CiSettings 1 <$> dnp <*> cache <*> serveLocalCache)

infoWidget
  :: (MonadAppIO r t m)
  => Dynamic t Bool
  -> m ()
infoWidget serveLocalCache = do
  pb <- delay 0.01 =<< getPostBuild
  trigger trigger_getCiInfo pb

  dcs <- asks _as_ciInfo
  _ <- networkHold blank $ ffor (updated ((,) <$> dcs <*> serveLocalCache)) $
         dynInfoWidget
  return ()

dynInfoWidget
  :: (MonadAppIO r t m)
  => (Maybe Text, Bool)
  -> m ()
dynInfoWidget (Just pubkey, True) = divClass "ui segment" $ do
  mRootRoute <- liftIO $ ObConfig.get "config/common/route"
  case mRootRoute of
    Nothing -> text "Can't find server address.  Server not configured properly."
    Just rootRoute -> do
      let route = T.strip rootRoute <> "/cache/"
      _ <- copyableValue "Cache Address" route
      _ <- copyableValue "Cache Public Key" pubkey
      el "h4" $ text "To use this cache, put the following in your /etc/nix/nix.conf:"
      elClass "pre" "ui segment" $ do
        text $ nixConfExample route pubkey
dynInfoWidget _ = blank

nixConfExample :: Text -> Text -> Text
nixConfExample addr pubkey = T.unlines
  [ "substituters = " <> addr <> " https://cache.nixos.org/"
  , "trusted-public-keys = " <> pubkey <> " cache.nixos.org-1:6NCHdD59X431o0gWypbMrAU"
  ]

copyableValue
  :: (MonadAppIO r t m)
  => Text
  -> Text
  -> m ()
copyableValue label val = do
  el "h4" $ text label
  el "div" $ do
    -- _ <- copyButton (_element_raw e)
    (e,_) <- el' "span" $ text val
    return ()

s3CacheWidget
  :: MonadApp r t m
  => Maybe S3Cache
  -> Event t (Maybe S3Cache)
  -> Bool
  -> m (Dynamic t (Maybe S3Cache))
s3CacheWidget _ _ False = return $ constDyn Nothing
s3CacheWidget iv sv True = divClass "ui segment" $ do
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
