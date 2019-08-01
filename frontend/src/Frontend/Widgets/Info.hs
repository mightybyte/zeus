{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Widgets.Info where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Obelisk.ExecutableConfig as ObConfig
import           Reflex.Dom
import           Reflex.Network
------------------------------------------------------------------------------
import           Frontend.App
import           Frontend.AppState
import           Frontend.Widgets.Common
------------------------------------------------------------------------------

infoWidget
  :: (MonadAppIO r t m, Prerender js t m)
  => m ()
infoWidget = do
  pb <- delay 0.01 =<< getPostBuild
  trigger trigger_getCiInfo pb

  dcs <- asks _as_ciInfo
  _ <- networkHold blank $ ffor (fmapMaybe id $ updated dcs) $
         dynInfoWidget
  return ()

dynInfoWidget
  :: (MonadAppIO r t m, Prerender js t m)
  => Text
  -> m ()
dynInfoWidget pubkey = do
  Just rootRoute <- liftIO $ ObConfig.get "config/common/route"
  let route = T.strip rootRoute <> "/cache/"
  _ <- prerender blank $ copyableValue "Cache Address" route
  _ <- prerender blank $ copyableValue "Cache Public Key" pubkey
  el "h4" $ text "To use this cache, put the following in your /etc/nix/nix.conf:"
  elClass "pre" "ui segment" $ do
    text $ nixConfExample route pubkey

nixConfExample :: Text -> Text -> Text
nixConfExample addr pubkey = T.unlines
  [ "substituters = " <> addr <> " https://cache.nixos.org/"
  , "trusted-public-keys = " <> pubkey <> " cache.nixos.org-1:6NCHdD59X431o0gWypbMrAU"
  ]

copyableValue
  :: (MonadWidget t m)
  => Text
  -> Text
  -> m ()
copyableValue label val = do
  el "h4" $ text label
  el "div" $ mdo
    _ <- copyButton (_element_raw e)
    (e,_) <- el' "span" $ text val
    return ()
