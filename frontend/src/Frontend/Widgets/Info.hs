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
------------------------------------------------------------------------------
import           Frontend.App
import           Frontend.Widgets.Common
------------------------------------------------------------------------------

infoWidget
  :: (MonadAppIO r t m, Prerender js t m)
  => m ()
infoWidget = do
  Just rootRoute <- liftIO $ ObConfig.get "config/common/route"
  let route = T.strip rootRoute <> "/cache/"
  Just pubkey <- fmap (fmap T.strip) $ liftIO $ ObConfig.get "config/common/zeus-cache-key.pub"
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
