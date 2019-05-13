{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Frontend.AppState where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Api
import           Common.Route
import           Common.Types.ConnectedAccount
import           Frontend.Common ()
------------------------------------------------------------------------------

data AppTriggers = AppTriggers
    { _trigger_getAccounts :: Batch ()
    , _trigger_connectAccount :: Batch (ConnectedAccountT Maybe)
    , _trigger_delAccounts :: Batch Int
    } deriving Generic

instance Semigroup AppTriggers where
  (AppTriggers ga1 ca1 da1) <> (AppTriggers ga2 ca2 da2) = AppTriggers
    (ga1 <> ga2)
    (ca1 <> ca2)
    (da1 <> da2)

instance Monoid AppTriggers where
    mempty = AppTriggers
      mempty
      mempty
      mempty
    mappend = (<>)

makeLenses ''AppTriggers

trigger
    :: (Reflex t, EventWriter t AppTriggers m)
    => Lens' AppTriggers (Batch a)
    -> Event t a
    -> m ()
trigger l e = triggerBatch l $ batchOne <$> e

triggerBatch
    :: (Reflex t, EventWriter t AppTriggers m)
    => Lens' AppTriggers (Batch a)
    -> Event t (Batch a)
    -> m ()
triggerBatch l e = tellEvent $ (\as -> set l as mempty) <$> e

data AppState t = AppState
    { _as_accounts :: Dynamic t [ConnectedAccountT Maybe]
    } deriving Generic

stateManager
    :: MonadWidget t m
    => Event t AppTriggers
    -> m (AppState t)
stateManager ft = do
    let upEvent = mergeWith (++) $ map (fmap (:[]))
          [ Up_ListAccounts <$ fmapMaybe (listToMaybe . _trigger_getAccounts) ft
          , Up_ConnectAccount . _trigger_connectAccount <$> ft
          , Up_DelAccounts . _trigger_delAccounts <$> ft
          ]
    let cfg = WebSocketConfig upEvent never True []
    route <- liftIO getAppRoute
    ws <- startWebsocket route cfg
    let downEvent = _webSocket_recv ws
    accounts <- holdDyn mempty $
                  fmapMaybe (preview (_Down_ConnectedAccounts)) downEvent

    return $ AppState accounts

startWebsocket
  :: MonadWidget t m
  => Text
  -> WebSocketConfig t Up
  -> m (RawWebSocket t Down)
startWebsocket siteRoute wsCfg = do
    let (scheme,rest) = T.breakOn "://" siteRoute
        wsScheme = case scheme of
                     "http" -> "ws"
                     "https" -> "wss"
                     _ -> error $ "Invalid scheme: " ++ T.unpack scheme
    RawWebSocket r o e c <- jsonWebSocket (wsScheme <> rest <> "/ws") wsCfg
    return (RawWebSocket (fmapMaybe id r) o e c)
