{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Frontend.App where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
import           Obelisk.Route.Frontend
------------------------------------------------------------------------------
import           Common.Route
import           Frontend.AppState
------------------------------------------------------------------------------


type MonadApp r t m =
  ( DomBuilder t m
  , PostBuild t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadRef m
  , PerformEvent t m
  , TriggerEvent t m
  , MonadReader (AppState t) m
  , EventWriter t AppTriggers m
  , Routed t r m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  )

type MonadAppIO r t m =
  ( MonadApp r t m
  , MonadIO m
  , MonadIO (Performable m)
  )

--type App r t m a =
--    RoutedT t r (ReaderT (AppState t) (EventWriterT t AppTriggers m)) a

runApp
  :: (Routed t r m, DomBuilder t m, Prerender js t m, MonadHold t m, MonadFix m)
  => Text
  -> RoutedT t r (ReaderT (AppState t) (EventWriterT t AppTriggers m)) a
  -> m a
runApp publicUrl m = mdo
    r <- askRoute
    as <- stateManager publicUrl triggers
    (res, triggers) <- runEventWriterT (runReaderT (runRoutedT m r) as)
    return res
