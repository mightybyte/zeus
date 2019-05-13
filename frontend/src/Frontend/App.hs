{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Frontend.App where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.AppState
------------------------------------------------------------------------------


type MonadApp t m = (MonadWidget t m,
                     MonadReader (AppState t) m,
                     EventWriter t AppTriggers m
                    )

type App t m a =
    ReaderT (AppState t) (EventWriterT t AppTriggers m) a

runApp :: MonadWidget t m => App t m a -> m a
runApp m = mdo
    as <- stateManager triggers
    (res, triggers) <- runEventWriterT (runReaderT m as)
    return res
