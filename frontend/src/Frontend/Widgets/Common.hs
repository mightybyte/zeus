{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Widgets.Common where

------------------------------------------------------------------------------
import           Control.Monad.Fix
import           Data.Map (Map)
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Humanizable
------------------------------------------------------------------------------

humanColumn :: (MonadWidget t m, Humanizable b) => (a -> b) -> Dynamic t a -> m ()
humanColumn f = dynText . fmap (humanize . f)
textColumn :: MonadWidget t m => (a -> Text) -> Dynamic t a -> m ()
textColumn f = dynText . fmap f

genericTable
  :: (MonadWidget t m, Ord k, Show b)
  => Dynamic t (Map k v)
  -> [(Text, k -> Dynamic t v -> m (Event t b))]
  -> m (Event t (Map k b))
genericTable rows cols = do
  elClass "table" "ui celled table" $ do
    el "thead" $ el "tr" $ do
      mapM_ (el "th" . text . fst) cols
    let doRow k v = el "tr" $ do
           es <- mapM (\(_,field) -> field k v) cols
           return $ traceEvent "row click" $ leftmost es
    el "tbody" $ --networkView $ mapM doRow <$> rows
      promptListViewWithKey rows doRow

deleteButton :: MonadWidget t m => m (Event t ())
deleteButton = do
  (e,_) <- el' "span" $ elAttr "i" ("class" =: "close icon") blank
  return $ domEvent Click e

genericRemovableTable
  :: (MonadWidget t m, Ord k)
  => Dynamic t (Map k v)
  -> [(Text, k -> Dynamic t v -> m ())]
  -> m (Event t (Map k ()))
genericRemovableTable rows cols = do
  elClass "table" "ui celled table" $ do
    el "thead" $ el "tr" $ do
      mapM_ (el "th" . text . fst) cols
      el "th" blank
    let doRow k v = el "tr" $ do
           mapM_ (\(_,field) -> field k v) cols
           del <- elClass "td" "right aligned collapsing" deleteButton
           return $ traceEvent "row click" del
    el "tbody" $ --networkView $ mapM doRow <$> rows
      listViewWithKey rows doRow

promptListViewWithKey
  :: (Ord k, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t (Map k v)
  -> (k -> Dynamic t v -> m (Event t a))
  -> m (Event t (Map k a))
promptListViewWithKey vals mkChild =
  switchPromptlyDyn . fmap mergeMap <$> listWithKey vals mkChild

--genericDynTable
--  :: MonadWidget t m
--  => Dynamic t [a]
--  -> [(Text, Dynamic t a -> m b)]
--  -> m b
--genericDynTable rows cols = do
--  elClass "table" "ui celled table" $ do
--    el "thead" $ el "tr" $ do
--      mapM_ (el "th" . text . fst) cols
--    el "tbody" $ simpleList rows $ \pair ->
--      el "tr" $ mapM (\(_,field) -> el "td" $ field pair) cols
