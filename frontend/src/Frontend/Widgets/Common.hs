{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Widgets.Common where

------------------------------------------------------------------------------
import           Control.Monad.Fix
import           Data.Default
import           Data.Map (Map)
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Frontend.App
import           Humanizable
------------------------------------------------------------------------------

humanColumn :: (MonadWidget t m, Humanizable b) => (a -> b) -> Dynamic t a -> m ()
humanColumn f = dynText . fmap (humanize . f)
textColumn :: MonadWidget t m => (a -> Text) -> Dynamic t a -> m ()
textColumn f = dynText . fmap f

promptListViewWithKey
  :: (Ord k, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t (Map k v)
  -> (k -> Dynamic t v -> m (Event t a))
  -> m (Event t (Map k a))
promptListViewWithKey vals mkChild =
  switchPromptlyDyn . fmap mergeMap <$> listWithKey vals mkChild

genericTable
  :: (MonadWidget t m, Ord k)
  => Dynamic t (Map k v)
  -> [(Text, k -> Dynamic t v -> m (Event t b))]
  -> m (Event t (Map k b))
genericTable rows cols = do
  elClass "table" "ui celled table" $ do
    el "thead" $ el "tr" $ do
      mapM_ (el "th" . text . fst) cols
    let doRow k v = el "tr" $ do
           es <- mapM (\(_,field) -> field k v) cols
           return $ leftmost es
    el "tbody" $ promptListViewWithKey rows doRow

genericTableG
  :: (MonadWidget t m, Ord k)
  => TableConfig k v t m a
  -> Dynamic t (Map k v)
  -> [(Text, k -> Dynamic t v -> m (Event t a))]
  -> m (Event t (Map k a))
genericTableG cfg rows cols = do
  _tableConfig_tableWrapper cfg $ do
    _tableConfig_headWrapper cfg $ do
      mapM_ (_tableConfig_headRowWrapper cfg) cols
    let doRow k v = _tableConfig_rowFunc cfg k v $ do
           es <- mapM (_tableConfig_cellFunc cfg k v) cols
           return $ leftmost es
    _tableConfig_bodyWrapper cfg $
      promptListViewWithKey rows doRow

data TableConfig k v t m a = TableConfig
  { _tableConfig_tableWrapper :: m (Event t (Map k a)) -> m (Event t (Map k a))
  , _tableConfig_headWrapper :: m () -> m ()
  , _tableConfig_headRowWrapper :: (Text, (k -> Dynamic t v -> m (Event t a))) -> m ()
  , _tableConfig_bodyWrapper :: m (Event t (Map k a)) -> m (Event t (Map k a))
  , _tableConfig_rowFunc :: k -> Dynamic t v -> m (Event t a) -> m (Event t a)
  , _tableConfig_cellFunc :: k -> Dynamic t v -> (Text, (k -> Dynamic t v -> m (Event t a))) -> m (Event t a)
  }

instance MonadWidget t m => Default (TableConfig k v t m a) where
  def = TableConfig
    { _tableConfig_tableWrapper = elClass "table" "ui celled table"
    , _tableConfig_headWrapper = el "thead" . el "tr"
    , _tableConfig_headRowWrapper = el "th" . text . fst
    , _tableConfig_bodyWrapper = el "tbody"
    , _tableConfig_rowFunc = (\_ _ -> el "tr")
    , _tableConfig_cellFunc = (\k v (_,f) -> f k v)
    }

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

genericPlaceholder :: MonadApp t m => Text -> m ()
genericPlaceholder placeholderText = do
  divClass "ui placeholder segment" $ do
    divClass "ui icon header" $ do
      elClass "i" "dont icon" blank
      text placeholderText
