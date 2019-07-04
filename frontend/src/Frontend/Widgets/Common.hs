{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Widgets.Common where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix
import           Data.Default
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified GHCJS.DOM.Types as DOM
import           Language.Javascript.JSaddle (MonadJSM, liftJSM, JSVal)
import qualified Language.Javascript.JSaddle as JS
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Api
import           Frontend.App
import           Frontend.AppState
import           Humanizable
------------------------------------------------------------------------------

humanColumn :: (DomBuilder t m, PostBuild t m, Humanizable b) => (a -> b) -> Dynamic t a -> m ()
humanColumn f = dynText . fmap (humanize . f)
textColumn :: (DomBuilder t m, PostBuild t m) => (a -> Text) -> Dynamic t a -> m ()
textColumn f = dynText . fmap f

promptListViewWithKey
  :: (Ord k, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t (Map k v)
  -> (k -> Dynamic t v -> m (Event t a))
  -> m (Event t (Map k a))
promptListViewWithKey vals mkChild =
  switchPromptlyDyn . fmap mergeMap <$> listWithKey vals mkChild

genericTable
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Ord k)
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
  :: (DomBuilder t m, PostBuild  t m, MonadHold t m, MonadFix m, Ord k)
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

instance DomBuilder t m => Default (TableConfig k v t m a) where
  def = TableConfig
    { _tableConfig_tableWrapper = elClass "table" "ui celled table"
    , _tableConfig_headWrapper = el "thead" . el "tr"
    , _tableConfig_headRowWrapper = el "th" . text . fst
    , _tableConfig_bodyWrapper = el "tbody"
    , _tableConfig_rowFunc = (\_ _ -> el "tr")
    , _tableConfig_cellFunc = (\k v (_,f) -> f k v)
    }

deleteButton :: DomBuilder t m => m (Event t ())
deleteButton = do
  (e,_) <- elAttr' "i" ("class" =: "trash icon") blank
  return $ domEvent Click e

deleteColumn
  :: MonadApp r t m
  => Lens' AppTriggers (Batch a)
  -> a
  -> m (Event t ())
deleteColumn trig k = do
  (e,_) <- elAttr' "td" ("class" =: "clickable right aligned collapsing") $
    elAttr "i" ("class" =: "trash icon") blank
  triggerBatch trig ([k] <$ domEvent Click e)
  return never

genericRemovableTable
  :: (DomBuilder t m, PostBuild  t m, MonadHold t m, MonadFix m, Ord k)
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
--  :: DomBuilder t m
--  => Dynamic t [a]
--  -> [(Text, Dynamic t a -> m b)]
--  -> m b
--genericDynTable rows cols = do
--  elClass "table" "ui celled table" $ do
--    el "thead" $ el "tr" $ do
--      mapM_ (el "th" . text . fst) cols
--    el "tbody" $ simpleList rows $ \pair ->
--      el "tr" $ mapM (\(_,field) -> el "td" $ field pair) cols

genericPlaceholder :: DomBuilder t m => Text -> m ()
genericPlaceholder placeholderText = do
  divClass "ui placeholder segment" $ do
    divClass "ui icon header" $ do
      elClass "i" "dont icon" blank
      text placeholderText

genericLoading :: DomBuilder t m => m ()
genericLoading = do
  elAttr "div" ("class" =: "ui segment" <> "style" =: "height: 100px") $
    divClass "ui active dimmer" $ do
      divClass "ui text loader" $
        text "Loading"

-- | Copies the text content of a given node to the clipboard.
copyButton
  :: forall t m
  . ( MonadJSM (Performable m), PerformEvent t m
    , RawElement (DomBuilderSpace m) ~ DOM.Element
    , DomBuilder t m
    )
  => RawElement (DomBuilderSpace m)
  -> m (Event t ())
copyButton e = do
    onClick <- fmap (domEvent Click . fst) $ elAttr' "span" ("class" =: "clickable") $
      elClass "button" "mini ui basic button" $ text "Copy"
      --elClass "i" "copy icon" blank
    performEvent_ $ jsCopy e <$ onClick
    pure onClick
  where

    jsCopy :: forall m1. MonadJSM m1 => RawElement (DomBuilderSpace m) -> m1 ()
    jsCopy eL = do
      jsCopyFunc <- jsCopyVal
      void $ liftJSM $ JS.call jsCopyFunc JS.obj [DOM.unElement eL]

    jsCopyVal :: forall m1. MonadJSM m1 => m1 JSVal
    jsCopyVal = liftJSM $ JS.eval $ T.unlines
      [ "(function(e) {"
      , " try {"
      , "    var range = document.createRange();"
      , "    range.selectNodeContents(e);"
      , "    var selection = window.getSelection();"
      , "    selection.removeAllRanges();"
      , "    selection.addRange(range);"
      , "    document.execCommand('copy');"
      , " } catch(e) { console.log('Copy failed!'); return false; }"
      , "})"
      ]
