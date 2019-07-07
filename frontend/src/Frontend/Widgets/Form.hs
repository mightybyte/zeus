{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Frontend.Widgets.Form where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Data.Readable
import qualified Data.Map as M
import           Data.Text (Text)
import           Reflex.Active
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
import qualified Reflex.Dom.SemanticUI as SemUI
------------------------------------------------------------------------------
import           Humanizable
------------------------------------------------------------------------------

semuiForm :: DomBuilder t m => m a -> m a
semuiForm = elClass "div" "ui form"

class Formable t m a where
  --form :: DomBuilder t m => Maybe a -> Event t a -> m (Dynamic t (Maybe a))
  --form :: DomBuilder t m => a -> Event t a -> m (Dynamic t a)
  aForm :: DomBuilder t m => a -> Event t a -> m (Dynamic t a)

--instance Formable t m (Maybe AccountProvider) where
--  aForm iv sv = do
--    v <- SemUI.dropdown def iv sv $ TaggedStatic $ M.fromList $
--           map (\a -> (a, text $ tshow a)) [GitHub, GitLab]
--    return $ value v

instance (Ord a, Enum a, Bounded a, Humanizable a, Prerender js t m) => Formable t m (Maybe a) where
  aForm iv sv = do
    v <- prerender (pure $ pure $ Just minBound) $ fmap value $ SemUI.dropdown def iv sv $ TaggedStatic $ M.fromList $
           map (\a -> (a, text $ humanize a)) [minBound..maxBound]
    return $ join v

filledDropdown
  :: (Ord a, Enum a, Bounded a, Humanizable a, DomBuilder t m, Prerender js t m)
  => a
  -> Event t a
  -> m (Dynamic t a)
filledDropdown iv sv = do
  v <- prerender (pure $ pure minBound) $ fmap (fmap runIdentity . value) $ SemUI.dropdown def (Identity iv) (Identity <$> sv) $ TaggedStatic $ M.fromList $
         map (\a -> (a, text $ humanize a)) [minBound..maxBound]
  return $ join v

--filledDropdown2
--  :: (Ord a, Enum a, Bounded a, Humanizable a, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
--  => a
--  -> Event t a
--  -> m (Dynamic t a)
--filledDropdown2 iv sv = do
--  let pairs = M.fromList $ map (\a -> (a, humanize a)) [minBound..maxBound]
--  v <- dropdown minBound (constDyn pairs) def
--  return $ value v


-- newtype Form t m a = Form { unForm :: Event t a -> m (Dynamic t a) }
--
-- instance (Functor (Form t m), Applicative m, Reflex t) => Applicative (Form t m) where
--   --pure :: a -> Form t m a
--   pure a = Form $ \_ -> pure (constDyn a)
-- --  (<*>) :: f (a -> b) -> f a -> f b
--   Form f <$> Form a = Form $ \svb ->

zoom
  :: (DomBuilder t m, Formable t m b)
  => Lens' a b
  -> a
  -> Event t a
  -> m (Dynamic t b)
zoom theLens iv sv = aForm (view theLens iv) (view theLens <$> sv)

labelledAs :: DomBuilder t m => Text -> m a -> m a
labelledAs label m = divClass "field" $ do
  el "label" $ text label
  m

fieldLabel :: MonadWidget t m => m a -> m a
fieldLabel m = divClass "field" $ el "label" m

textareaField
  :: (DomBuilder t m)
  => Text
  -> Event t Text
  -> m (Dynamic t Text)
textareaField iv sv = do
  t <- textAreaElement $ def
    & textAreaElementConfig_initialValue .~ iv
    & textAreaElementConfig_setValue .~ sv
  return $ value t

textField
  :: (DomBuilder t m)
  => Text
  -> Event t Text
  -> m (Dynamic t Text)
textField iv sv = do
  t <- inputElement $ def
    & inputElementConfig_initialValue .~ iv
    & inputElementConfig_setValue .~ sv
  return $ value t

readableField
  :: (DomBuilder t m,
      Readable a,
      Show a)
  => Maybe Text
  -> Maybe a
  -> Event t (Maybe a)
  -> m (Dynamic t (Maybe a))
readableField mlabel iv sv = divClass "field" $ do
  maybe blank (el "label" . text) mlabel
  let sv2 = maybe "" tshow <$> sv
      iv2 = maybe "" tshow iv
  t <- inputElement $ def
    & inputElementConfig_initialValue .~ iv2
    & inputElementConfig_setValue .~ sv2
  return $ fromText <$> value t
