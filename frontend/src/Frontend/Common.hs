{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Frontend.Common where

------------------------------------------------------------------------------
import           Data.Default
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.CssClass
------------------------------------------------------------------------------


instance Reflex t => Default (Event t a) where
  def = never

infixr 8 <$$>
(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f1 (f0 a) -> f1 (f0 b)
(<$$>) = fmap . fmap

data ListState = EmptyPlaceholder | AddForm | ListTable
  deriving (Eq,Ord,Show,Read)

data TableAction t a = TableAction
  { tableAction_showAddForm :: Event t ()
  , tableAction_showList :: Event t ()
  }

instance Reflex t => Default (TableAction t a) where
  def = TableAction def def

addClassWhen :: Monad (Dynamic t) => CssClass -> Dynamic t Bool -> CssClass -> Dynamic t CssClass
addClassWhen dynKlass dynBool staticKlass = do
  a <- dynBool
  return $ if a then dynKlass <> staticKlass else staticKlass

--intLink :: DomBuilder t m => Text -> m a -> m a
--intLink href m =
--  elAttr "a" ("href" =: href) $ m

extLink :: DomBuilder t m => Text -> m a -> m a
extLink href m =
  elAttr "a" ("href" =: href <> "target" =: "_blank" <> "rel" =: "noopener") $ m
