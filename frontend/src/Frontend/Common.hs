{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Frontend.Common where

------------------------------------------------------------------------------
import           Data.Default
import           Reflex
------------------------------------------------------------------------------


instance Reflex t => Default (Event t a) where
  def = never

infixr 8 <$$>
(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f1 (f0 a) -> f1 (f0 b)
(<$$>) = fmap . fmap
