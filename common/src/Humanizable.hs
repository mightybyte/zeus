{-# LANGUAGE OverloadedStrings #-}

module Humanizable where

------------------------------------------------------------------------------
import           Data.Text (Text)
------------------------------------------------------------------------------

class Humanizable a where
  humanize :: a -> Text
