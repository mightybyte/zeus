{-# LANGUAGE OverloadedStrings #-}

module Humanizable where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
------------------------------------------------------------------------------
import           Common.Types.ConnectedAccount
------------------------------------------------------------------------------

class Humanizable a where
  humanize :: a -> Text

instance Humanizable AccountProvider where
  humanize = T.pack . show
