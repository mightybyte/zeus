{-# LANGUAGE OverloadedStrings #-}

module Humanizable where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
------------------------------------------------------------------------------
import           Common.Types.ConnectedAccount
import           Common.Types.Repo
------------------------------------------------------------------------------

class Humanizable a where
  humanize :: a -> Text

instance Humanizable AccountProvider where
  humanize = T.pack . show

instance Humanizable CloneMethod where
  humanize SshClone = "SSH"
  humanize HttpClone = "HTTP"
