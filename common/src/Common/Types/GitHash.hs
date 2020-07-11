module Common.Types.GitHash where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
------------------------------------------------------------------------------

newtype GitHash = GitHash { unGitHash :: Text }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GitHash where
  parseJSON a = GitHash <$> parseJSON a

instance ToJSON GitHash where
  toJSON (GitHash a) = toJSON a

shortHash :: GitHash -> Text
shortHash = T.take 8 . unGitHash

