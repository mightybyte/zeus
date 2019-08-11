{-# LANGUAGE OverloadedStrings #-}
module Nix.Types where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Readable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word
import           Database.SQLite.Simple
------------------------------------------------------------------------------
import           Backend.Types.ServerEnv
------------------------------------------------------------------------------

nixSqliteDb :: String
nixSqliteDb = "/nix/var/nix/db/db.sqlite"

data ValidPath = ValidPath
  { _validPath_id   :: Int
  , _validPath_path :: Text
  , _validPath_hash :: Text
  , _validPath_registrationTime :: Int
  , _validPath_deriver :: Maybe Text
  , _validPath_narSize :: Maybe Int
  , _validPath_ultimate :: Maybe Int
  , _validPath_sigs :: Maybe [Text]
  , _validPath_ca :: Maybe Text
  } deriving (Eq,Ord,Show,Read)

instance FromRow ValidPath where
  fromRow = ValidPath <$> field <*> field <*> field
                      <*> field <*> field <*> field
                      <*> field <*> (fmap T.words <$> field) <*> field

newtype StorePath = StorePath { unStorePath :: FilePath }
  deriving (Eq,Ord,Show,Read)

data CacheEnv = CacheEnv
  { _cacheEnv_nixSqliteConn :: Connection
  , _cacheEnv_se :: ServerEnv
  }

data NarCompression = NoCompression | Xz | Bzip2
  deriving (Eq,Ord,Enum,Bounded)

instance Show NarCompression where
  show NoCompression = "none"
  show Xz = "xz"
  show Bzip2 = "bzip2"

instance Readable NarCompression where
  fromText "none" = pure NoCompression
  fromText "xz" = pure Xz
  fromText "bzip2" = pure Bzip2
  fromText _ = mzero

data NarInfo = NarInfo
  { _narInfo_storePath :: StorePath
  , _narInfo_urlHash :: Text
  , _narInfo_compression :: NarCompression
  , _narInfo_narHash :: Maybe Text
  , _narInfo_narSize :: Maybe Word64
  , _narInfo_references :: [Text]
  , _narInfo_deriver :: Maybe Text
  , _narInfo_sigs :: [Text]
  } deriving (Eq,Ord)
