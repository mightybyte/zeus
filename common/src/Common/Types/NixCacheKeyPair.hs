{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Types.NixCacheKeyPair
  ( NixCacheKey
  , _nck_name
  , _nck_key
  , readKeyFile
  , nckToText
  , NixCacheKeyPair(..)
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           GHC.Generics
import           System.Directory
------------------------------------------------------------------------------

data NixCacheKey = NixCacheKey
  { _nck_name :: Text
  , _nck_key :: ByteString
  } deriving (Eq,Ord,Show,Generic)

readKeyFile :: FilePath -> IO (Either String NixCacheKey)
readKeyFile fp = do
  exists <- doesFileExist fp
  if not exists
    then return $ Left $ "File " <> fp <> " does not exist"
    else do
      t <- T.strip <$> T.readFile fp
      let (n,k) = T.breakOn ":" t
      return $ NixCacheKey n <$> Base64.decode (T.encodeUtf8 $ T.drop 1 k)

nckToText :: NixCacheKey -> Text
nckToText (NixCacheKey n k) = n <> ":" <> (T.decodeUtf8 $ Base64.encode k)

data NixCacheKeyPair = NixCacheKeyPair
  { _nixCacheKey_secret :: NixCacheKey
  , _nixCacheKey_public :: NixCacheKey
  } deriving (Eq,Ord,Show,Generic)
