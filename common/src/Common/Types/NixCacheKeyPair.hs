{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Types.NixCacheKeyPair
  ( NixCacheKey(..)
  , readKeyFile
  , nckToText
  , NixCacheKeyPair(..)
  ) where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
import           System.Directory
------------------------------------------------------------------------------

data NixCacheKey = NixCacheKey
  { _nck_name :: Text
  , _nck_key :: ByteString
  } deriving (Eq,Ord,Show,Generic)

nckToText :: NixCacheKey -> Text
nckToText (NixCacheKey n k) = n <> ":" <> (T.decodeUtf8 $ Base64.encode k)

readKeyText :: Text -> Either String NixCacheKey
readKeyText t = do
    NixCacheKey n <$> Base64.decode (T.encodeUtf8 $ T.drop 1 k)
  where
    (n,k) = T.breakOn ":" t

instance ToJSON NixCacheKey where
  toJSON = String . nckToText
  toEncoding = toEncoding . nckToText

instance FromJSON NixCacheKey where
  parseJSON = withText "NixCacheKey" (either (fail "Invalid nix key format") pure . readKeyText)

instance HasSqlValueSyntax be Text => HasSqlValueSyntax be NixCacheKey where
  sqlValueSyntax = sqlValueSyntax . T.decodeUtf8 . toS . encode

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be NixCacheKey where
  fromBackendRow = maybe (fail "Could not parse NixCacheKey") return . decodeStrict . T.encodeUtf8 =<< fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be NixCacheKey where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

readKeyFile :: FilePath -> IO (Either String NixCacheKey)
readKeyFile fp = do
  exists <- doesFileExist fp
  if not exists
    then return $ Left $ "File " <> fp <> " does not exist"
    else readKeyText . T.strip <$> T.readFile fp

data NixCacheKeyPair = NixCacheKeyPair
  { _nixCacheKey_secret :: NixCacheKey
  , _nixCacheKey_public :: NixCacheKey
  } deriving (Eq,Ord,Show,Generic)
