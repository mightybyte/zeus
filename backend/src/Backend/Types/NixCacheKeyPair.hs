{-# LANGUAGE OverloadedStrings #-}

module Backend.Types.NixCacheKeyPair
  ( NixCacheKey
  , _nck_name
  , _nck_key
  , readKeyFile
  , mkNixSig
  , NixCacheKeyPair(..)
  , signingKeyBaseName
  , signingKeySecretFile
  , signingKeyPublicFile
  ) where

------------------------------------------------------------------------------
import           Crypto.Sign.Ed25519
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
------------------------------------------------------------------------------
import           Common.Types.NixCacheKeyPair
------------------------------------------------------------------------------

mkNixSig :: NixCacheKey -> ByteString -> Text
mkNixSig secret msg = _nck_name secret <> ":" <> sig
  where
    sig = T.decodeUtf8 $ Base64.encode $ unSignature $
            dsign (SecretKey $ _nck_key secret) msg

signingKeyBaseName :: String
signingKeyBaseName = "zeus-cache-key"

-- Would like to put these files in config/backend and config/common
-- respectively but when deployed with obelisk the backend does not have
-- permission to write to those directories.
signingKeySecretFile :: String
--signingKeySecretFile = "config/backend/" <> signingKeyBaseName <> ".sec"
signingKeySecretFile = signingKeyBaseName <> ".sec"
signingKeyPublicFile :: String
--signingKeyPublicFile = "config/common/" <> signingKeyBaseName <> ".pub"
signingKeyPublicFile = signingKeyBaseName <> ".pub"
