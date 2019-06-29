{-# LANGUAGE OverloadedStrings #-}
module Backend.Common where

import Data.ByteString (ByteString)
import Snap.Core

-------------------------------------------------------------------------------
-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith


-------------------------------------------------------------------------------
-- | Finish early with error code 400
badReq :: MonadSnap m => ByteString -> m b
badReq = finishEarly 400


-------------------------------------------------------------------------------
-- | Finish early with error code 404
notFound :: MonadSnap m => ByteString -> m b
notFound = finishEarly 404


-------------------------------------------------------------------------------
-- | Finish early with error code 500
serverError :: MonadSnap m => ByteString -> m b
serverError = finishEarly 500
