{-# LANGUAGE OverloadedStrings #-}
module Backend.Common where

import Snap.Core

err404 :: Snap ()
err404 = modifyResponse (setResponseStatus 404 "Not Found") >> writeBS "404 Not Found"
