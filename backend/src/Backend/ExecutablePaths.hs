{-# LANGUAGE TemplateHaskell #-}

module Backend.ExecutablePaths where

import Which

gitBinary :: String
gitBinary = $(staticWhich "git")

nixBinary :: String
nixBinary = $(staticWhich "nix")

nixBuildBinary :: String
nixBuildBinary = $(staticWhich "nix-build")

nixInstantiate :: String
nixInstantiate = $(staticWhich "nix-instantiate")

nixStore :: String
nixStore = $(staticWhich "nix-store")

bzip2 :: String
bzip2 = $(staticWhich "bzip2")
