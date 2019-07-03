{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Which where

import qualified Shelly as Sh
import qualified Data.Text as T
import Language.Haskell.TH (Exp, Q, reportError, runIO)
import Data.Monoid ((<>))
import Data.List (isPrefixOf)

-- | Determine which executable would run if the given path were executed, or return Nothing if a suitable executable cannot be found
which :: FilePath -> IO (Maybe FilePath)
which f = fmap (fmap (T.unpack . Sh.toTextIgnore)) $ Sh.shelly $ Sh.which $ Sh.fromText $ T.pack f

-- | Run `which` at compile time, and substitute the full path to the executable.
--
-- This is useful in NixOS to ensure that the resulting executable contains the dependency in its closure and that it refers to the same version at run time as at compile time
staticWhich :: FilePath -> Q Exp
staticWhich f = do
  mf' <- runIO $ which f
  case mf' of
    Nothing -> compileError $ "Could not find executable for " <> show f
    Just f'
      | "/nix/store/" `isPrefixOf` f' -> [| f' |]
      | otherwise -> compileError $ "Path to executable " <> show f <> " was found in " <> show f' <> " which is not in /nix/store. Be sure to add the relevant package to 'backendTools' in default.nix."

  where
    compileError msg' = do
      let msg = "staticWhich: " <> msg'
      reportError msg
      [| error msg |]
