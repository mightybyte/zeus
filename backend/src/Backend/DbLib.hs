{-# LANGUAGE OverloadedStrings #-}

module Backend.DbLib where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Int
import           Database.Beam
import           Database.SQLite.Simple
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Types.ServerEnv
import           Common.Types.CiSettings
------------------------------------------------------------------------------

globalCiSettingsKey :: Int32
globalCiSettingsKey = 0

defCiSettings :: CiSettings
defCiSettings = CiSettings globalCiSettingsKey "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos" True

getCiSettings :: Connection -> IO (Maybe CiSettings)
getCiSettings dbConn = do
  beamQueryConn dbConn $
    runSelectReturningOne $ select $ do
      ci <- all_ (_ciDb_ciSettings ciDb)
      guard_ (ci ^. ciSettings_id ==. (val_ globalCiSettingsKey))
      return ci

setCiSettings :: Connection -> CiSettings -> IO ()
setCiSettings dbConn (CiSettings _ np slc) = do
  beamQueryConn dbConn $ do
    --ms <- runSelectReturningOne $ select $ do
    --  ci <- all_ (_ciDb_ciSettings ciDb)
    --  guard_ (ci ^. ciSettings_id ==. (val_ 1))
    --  return ci
    runUpdate $
      update (_ciDb_ciSettings ciDb)
             (\ci -> mconcat
                       [ ci ^. ciSettings_nixPath <-. val_ np
                       , ci ^. ciSettings_serveLocalCache <-. val_ slc
                       ])
             (\ci -> _ciSettings_id ci ==. val_ 1)

initCiSettings :: Connection -> CiSettings -> IO ()
initCiSettings dbConn (CiSettings i a slc) = do
  beamQueryConn dbConn $ runInsert $ insert (_ciDb_ciSettings ciDb) $
    insertExpressions [CiSettings (val_ i) (val_ a) (val_ slc)]
