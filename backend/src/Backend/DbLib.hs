{-# LANGUAGE OverloadedStrings #-}

module Backend.DbLib where

------------------------------------------------------------------------------
import           Control.Lens
import           Database.Beam
import           Database.SQLite.Simple
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Types.ServerEnv
import           Common.Types.CiSettings
------------------------------------------------------------------------------

getCiSettings :: Connection -> IO (Maybe CiSettings)
getCiSettings dbConn = do
  beamQueryConn dbConn $
    runSelectReturningOne $ select $ do
      ci <- all_ (_ciDb_ciSettings ciDb)
      guard_ (ci ^. ciSettings_id ==. (val_ 1))
      return ci

setCiSettings :: Connection -> CiSettings -> IO ()
setCiSettings dbConn (CiSettings _ np Nothing) = do
  beamQueryConn dbConn $ do
    runUpdate $
      update (_ciDb_ciSettings ciDb)
             (\ci -> mconcat
                       [ ci ^. ciSettings_nixPath <-. val_ np
                       , ci ^. ciSettings_s3Cache <-. val_ Nothing ])
             (\ci -> _ciSettings_id ci ==. val_ 1)
setCiSettings dbConn (CiSettings _ np (Just c)) = do
  beamQueryConn dbConn $ do
    ms <- runSelectReturningOne $ select $ do
      ci <- all_ (_ciDb_ciSettings ciDb)
      guard_ (ci ^. ciSettings_id ==. (val_ 1))
      return ci
    case (_ciSettings_s3Cache =<< ms, _s3Cache_secretKey c) of
      (Just old, "") -> do
        runUpdate $
          update (_ciDb_ciSettings ciDb)
                 (\ci -> let c2 = c { _s3Cache_secretKey = _s3Cache_secretKey old }
                          in mconcat
                               [ ci ^. ciSettings_nixPath <-. val_ np
                               , ci ^. ciSettings_s3Cache <-. val_ (Just c2) ])
                 (\ci -> _ciSettings_id ci ==. val_ 1)
      (_, _) -> do
        runUpdate $
          update (_ciDb_ciSettings ciDb)
                 (\ci -> mconcat
                           [ ci ^. ciSettings_nixPath <-. val_ np
                           , ci ^. ciSettings_s3Cache <-. val_ (Just c) ])
                 (\ci -> _ciSettings_id ci ==. val_ 1)

initCiSettings :: Connection -> CiSettings -> IO ()
initCiSettings dbConn (CiSettings _ a b) = do
  beamQueryConn dbConn $ runInsert $ insert (_ciDb_ciSettings ciDb) $
    insertExpressions [CiSettings default_ (val_ a) (val_ b)]
