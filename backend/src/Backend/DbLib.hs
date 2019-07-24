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
      guard_ (ci ^. ciSettings_id ==. (val_ 0))
      return ci

setCiSettings :: Connection -> CiSettings -> IO ()
setCiSettings dbConn (CiSettings _ np c) = do
  beamQueryConn dbConn $
    runUpdate $
      update (_ciDb_ciSettings ciDb)
             (\ci -> mconcat
                        [ ci ^. ciSettings_nixPath <-. val_ np
                        , ci ^. ciSettings_s3Cache <-. val_ c ])
             (\ci -> _ciSettings_id ci ==. val_ 0)

initCiSettings :: Connection -> CiSettings -> IO ()
initCiSettings dbConn (CiSettings _ a b) = do
  beamQueryConn dbConn $ runInsert $ insert (_ciDb_ciSettings ciDb) $
    insertExpressions [CiSettings default_ (val_ a) (val_ b)]
