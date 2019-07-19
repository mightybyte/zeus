module Backend.Schedule where

------------------------------------------------------------------------------
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Types.ServerEnv
import           Common.Types.BuildJob
import           Common.Types.JobStatus
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

scheduleBuild :: ServerEnv -> RepoBuildInfo -> IO ()
scheduleBuild env rbi = do
  t <- getCurrentTime
  _ <- runBeamSqlite (_serverEnv_db env) $ do
    runInsertReturningList $ insert (_ciDb_buildJobs ciDb) $ insertExpressions
      [ BuildJob default_ (val_ rbi) (val_ t) (val_ Nothing) (val_ Nothing) (val_ JobPending) ]
  return ()
