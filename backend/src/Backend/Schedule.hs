module Backend.Schedule where

------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Types.ServerEnv
import           Common.Types.BuildJob
import           Common.Types.BuildMsg
import           Common.Types.JobStatus
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

scheduleBuild :: ServerEnv -> RepoBuildInfo -> IO (Either String ())
scheduleBuild env rbi = do
  t <- getCurrentTime
  jobs <- runBeamSqliteDebug putStrLn (_serverEnv_db env) $ do
    runInsertReturningList $ insert (_ciDb_buildJobs ciDb) $ insertExpressions
      [ BuildJob default_ (val_ rbi) (val_ t) (val_ Nothing) (val_ Nothing) (val_ JobPending) ]
  case jobs of
    [j] -> do
      atomically $ writeTQueue (_serverEnv_buildQueue env) (BuildMsg (_buildJob_id j) rbi)
      return (Right ())
    _ -> return $ Left $ "Insert into jobs table returned unexpected number of entries: %s" ++ show jobs
