module Backend.Schedule where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Types.ServerEnv
import           Common.Types.BuildJob
import           Common.Types.JobStatus
import           Common.Types.Repo
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

scheduleBuild :: ServerEnv -> RepoBuildInfo -> Repo -> IO ()
scheduleBuild env rbi repo = do
  t <- getCurrentTime

  forM_ (unPlatformSet $ _repo_platforms repo) $ \plat ->
    runBeamSqlite (_serverEnv_db env) $ do
      void $ runInsertReturningList $ insert (_ciDb_buildJobs ciDb) $ insertExpressions
        [ BuildJob default_ (val_ rbi) (val_ $ primaryKey repo) (val_ plat) (val_ t)
                   (val_ Nothing) (val_ Nothing) (val_ JobPending) ]
