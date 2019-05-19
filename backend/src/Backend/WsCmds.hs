module Backend.WsCmds where

------------------------------------------------------------------------------
import           Database.Beam
import           Database.SQLite.Simple
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Types.ConnRepo
import           Backend.Types.ServerEnv
import           Common.Api
import           Common.Types.BuildJob
------------------------------------------------------------------------------

getJobsFromDb :: Connection -> IO [BuildJob]
getJobsFromDb conn = do
  beamQueryConn conn $
    runSelectReturningList $ select $ do
      all_ (_ciDb_buildJobs ciDb)

broadcastJobs :: Connection -> ConnRepo -> IO ()
broadcastJobs conn connRepo = do
  jobs <- getJobsFromDb conn
  broadcast connRepo $ Down_Jobs jobs
