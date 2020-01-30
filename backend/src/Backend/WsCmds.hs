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

getJobsFromDb :: Connection -> Integer -> Integer -> IO [BuildJob]
getJobsFromDb conn lim off = do
  beamQueryConn conn $
    runSelectReturningList $ select $ limit_ lim $ offset_ off $ do
      orderBy_ (desc_ . _buildJob_id) $
        all_ (_ciDb_buildJobs ciDb)

broadcastJobs :: Connection -> ConnRepo -> IO ()
broadcastJobs conn connRepo = do
  jobs <- getJobsFromDb conn 20 0
  broadcast connRepo $ Down_Jobs jobs
