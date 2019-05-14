module Backend.WsCmds where

------------------------------------------------------------------------------
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Migrate
import           Database.Beam.Migrate.Simple
import           Database.SQLite.Simple
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Types.ConnRepo
import           Backend.Types.ServerEnv
import           Backend.WsUtils
import           Common.Api
import           Common.Types.BuildJob
------------------------------------------------------------------------------

getJobsFromDb :: Connection -> IO [BuildJob]
getJobsFromDb conn = do
  beamQueryConn conn $
    runSelectReturningList $ select $ do
      all_ (_ciDb_buildJobs ciDb)

sendJobs :: [BuildJob] -> WS.Connection -> IO ()
sendJobs jobs wsConn = do
  putStrLn "--------------"
  putStrLn "Sending list of jobs:"
  print jobs
  wsSend wsConn $ Down_Jobs jobs

broadcastJobs conn connRepo = do
  jobs <- getJobsFromDb conn
  broadcast connRepo jobs
