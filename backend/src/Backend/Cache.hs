{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Backend.Cache where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Error
import           Control.Lens
import           Control.Monad
import qualified Data.Text as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
import           System.Directory
import           System.IO
import           System.Process
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.DbLib
import           Backend.ExecutablePaths
import           Backend.Process
import           Backend.Types.NixCacheKeyPair
import           Backend.Types.ServerEnv
import           Common.Types.CacheJob
import           Common.Types.CiSettings
import           Common.Types.JobStatus
import           Common.Types.ProcMsg
------------------------------------------------------------------------------

getNextJob :: ServerEnv -> IO (Maybe CacheJob)
getNextJob se = do
  beamQueryConn (_serverEnv_db se) $
    runSelectReturningOne $
    select $
    orderBy_ (asc_ . _cacheJob_id) $ do
      job <- all_ (_ciDb_cacheJobs ciDb)
      guard_ (job ^. cacheJob_status ==. (val_ JobPending))
      return job


cacheManagerThread :: ServerEnv -> IO ()
cacheManagerThread se = do
  forever $ do
    Just cs <- liftIO $ getCiSettings (_serverEnv_db se)
    case _ciSettings_s3Cache cs of
      Nothing -> threadDelay 30000000
      Just s3 -> do
          mjob <- getNextJob se
          case mjob of
            Nothing -> threadDelay 5000000
            Just job -> do
              printf "Caching job:\n%s\nto cache:\n%s\n" (show job) (show s3)
              cacheBuild se s3 job

cacheBuild
  :: ServerEnv
  -> S3Cache
  -> CacheJob
  -> IO ()
cacheBuild se s3cache cj = do
  let dbConn = _serverEnv_db se
  start <- getCurrentTime
  runBeamSqlite dbConn $ do
    runUpdate $
      update (_ciDb_cacheJobs ciDb)
             (\job -> mconcat
                        [ job ^. cacheJob_startedAt <-. val_ (Just start)
                        , job ^. cacheJob_status <-. val_ JobInProgress ])
             (\job -> _cacheJob_id job ==. val_ (_cacheJob_id cj))
    return ()

  let outDir = "log/cache"
  createDirectoryIfMissing True outDir
  let outputFile = printf (outDir <> "/%d.txt") (_cacheJob_id cj)
  printf "Writing cache output to %s\n" outputFile
  _ <- withLogHandle outputFile $ \lh  -> runExceptT $ do
    -- nix sign-paths -k ~/cache-key.sec --all && nix copy --to s3://cache.kadena.io $(nix-build)
    let signCP = proc nixBinary ["sign-paths", "-k", signingKeySecretFile, "--all"]
    let saveAndSend pm = hPutStrLn lh $! prettyProcMsg pm
        saveAndSendStr msgTy msg = do
          !t <- getCurrentTime
          let pm = ProcMsg t msgTy msg
          saveAndSend pm

    runCP signCP saveAndSend

    let bucket = T.unpack $ _s3Cache_bucket s3cache
    let region = T.unpack $ _s3Cache_region s3cache
    let s3url = printf "s3://%s?region=%s" bucket region
    let copyCP = (proc nixBinary ["copy", "--to", s3url, T.unpack $ _cacheJob_storePath cj])
          { env = Just
              [ ("AWS_ACCESS_KEY_ID", T.unpack $ _s3Cache_accessKey s3cache)
              , ("AWS_SECRET_ACCESS_KEY", T.unpack $ _s3Cache_secretKey s3cache)
              ]
          }
    runCP copyCP saveAndSend
    liftIO $ saveAndSendStr CiMsg "Finished caching"

  end <- getCurrentTime
  runBeamSqlite dbConn $ do
    runUpdate $
      update (_ciDb_cacheJobs ciDb)
             (\job -> mconcat
                        [ job ^. cacheJob_endedAt <-. val_ (Just end)
                        , job ^. cacheJob_status <-. val_ JobSucceeded ])
             (\job -> _cacheJob_id job ==. val_ (_cacheJob_id cj))

  return ()
