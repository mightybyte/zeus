{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import qualified Network.AWS as AWS
import qualified Network.AWS.S3 as AWS
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.CacheServer
import           Backend.Db
import           Backend.ExecutablePaths
import           Backend.Process
import           Backend.Types.NixCacheKeyPair
import           Backend.Types.ServerEnv
import           Common.Types.BinaryCache
import           Common.Types.CacheJob
import           Common.Types.CachedHash
import           Common.Types.JobStatus
import           Common.Types.ProcMsg
import           Common.Types.S3Cache
import           Nix.Types
------------------------------------------------------------------------------

getNextJob :: Connection -> IO (Maybe (CacheJob, BinaryCache))
getNextJob conn = do
  beamQueryConn conn $
    runSelectReturningOne $
    select $ do
      job <- limit_ 1 $ orderBy_ (asc_ . _cacheJob_id) $
        all_ (_ciDb_cacheJobs ciDb)
      guard_ (job ^. cacheJob_status ==. (val_ JobPending))

      cache <- all_ (_ciDb_binaryCaches ciDb)
      return (job, cache)


cacheManagerThread :: ServerEnv -> IO ()
cacheManagerThread se = do
  putStrLn "Starting cache manager thread"
  forever $ do
    mjob <- getNextJob (_serverEnv_db se)
    case mjob of
      Nothing -> threadDelay 5000000
      Just (job, cache) -> do
        printf "Caching store path %s to cache: %s\n" (_cacheJob_storePath job) (_s3Cache_bucket $ _binaryCache_s3Cache cache)
        cacheBuild se cache job

------------------------------------------------------------------------------
-- Calculate the transitive closure of a nix file.
calcNixClosure :: FilePath -> IO (Either ExitCode [Text])
calcNixClosure nixFile = calcStorePathClosure ("$(nix-instantiate " <> nixFile <> ")")

calcStorePathClosure :: FilePath -> IO (Either ExitCode [Text])
calcStorePathClosure storePath = do
    let cmd = printf "%s -qR --include-outputs %s" nixStore storePath
    (ec,out,_) <- readCreateProcessWithExitCode (shell cmd) ""
    case ec of
      ExitSuccess -> return $ Right $ T.lines $ T.pack out
      ExitFailure _ -> return $ Left ec

toAwsRegion :: Region -> AWS.Region
toAwsRegion = \case
  NorthVirginia -> AWS.NorthVirginia
  Ohio -> AWS.Ohio
  NorthCalifornia -> AWS.NorthCalifornia
  Oregon -> AWS.Oregon
  Montreal -> AWS.Montreal
  Tokyo -> AWS.Tokyo
  Seoul -> AWS.Seoul
  Mumbai -> AWS.Mumbai
  Singapore -> AWS.Singapore
  Sydney -> AWS.Sydney
  SaoPaulo -> AWS.SaoPaulo
  Ireland -> AWS.Ireland
  London -> AWS.London
  Frankfurt -> AWS.Frankfurt
  GovCloud -> AWS.GovCloud
  GovCloudFIPS -> AWS.GovCloudFIPS
  Beijing -> AWS.Beijing


listBucket :: AWS.Env -> Text -> Region -> IO [Text]
listBucket e b r = do
  resp <- AWS.runResourceT $ AWS.runAWS e $
    AWS.within (toAwsRegion r) $
      AWS.send (AWS.listObjectsV2 (AWS.BucketName b) & AWS.lovPrefix .~ Just "nar/")
  let os = resp ^. AWS.lovrsContents
  return $ map (view $ AWS.oKey . AWS._ObjectKey) os

objectInfo :: AWS.Env -> Text -> Region -> Text -> IO AWS.HeadObjectResponse
objectInfo e b r k = do
  AWS.runResourceT $ AWS.runAWS e $
    AWS.within (toAwsRegion r) $
      AWS.send (AWS.headObject (AWS.BucketName b) (AWS.ObjectKey k))

doesObjectExist :: AWS.Env -> Text -> Region -> Text -> IO Bool
doesObjectExist e b r k = do
  eresp :: Either SomeException AWS.HeadObjectResponse <- try (objectInfo e b r k)
  return $ either (const False) (const True) eresp

storePathHash :: Text -> Text
storePathHash = T.takeWhile (/= '-') . T.takeWhileEnd (/= '/')

------------------------------------------------------------------------------
-- | To upload a store path to a binary cache, we have to do two things:
--
-- 1. Upload the nar (probably compressed) to nar/<uniqueid>.nar[.xz]
-- 2. Upload the narinfo as <storepath>.narinfo signed with the store's
-- private key
--
-- The only constraints on uniqueid in the nar filename are that it must be
-- internally consistent with what is contained in the narinfo.
cacheStorePath
  :: ServerEnv
  -> (ProcMsg -> IO ())
  -> Connection
  -> NixCacheKeyPair
  -> BinaryCache
  -> StorePath
  -> ExceptT ExitCode IO ()
cacheStorePath se logFunc nixDb cacheKey cache sp@(StorePath spt) = do
    mni <- liftIO $ getNarInfo nixDb (_nixCacheKey_secret cacheKey) sp
    case mni of
      Nothing -> throwError $ ExitFailure 42
      Just ni -> do
        let spHash = storePathHash $ T.pack spt
        --let narDumpCmd = printf "%s --dump %s"
        let oname = "nar/" <> spHash <> ".nar.xz"

        let s3cache = _binaryCache_s3Cache cache
        e <- AWS.newEnv $ AWS.FromKeys
          (AWS.AccessKey $ encodeUtf8 $ _s3Cache_accessKey s3cache)
          (AWS.SecretKey $ encodeUtf8 $ _s3Cache_secretKey s3cache)
        exists <- liftIO $ doesObjectExist e (_s3Cache_bucket s3cache) (_s3Cache_region s3cache) oname

        if exists
          then return ()
          else do
            let narUploadCmd = printf "%s --dump %s | %s --stdout | %s s3 cp - s3://%s/%s"
                  nixStore spt xzBinary awsBinary (_s3Cache_bucket s3cache) oname
            runCP (shell narUploadCmd) logFunc

            let niContents = narInfoToString ni

            resp <- liftIO $ AWS.runResourceT $ AWS.runAWS e $
              AWS.within (toAwsRegion $ _s3Cache_region s3cache) $
                AWS.send (AWS.putObject (AWS.BucketName $ _s3Cache_bucket s3cache)
                         (AWS.ObjectKey (spHash <> ".narinfo"))
                         (AWS.toBody niContents))
            let status = resp ^. AWS.porsResponseStatus
            if status == 200
              then return ()
              else do
                liftIO $ printf "Error uploading narinfo for %s\n" spt
                throwError (ExitFailure status)
            liftIO $ storeCachedHash se (primaryKey cache) spHash

storeCachedHash :: ServerEnv -> PrimaryKey BinaryCacheT Identity -> Text -> IO ()
storeCachedHash se cache hash = do
  t <- getCurrentTime
  beamQuery se $ do
    runInsert $ insert (_ciDb_cachedHashes ciDb) $ insertExpressions
      [CachedHash default_ (val_ cache) (val_ hash) (val_ t)]

narInfoToString :: NarInfo -> Text
narInfoToString (NarInfo sp u c _ _ refs _ sigs) = T.unlines $
  [ "StorePath: " <> T.pack (unStorePath sp)
  , "URL: nar/" <> u <> ".nar.xz"
  , "Compression: " <> T.pack (show c)
  --, "FileHash: sha256:1g3pf3h89a5l106casdf8wgqjagsr52a5mxlnhv5xykkwf3rf2r6"
  --, "FileSize: 31476"
  --, "NarHash: sha256:1c415vhi7zbkxlvgphanjm3q31x8qhbh2zs9ygfnmk57xgrwf3kl"
  --, "NarSize: 107320"
  , "References: " <> T.unwords (map stripPath refs)
  --, "CA: fixed:r:sha256:1c415vhi7zbkxlvgphanjm3q31x8qhbh2zs9ygfnmk57xgrwf3kl"
  ] ++ map ("Sig: " <>) sigs

cacheBuild
  :: ServerEnv
  -> BinaryCache
  -> CacheJob
  -> IO ()
cacheBuild se cache cj = do
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
    let logProcMsg pm = hPutStrLn lh $! prettyProcMsg pm
        logStr msgTy msg = do
          !t <- getCurrentTime
          let pm = ProcMsg t msgTy msg
          logProcMsg pm

    toCache <- ExceptT $ calcStorePathClosure $ T.unpack $ _cacheJob_storePath cj
    liftIO $ logStr CiMsg $ T.pack $ printf "Caching %d store paths" (length toCache)
    nixDbConn <- liftIO $ open nixSqliteDb
    liftIO $ forM_ toCache $ \sp -> do
      liftIO $ logStr CiMsg $ T.pack $ printf "Caching %s" sp
      res <- runExceptT $ cacheStorePath se logProcMsg nixDbConn (_serverEnv_cacheKey se) cache (StorePath $ T.unpack sp)
      case res of
        Left e -> liftIO $ logStr CiMsg $ T.pack $ printf "Error %s while caching %s" (show e) sp
        Right _ -> return ()
    liftIO $ logStr CiMsg "Finished caching"

  end <- getCurrentTime
  runBeamSqlite dbConn $ do
    runUpdate $
      update (_ciDb_cacheJobs ciDb)
             (\job -> mconcat
                        [ job ^. cacheJob_endedAt <-. val_ (Just end)
                        , job ^. cacheJob_status <-. val_ JobSucceeded ])
             (\job -> _cacheJob_id job ==. val_ (_cacheJob_id cj))

  return ()
