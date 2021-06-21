{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Backend.CacheServer where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Database.SQLite.Simple
import           Snap.Core
import           System.Process
import           Text.Printf
import qualified Turtle.Bytes as T
------------------------------------------------------------------------------
import           Backend.Common
import           Backend.ExecutablePaths
import qualified Backend.NixBase32 as Base32
import           Backend.Types.NixCacheKeyPair
import           Backend.Types.ServerEnv
import           Nix.Types
------------------------------------------------------------------------------

fingerprintPath :: Text -> Text -> Int -> [Text] -> Either String Text
fingerprintPath storePath narHash narSize refs = do
    (hashType,hash) <- mkBase32 narHash
    pure $ "1;" <> T.intercalate ";"
      [ storePath, hashType <> ":" <> hash, T.pack (show narSize)
      , T.intercalate "," refs
      ]

fingerprintVP :: ValidPath -> [Text] -> Either String Text
fingerprintVP (ValidPath _ p h _ _ (Just s) _ _ _) refs =
  fingerprintPath p h s refs
fingerprintVP _ _ = Left "NarSize not available"

------------------------------------------------------------------------------
-- | Similar to queryPathInfo / queryPathInfoUncached in the Nix C++ code.
queryStorePathInfo :: Connection -> FilePath -> IO (Maybe ValidPath)
queryStorePathInfo conn storePath = do
    vpRes <- query conn "select * from ValidPaths where path = ? limit 1" (Only storePath)
    return $ listToMaybe vpRes

mkBase32 :: Text -> Either String (Text,Text)
mkBase32 narHash = (hashType,) <$> base32hash
  where
    (hashType,hash) = T.breakOn ":" narHash
    hashBS = T.encodeUtf8 $ T.drop 1 hash
    base32hash =
      case T.length narHash of
        71 -> let (bs,rest) = Base16.decode hashBS
               in if B.length rest > 0
                    then Left $ printf "Hash didn't decode completely (%s), rest=%s\n"
                                       (T.decodeUtf8 hashBS) (T.decodeUtf8 rest)
                    else Right (Base32.encode bs)
        59 -> Right $ T.decodeUtf8 hashBS
        _ -> Left $ printf "Hash had unexpected length (%s)\n" (T.decodeUtf8 hashBS)

nixCacheRoutes :: ServerEnv -> [Text] -> Snap ()
nixCacheRoutes se ps = do
  case ps of
    ["nix-cache-info"] -> cacheInfoHandler
    ["nar", nar] -> narHandler nar
    [p] -> otherHandler se p
    _ -> notFound "File not found."

stripPath :: Text -> Text
stripPath = T.takeWhileEnd (/= '/')

storePathHash :: Text -> Text
storePathHash = T.takeWhile (/= '-') . T.takeWhileEnd (/= '/')

otherHandler :: ServerEnv -> Text -> Snap ()
otherHandler se file = do
  case T.breakOn ".narinfo" file of
    (hash, ".narinfo") -> do
      sd <- read <$> liftIO (getNixConfigAttr "nixStoreDir")
      bracketSnap (open nixSqliteDb) close $ \conn -> do
        let prefix = sd <> "/" <> hash
        vpRes <- liftIO $ query conn "select * from ValidPaths where path >= ? limit 1" (Only prefix)
        case vpRes of
          [vp] -> do
            refs <- fmap (sort . fmap fromOnly) $ liftIO $ query conn
              "select path from Refs join ValidPaths on reference = id where referrer = ?"
              (Only $ _validPath_id vp)
            case fingerprintVP vp refs of
              Left e -> do
                liftIO $ putStrLn $ "otherHandler in Left: " <> e
                modifyResponse $ setResponseStatus 500 "Error constructing fingerprint"
                writeText $ T.pack e
                getResponse >>= finishWith
              Right fingerprint -> do
                modifyResponse (setContentType "text/x-nix-narinfo")
                writeText $ T.pack $ printf "StorePath: %s\n" (_validPath_path vp)
                writeText $ T.pack $ printf "URL: nar/%s.nar\n" hash
                writeText $ T.pack $ printf "Compression: none\n"
                (hashType, base32hash) <- case mkBase32 (_validPath_hash vp) of
                  Left e -> error $ "Bad hash in nix sqlite DB: " <> e
                  Right h -> return h
                writeText $ T.pack $ printf "NarHash: %s:%s\n" hashType base32hash
                writeText $ T.pack $ printf "NarSize: %s\n" (maybe "NULL" show $ _validPath_narSize vp)
                when (length refs > 0) $
                  writeText $ T.pack $ printf "References: %s\n"
                    (T.unwords $ map stripPath refs)
                case _validPath_deriver vp of
                  Nothing -> return ()
                  Just deriver ->
                    writeText $ T.pack $ printf "Deriver: %s\n" (stripPath deriver)

                let secret = _nixCacheKey_secret $ _serverEnv_cacheKey se
                liftIO $ T.putStrLn $ "fingerprint: " <> fingerprint
                let sig = mkNixSig secret (T.encodeUtf8 fingerprint)
                writeText $ "Sig: " <> sig <> "\n"
          _ -> notFound "No such path."
    _ -> notFound "Cache server"

getNixConfigAttr :: String -> IO String
getNixConfigAttr attr = do
  let args = [ "--eval"
             , "--strict"
             , "<nix/config.nix>"
             , "-A"
             , attr
             ]
  -- TODO Suppress warning going to stderr
  readCreateProcess (proc nixInstantiate args) ""

cacheInfoHandler :: MonadSnap m => m ()
cacheInfoHandler = do
  modifyResponse (setContentType "text/plain")
  writeText =<< liftIO getCacheInfo

getCacheInfo :: IO Text
getCacheInfo = do
  sd <- read <$> getNixConfigAttr "nixStoreDir"
  return $ cacheInfo sd

cacheInfo :: Text -> Text
cacheInfo storeDir = T.unlines
    [ "StoreDir: " <> storeDir
    , "WantMassQuery: 1"
    , "Priority: 30"
    ]


storePathForHash :: Text -> Text -> IO (Maybe Text)
storePathForHash storeDir hash = bracket (open nixSqliteDb) close $ \conn -> do
  let prefix = storeDir <> "/" <> hash
  res <- query conn "select path from ValidPaths where path >= ? limit 1" (Only prefix)
  case res of
    [Only storePath] -> return $ Just storePath
    _ -> return Nothing

data NarType = NarZipped | NarPlain

narHandler :: MonadSnap m => Text -> m ()
narHandler file = do
  sd <- read <$> liftIO (getNixConfigAttr "nixStoreDir")
  let (hash, extension) = T.breakOn "." file
      mnt = case extension of
              ".nar.bz2" -> Just NarZipped
              ".nar" -> Just NarPlain
              _ -> Nothing
  msp <- liftIO $ storePathForHash sd hash
  case msp of
    Nothing -> notFound "No such path."
    Just sp -> do
      case mnt of
        Nothing -> notFound "File not found."
        Just nt -> do
          modifyResponse (setContentType "text/plain")
          let dumpCmd = T.pack $ printf "%s --dump '%s'" nixStore sp
          let cmd = case nt of
                      NarZipped -> dumpCmd <> " | " <> T.pack bzip2 <> " --fast"
                      NarPlain -> dumpCmd
          (_, out) <- T.shellStrict cmd ""
          writeBS out
