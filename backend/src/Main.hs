{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TQueue
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.RNG
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
import           GitHub.Auth
import           Options.Applicative
import           Snap.Core
import           Snap.Http.Server
import           System.Directory
import           System.Environment
import           Text.PrettyPrint.ANSI.Leijen (string)
import           Text.Printf
------------------------------------------------------------------------------
import           SimpleCI
import           SimpleCI.Build
import           SimpleCI.Types
import           SimpleCI.Types.Account
import           SimpleCI.Types.Repo
------------------------------------------------------------------------------

data ServerConfig = ServerConfig
  { _serveConfig_myHostname :: Text
  , _serveConfig_myPort :: Int
  } deriving (Eq,Ord,Show,Read)

serveConfigParser = ServerConfig <$> hostOpt <*> portOpt
  where
    hostOpt = strOption $ mconcat
      [ long "host"
      , help "The external hostname to reach this CI server"
      ]
    portOpt = option auto $ mconcat
      [ long "port"
      , short 'p'
      , help "The external port to reach this CI server"
      ]

data CiConfig = CiConfig
  { _ciConfig_serveConfig :: Maybe ServerConfig
  , _ciConfig_cmd :: Maybe Cmd
  } deriving (Eq,Ord,Show,Read)


configParser = CiConfig <$> optional serveConfigParser <*> optional cmdParser

testPlayground = do
  conn <- open dbConnectInfo
  accountRepos <- runBeamSqlite conn $
    runSelectReturningList $ select $ do
      repo <- all_ (ciDb ^. ciDb_repos)
      --account <- all_ (ciDb ^. ciDb_accounts)
      guard_ (_repo_fullName repo ==. val_ "mightybyte/test-project")
      --guard_ (account ^. account_id ==. repo ^. repo_owner)
      --return (account, repo)
      return repo

  print accountRepos

main :: IO ()
main = do
    CiConfig mscfg cmd <- customExecParser p opts
    case cmd of
      Just MakeDb -> do
        conn <- open dbConnectInfo
        createDb conn
        populateDb conn
      Just Test -> testPlayground
      Nothing -> case mscfg of
        Nothing -> putStrLn "Must specify hostname and port to run server"
        Just cfg -> runServer cfg
  where
    opts = info (configParser <**> helper) mods
    mods = progDesc "Nix Development Tool"
    p = prefs showHelpOnEmpty

runServer :: ServerConfig -> IO ()
runServer cfg = do
  mNixShell <- lookupEnv "IN_NIX_SHELL"
  when (isJust mNixShell) $ error "Error: Cannot run server in a nix shell!"
  conn <- open dbConnectInfo

  accountRepos <- runBeamSqlite conn $
    runSelectReturningList $ select $ do
      account <- all_ (ciDb ^. ciDb_accounts)
      repo <- all_ (ciDb ^. ciDb_repos)
      guard_ (account ^. account_id ==. repo ^. repo_owner)
      return (account, repo)

  ghSecret <- getGitHubSecret
  putStrLn $ "Setting up these account repos: " <> show accountRepos
  mapM_ (setupHook cfg ghSecret) accountRepos

  buildQueue <- atomically newTQueue
  forkIO (buildThread conn buildQueue)
  let snapCfg = defaultConfig
        & setHostname (toS $ _serveConfig_myHostname cfg)
        & setPort (_serveConfig_myPort cfg)
  httpServe snapCfg $ do
    let hhd = HookHandlerData ghSecret buildQueue conn
    route $ endpoints hhd

setupHook :: ServerConfig -> Text -> (Account, Repo) -> IO ()
setupHook (ServerConfig host port) ghSecret (a,r) = do
  let an = _account_name a
  let rn = _repo_name r
  printf "Setting up webhook for %s/%s\n" an rn
  let domain = T.unpack host <> if port == 80 then mempty else ":" <> show port
  erw <- setupWebhook domain
           (OAuth $ toS $ _account_accessToken a)
           an rn ghSecret AllowInsecure
  case erw of
    Left e -> putStrLn $ "Error creating webhook: " ++ (show e)
    Right rw-> do
      putStrLn "Created webhook"
      print rw

endpoints :: HookHandlerData -> [(ByteString, Snap ())]
endpoints hhd =
  [ ("ping", writeText "PONG\nPONG\nPONG\n")
  , (hookPath, hookHandler hhd)
  ]
