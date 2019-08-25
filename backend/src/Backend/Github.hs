{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Github where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Lens
import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map as M
import           Data.String.Conv
import           Data.Text (Text)
import           Data.Text.Encoding
import qualified Data.Vector as V
import           Database.Beam
import           Database.Beam.Sqlite
import           GitHub.Data
import           GitHub.Data.Name
import qualified GitHub.Data.Webhooks.Events as GW
import qualified GitHub.Data.Webhooks.Payload as GW
import           GitHub.Data.Webhooks.Validate
import           GitHub.Endpoints.Repos.Webhooks
import           Snap.Core
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.Db
import           Backend.Schedule
import           Backend.Types.ServerEnv
import           Common.Route
import           Common.Types.ConnectedAccount
import           Common.Types.Repo
import           Common.Types.RepoBuildInfo
------------------------------------------------------------------------------

githubHandler :: ServerEnv -> Snap ()
githubHandler env = do
    req <- getRequest
    body <- readRequestBody 1000000
    let event = getHeader "X-GitHub-Event" req
        sig = getHeader "X-Hub-Signature" req
    let isValid = isValidPayload (_serverEnv_secretToken env)
                                 (decodeUtf8 <$> sig) (toS body)
    if isValid
      then do
        liftIO $ putStrLn $ "Payload successfully validated: event=" ++ maybe "" toS event
        liftIO $ handleValidatedHook env event body
      else liftIO $ putStrLn "Payload not valid!"

handleValidatedHook :: ServerEnv -> Maybe ByteString -> LB.ByteString -> IO ()
handleValidatedHook env event body = do
  res <- runExceptT $ do
    rbi <- hoistEither $ case event of
      Just "pull_request" -> do
        let decodeErr e = Left $ "Error decoding push message: " ++ e
            mkPrMsg rbi = Left $
              printf "Ignoring pull request message on %s/%s commit %s"
                     (_rbi_repoNamespace rbi) (_rbi_repoName rbi)
                     (_rbi_commitHash rbi)
        either decodeErr (mkPrMsg . handlePR) $ eitherDecodeStrict (toS body)
      Just "push" -> mkPushRBI =<< eitherDecodeStrict (toS body)
      _ -> Left "Event not supported"

    ras <- lift $ runBeamSqlite (_serverEnv_db env) $
      runSelectReturningList $ select $ do
        account <- all_ (_ciDb_connectedAccounts ciDb)
        repo <- all_ (_ciDb_repos ciDb)
        guard_ (_repo_name repo ==. val_ (_rbi_repoName rbi) &&.
                _repo_namespace repo ==. val_ (_rbi_repoNamespace rbi) &&.
                _connectedAccount_provider account ==. val_ GitHub &&.
                _connectedAccount_id account ==. repo ^. repo_accessAccount)
        return repo
    liftIO $ case ras of
      [] -> putStrLn "Error building push request: repo doesn't exist"
      [r] -> scheduleBuild env rbi r
      _ -> putStrLn "Error building push request: multiple repos match (this shouldn't happen)"
  case res of
    Left e -> putStrLn e
    Right _ -> return ()

handlePR :: GW.PullRequestEvent -> RepoBuildInfo
handlePR prEvent = do
    RepoBuildInfo
      (GW.whRepoName repo)
      (either GW.whSimplUserName GW.whUserLogin $ GW.whRepoOwner repo)
      RepoPullRequest (GW.getUrl $ GW.whRepoSshUrl repo)
      (GW.getUrl $ GW.whRepoCloneUrl repo)
      (GW.whPullReqTargetRef prHead)
      (GW.whPullReqTargetSha prHead)
      "" -- TODO Haven't found how to get the commit message from github yet
      (GW.whUserLogin $ GW.evPullReqSender prEvent)
      (Just $ GW.getUrl $ GW.whUserAvatarUrl $ GW.evPullReqSender prEvent)
  where
    pr = GW.evPullReqPayload prEvent
    repo = GW.evPullReqRepo prEvent
    prHead = GW.whPullReqHead pr


mkPushRBI :: GW.PushEvent -> Either String RepoBuildInfo
mkPushRBI pe = do
    sha <- note ("PushEvent didn't have git hash:\n" ++ show pe) $
      GW.evPushHeadSha pe
    pure $ RepoBuildInfo
      (GW.whRepoName repo)
      (either GW.whSimplUserName GW.whUserLogin $ GW.whRepoOwner repo)
      RepoPush
      (GW.getUrl $ GW.whRepoSshUrl repo)
      (GW.getUrl $ GW.whRepoCloneUrl repo)
      (GW.evPushRef pe)
      sha
      "" -- TODO Haven't found how to get the commit message from github yet
      (GW.whUserLogin $ GW.evPushSender pe)
      (Just $ GW.getUrl $ GW.whUserAvatarUrl $ GW.evPushSender pe)
  where
    repo = GW.evPushRepository pe

data SslSettings
  = AllowInsecure
    -- ^ Allow github to connect to this server insecurely
  | ForceSSL
    -- ^ Force github to use SSL.  (Requires setting up a certificate.)
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

------------------------------------------------------------------------------
-- | Adds the GitHub webhook necessary for allowing this server to do CI for a
-- particular repo.
setupGithubWebhook
  :: Text
    -- ^ Domain (optionally with port) that your hook will be served on
  -> Auth
    -- ^ Token authenticating you with GitHub so the webhook can be created.
    -- This can be a personal access token created at:
    -- https://github.com/settings/tokens
  -> Text
    -- ^ Owner of the repo (username or organization)
  -> Text
    -- ^ Name of the repo
  -> Text
    -- ^ Secret that github will use to authenticate with this CI server
  -> SslSettings
  -> IO (Either Error RepoWebhook)
setupGithubWebhook domain auth owner repo secret sslSettings = do
    let url = domain <> "/" <> githubHookPath
    let cfg = M.fromList
          [ ("url", url)
          , ("content_type", "json")
          , ("secret", secret)

          -- TODO FIXME Change this once done debugging
          , ("insecure_ssl", ssl)
          ]
        events = Just $ V.fromList
          [ WebhookPushEvent
          , WebhookPullRequestEvent
          , WebhookStatusEvent
          ]
    eh <- webhooksFor' auth (N owner) (N repo)
    case eh of
      Left e -> return $ Left e
      Right hooks -> do
        mapM_ print hooks
        case filter (\h -> hookUrl h == Just url) (V.toList hooks) of
          (h:_) -> return $ Right h
          _ -> do
            createRepoWebhook' auth (N owner) (N repo) $
                 NewRepoWebhook "web" cfg events (Just True)
  where
    ssl = case sslSettings of
            AllowInsecure -> "1"
            ForceSSL -> "0"

hookUrl :: RepoWebhook -> Maybe Text
hookUrl hook = M.lookup "url" m
  where
    m = repoWebhookConfig hook
