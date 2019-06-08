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
import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map as M
import           Data.String.Conv
import           Data.Text (Text)
import           Data.Text.Encoding
import qualified Data.Vector as V
import           GitHub.Data
import           GitHub.Data.Name
import qualified GitHub.Data.Webhooks.Events as GW
import qualified GitHub.Data.Webhooks.Payload as GW
import           GitHub.Data.Webhooks.Validate
import           GitHub.Endpoints.Repos.Webhooks
import           Snap.Core
import           Text.Printf
------------------------------------------------------------------------------
import           Backend.Schedule
import           Backend.Types.ServerEnv
import           Common.Route
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
              printf "Ignoring pull request message on %s commit %s"
                     (_rbi_repoFullName rbi) (_rbi_commitHash rbi)
        either decodeErr (mkPrMsg . handlePR) $ eitherDecodeStrict (toS body)
      Just "push" -> mkPushRBI =<< eitherDecodeStrict (toS body)
      _ -> Left "Event not supported"

    ExceptT $ scheduleBuild env rbi
  case res of
    Left e -> putStrLn e
    Right _ -> return ()

handlePR :: GW.PullRequestEvent -> RepoBuildInfo
handlePR pre = do
    RepoBuildInfo (GW.whRepoName repo) (GW.whRepoFullName repo)
      RepoPullRequest (GW.getUrl $ GW.whRepoSshUrl repo)
      (GW.getUrl $ GW.whRepoCloneUrl repo)
      (GW.whPullReqTargetRef prHead)
      (GW.whPullReqTargetSha prHead)
      "" -- TODO Haven't found how to get the commit message from github yet
      (GW.whUserLogin $ GW.evPullReqSender pre)
      (Just $ GW.getUrl $ GW.whUserAvatarUrl $ GW.evPullReqSender pre)
  where
    pr = GW.evPullReqPayload pre
    repo = GW.evPullReqRepo pre
    prHead = GW.whPullReqHead pr


mkPushRBI :: GW.PushEvent -> Either String RepoBuildInfo
mkPushRBI pe = do
    sha <- note ("PushEvent didn't have git hash:\n" ++ show pe) $
      GW.evPushHeadSha pe
    pure $ RepoBuildInfo (GW.whRepoName repo) (GW.whRepoFullName repo)
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
  :: String
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
    let url = toS domain <> "/" <> githubHookPath
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
