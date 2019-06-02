{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend.Gitlab where

import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import Database.Groundhog.Postgresql
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Focus.Gitlab.Schema
import Rhyolite.Backend.Schema
import Snap

import Common.Schema
import Backend.Autobuild
import Backend.Common
import Backend.Merge

gitlabHandler :: CI -> Snap ()
gitlabHandler app = do
  let db = _ci_db app
  checkToken $ _gitlabConfig_secret $ _ci_gitlab app
  eventHeader <- getHeader "X-Gitlab-Event" <$> getRequest
  body <- readRequestBody 1048576 -- TODO what should this number be?
  case eventHeader of
    Just "Merge Request Hook" -> gitlabMergeRequestHandler app $ eitherDecode body
    Just "Push Hook" -> do
      let mp :: Maybe Push = decode body
      forM_ mp $ \p -> liftIO $ do
        dbTransaction db $ scheduleBuildGitlab p
        checkOutstandingMergeRequests app p
    _ -> err404

scheduleBuildGitlab
  :: PersistBackend m
  => Push
  -> m ()
scheduleBuildGitlab gitlabPush = do
  pid <- toId <$> insert gitlabPush
  gpid <- fmap toId $ insert $ fromGitlabPush gitlabPush
  insert_ $ GitlabPush
    { _gitlabPush_gitPush = gpid
    , _gitlabPush_raw = pid
    }
  scheduleBuild gpid Nothing

checkToken :: Text -> Snap ()
checkToken secret = do
  let secret' = T.encodeUtf8 secret
  tokenHeaders <- getHeader "X-Gitlab-Token" <$> getRequest
  guard (Just secret' == tokenHeaders)

pushMessage :: Push -> Text
pushMessage p =
  let num = _push_total_commits_count p
      maxCommitMessages = 10
      commits = take maxCommitMessages $ _push_commits p
      commitMessages = concatMap (\c -> ["—", _commit_message c]) commits
      extraCommits = if num > maxCommitMessages
                       then ["...and " <> T.pack (show (num - maxCommitMessages)) <> " more."]
                       else []
      content = [ _push_user_name p
                , " pushed "
                , T.pack (show num)
                , " commit"
                , if num == 1 then "" else "s"
                , " to "
                , _push_ref p
                , " of "
                , _repository_name (_push_repository p)
                ]

      hash =  [ "Hash: "
              , unGitHash $ _push_after p
              ]
  in  T.unlines $ (mconcat <$>
        [ content
        , hash
        , commitMessages
        , extraCommits
        ]) ++ map _commit_url commits

gitlabMergeRequestHandler :: CI -> Either String MergeRequest -> Snap ()
gitlabMergeRequestHandler app mmr = case mmr of
  Left parseErr -> error $ "MR: Couldn't parse merge request payload: " <> parseErr
  Right mr | mergeRequestIsActionable mr -> do
    (obsolete, mrid) <- dbTransaction (_ci_db app) $ insertMergeRequest mr
    dbTransaction (_ci_db app) $ scheduleMerge mrid
    liftIO $ forM_ (NE.nonEmpty obsolete) $ unapproveMergeRequests app
  Right mr |  _objectAttributes_state (_mergeRequest_object_attributes mr) `elem` ["merged", "closed"] ->
    void $ dbTransaction (_ci_db app) $ insertMergeRequest mr -- Insert this so that we know not to try to build this MR anymore
  Right mr -> liftIO $ putStrLn $ unwords
    [ "MR: Merge request update not actionable:"
    , "IID:"
    , show (_objectAttributes_iid $ _mergeRequest_object_attributes mr)
    , "State:"
    , show (_objectAttributes_state $ _mergeRequest_object_attributes mr)
    , "Action:"
    , show (_objectAttributes_action $ _mergeRequest_object_attributes mr)
    ]
