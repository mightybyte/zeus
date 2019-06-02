{-# LANGUAGE TemplateHaskell #-}
module Backend.Gitlab.Schema where

import Data.Aeson
import Data.Aeson.TH
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T

newtype GitlabId = GitlabId { unGitlabId :: Int64 }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GitlabId where
  parseJSON a = GitlabId <$> parseJSON a

instance ToJSON GitlabId where
  toJSON (GitlabId a) = toJSON a

newtype GitHash = GitHash { unGitHash :: Text }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GitHash where
  parseJSON a = GitHash <$> parseJSON a

instance ToJSON GitHash where
  toJSON (GitHash a) = toJSON a

shortHash :: GitHash -> Text
shortHash = T.take 8 . unGitHash

data Project = Project
  { _project_id :: Maybe GitlabId
  , _project_name :: Text
  , _project_description :: Maybe Text
  , _project_web_url ::Maybe  Text
  , _project_avatar_url ::Maybe  Text
  , _project_git_ssh_url :: Text
  , _project_git_http_url :: Text
  , _project_namespace :: Text
  , _project_visibility_level :: Int
  , _project_path_with_namespace :: Text
  , _project_default_branch :: Text
  , _project_homepage ::Maybe  Text
  , _project_url :: Text
  , _project_ssh_url :: Text
  , _project_http_url :: Text
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 9
  , omitNothingFields = True
  } ''Project

data Repository = Repository
  { _repository_name :: Text
  , _repository_url :: Text
  , _repository_description :: Maybe Text
  , _repository_homepage ::Maybe  Text
  , _repository_git_http_url :: Maybe Text
  , _repository_git_ssh_url :: Maybe Text
  , _repository_visibility_level :: Maybe Int
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 12
  , omitNothingFields = True
  } ''Repository

data Author = Author
  { _author_name :: Text
  , _author_email :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions { fieldLabelModifier = drop 8 } ''Author

data Commit = Commit
  { _commit_id :: GitHash
  , _commit_message :: Text
  , _commit_timestamp :: Text
  , _commit_url :: Text
  , _commit_author :: Author
  , _commit_added :: [Text]
  , _commit_modified :: [Text]
  , _commit_removed :: [Text]
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 8
  , omitNothingFields = True
  } ''Commit

data CommitSummary = CommitSummary
   { _commitSummary_id :: GitHash
   , _commitSummary_message :: Text
   , _commitSummary_timestamp :: Text
   , _commitSummary_url :: Text
   , _commitSummary_author :: Author
   }
   deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 15
  } ''CommitSummary


data Push = Push
  { _push_object_kind :: Text
  , _push_before :: GitHash
  , _push_after :: GitHash
  , _push_ref :: Text
  , _push_checkout_sha :: GitHash
  , _push_user_id :: GitlabId
  , _push_user_name :: Text
  , _push_user_email :: Maybe Text
  , _push_user_avatar :: Maybe Text
  , _push_project_id :: GitlabId
  , _push_project :: Project
  , _push_repository :: Repository
  , _push_commits :: [Commit]
  , _push_total_commits_count :: Int
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions { fieldLabelModifier = drop 6 } ''Push

data User = User
  { _user_name :: Text
  , _user_username :: Text
  , _user_avatar_url :: Text
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 6
  } ''User

data ObjectAttributes = ObjectAttributes
  { _objectAttributes_id :: GitlabId
  , _objectAttributes_target_branch :: Text
  , _objectAttributes_source_branch :: Text
  , _objectAttributes_source_project_id :: GitlabId
  , _objectAttributes_author_id :: GitlabId
  , _objectAttributes_assignee_id :: Maybe GitlabId
  , _objectAttributes_title :: Text
  , _objectAttributes_created_at :: Text
  , _objectAttributes_updated_at :: Text
  , _objectAttributes_milestone_id :: Maybe GitlabId
  , _objectAttributes_state :: Text
  , _objectAttributes_merge_error :: Maybe Text
  , _objectAttributes_merge_status :: Text
  , _objectAttributes_target_project_id :: GitlabId
  , _objectAttributes_iid :: Int
  , _objectAttributes_description :: Text
  , _objectAttributes_source :: Project
  , _objectAttributes_target :: Project
  , _objectAttributes_last_commit :: CommitSummary
  , _objectAttributes_work_in_progress :: Bool
  , _objectAttributes_url :: Text
  , _objectAttributes_action :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 18
  } '' ObjectAttributes

data Label = Label
  { _label_id :: GitlabId
  , _label_name :: Text
  , _label_color :: Text
  , _label_description :: Maybe Text
  , _label_open_issues_count :: Int
  , _label_closed_issues_count :: Int
  , _label_open_merge_requests_count :: Int
  , _label_subscribed :: Bool
  , _label_priority :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 7
  } ''Label

data MergeRequest = MergeRequest
  { _mergeRequest_object_kind :: Text
  , _mergeRequest_user :: User
  , _mergeRequest_project :: Project
  , _mergeRequest_repository :: Repository
  , _mergeRequest_object_attributes :: ObjectAttributes
  , _mergeRequest_labels :: [Label]
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON defaultOptions
  { fieldLabelModifier = drop 14
  } ''MergeRequest
