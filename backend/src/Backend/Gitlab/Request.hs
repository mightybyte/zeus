{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Backend.Gitlab.Request where

------------------------------------------------------------------------------
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import Reflex.FunctorMaybe
------------------------------------------------------------------------------
import Backend.Gitlab.Schema
------------------------------------------------------------------------------

getLabels
  :: Manager
  -> Text -- ^ Gitlab Token
  -> GitlabId -- ^ Project
  -> IO (Either String [Label])
getLabels mgr token pid = do
  req <- parseRequest $ projectApi pid "/labels"
  rsp <- httpLbs (applyAuth token req) mgr
  return $ eitherDecode $ responseBody rsp

gitlabApi :: String -> String
gitlabApi = (<>) "https://gitlab.com/api/v4"

projectsApi :: String -> String
projectsApi = gitlabApi . (<>) "/projects"

projectApi :: GitlabId -> String -> String
projectApi g = projectsApi . ("/" <>) . (show (unGitlabId g) <>)

applyAuth :: Text -> Request -> Request
applyAuth token req = req { requestHeaders = ("Private-Token", T.encodeUtf8 token) : requestHeaders req }

addPostData :: Request -> Value -> Request
addPostData r v = r { method = "POST", requestBody = RequestBodyLBS $ encode v }

createLabel
  :: Manager
  -> Text
  -> GitlabId
  -> Text
  -> Text
  -> IO (Either String Label)
createLabel mgr token gid name color = do
  req <- parseRequest $ projectApi gid "/labels"
  rsp <- flip httpLbs mgr $ applyAuth token $ flip urlEncodedBody req $ fmap (fmap T.encodeUtf8)
    [ ("name", name)
    , ("color", color)
    ]
  return $ eitherDecode $ responseBody rsp

modifyMergeRequestApproval
  :: Bool -- ^ approve
  -> Manager -- ^ HTTP client manager
  -> Text -- ^ Gitlab token
  -> GitlabId -- ^ Project id
  -> Int -- ^ MR number/iid
  -> Maybe GitHash -- ^ Hash to specifically approve
  -> IO (Either String Value)
modifyMergeRequestApproval approve mgr token gid mr hash = do
  req <- parseRequest $ projectApi gid $ "/merge_requests/" <> show mr <> (if approve then "/approve" else "/unapprove")
  rsp <- flip httpLbs mgr $ applyAuth token $
    urlEncodedBody (maybeToList $ fmap (("sha",) . T.encodeUtf8 . unGitHash) hash) req
  return $ eitherDecode $ responseBody rsp

approveMergeRequest
  :: Manager
  -> Text
  -> GitlabId
  -> Int
  -> GitHash
  -> IO (Either String Value)
approveMergeRequest mgr a b c d = modifyMergeRequestApproval True mgr a b c (Just d)

unapproveMergeRequest
  :: Manager
  -> Text
  -> GitlabId
  -> Int
  -> Maybe GitHash
  -> IO (Either String Value)
unapproveMergeRequest = modifyMergeRequestApproval False

getMergeRequest
  :: Manager
  -> Text -- ^ Token
  -> GitlabId -- ^ Project ID
  -> Int -- ^ Merge Request IID
  -> IO (Either String Value)
getMergeRequest mgr token pid iid = do
  req <- parseRequest $ projectApi pid $ "/merge_requests/" <> show iid
  rsp <- flip httpLbs mgr $ applyAuth token req
  return $ eitherDecode $ responseBody rsp

getMergeRequestStatus
  :: Manager
  -> Text -- ^ Token
  -> GitlabId -- ^ Project ID
  -> Int -- ^ Merge Request IID
  -> IO (Either String Text)
getMergeRequestStatus mgr token pid iid = do
  rsp <- getMergeRequest mgr token pid iid
  case rsp of
    Left err -> return $ Left err
    Right v -> case v ^? key "state" . _String of
      Nothing -> return $ Left "getMergeRequestStatus: Couldn't parse merge request object"
      Just state -> return $ Right state

getOpenMergeRequestSubset
  :: Manager
  -> Text -- ^ Token
  -> GitlabId -- ^ Project ID
  -> NonEmpty Int -- ^ Merge Request IIDs
  -> IO (Either String [Int])
getOpenMergeRequestSubset mgr token pid iids = do
  req <- parseRequest $ projectApi pid "/merge_requests"
  rsp <- flip httpLbs mgr $ applyAuth token req
    { queryString = T.encodeUtf8 $ T.pack $ mconcat $
      "?state=opened" : fmap (\x -> "&iids[]=" <> show x) (NE.toList iids)
    }
  return $ case eitherDecode $ responseBody rsp of
    Left err -> Left err
    Right (vs :: [Value]) -> Right $ fmapMaybe iid vs -- TODO: What should we do on parse errors here?
  where
    iid :: Value -> Maybe Int
    iid v = fmap ((floor :: Double -> Int) . realToFrac) $ v ^? key "iid" . _Number
