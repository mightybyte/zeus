{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Frontend.AppState where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Witherable as W
import           Database.Beam (Table, primaryKey)
import           GHC.Generics
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Common.Api
import           Common.Types.BuildJob
import           Common.Types.ConnectedAccount
import           Common.Types.Repo
import           Frontend.Common ()
------------------------------------------------------------------------------

data AppTriggers = AppTriggers
    { _trigger_getAccounts :: Batch ()
    , _trigger_connectAccount :: Batch (ConnectedAccountT Maybe)
    , _trigger_delAccounts :: Batch ConnectedAccountId
    , _trigger_getRepos :: Batch ()
    , _trigger_addRepo :: Batch (RepoT Maybe)
    , _trigger_delRepos :: Batch RepoId
    , _trigger_getJobs :: Batch ()
    , _trigger_cancelJobs :: Batch (PrimaryKey BuildJobT Identity)
    , _trigger_rerunJobs :: Batch (PrimaryKey BuildJobT Identity)
    } deriving Generic

instance Semigroup AppTriggers where
  (AppTriggers ga1 ca1 da1 gr1 ar1 dr1 gj1 cj1 rj1) <> (AppTriggers ga2 ca2 da2 gr2 ar2 dr2 gj2 cj2 rj2) = AppTriggers
    (ga1 <> ga2)
    (ca1 <> ca2)
    (da1 <> da2)
    (gr1 <> gr2)
    (ar1 <> ar2)
    (dr1 <> dr2)
    (gj1 <> gj2)
    (cj1 <> cj2)
    (rj1 <> rj2)

instance Monoid AppTriggers where
    mempty = AppTriggers
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
    mappend = (<>)

makeLenses ''AppTriggers

trigger
    :: (Reflex t, EventWriter t AppTriggers m)
    => Lens' AppTriggers (Batch a)
    -> Event t a
    -> m ()
trigger l e = triggerBatch l $ batchOne <$> e

triggerBatch
    :: (Reflex t, EventWriter t AppTriggers m)
    => Lens' AppTriggers (Batch a)
    -> Event t (Batch a)
    -> m ()
triggerBatch l e = tellEvent $ (\as -> set l as mempty) <$> e

type BeamMap f a = Map (PrimaryKey a f) (a f)

data AppState t = AppState
    { _as_accounts :: Dynamic t (BeamMap Identity ConnectedAccountT)
    , _as_jobs :: Dynamic t (BeamMap Identity BuildJobT)
    , _as_repos :: Dynamic t (BeamMap Identity RepoT)
    , _as_serverAlert :: Event t Text
    } deriving Generic

squash
  :: (W.Filterable f, Foldable t)
  => (a1 -> t a2)
  -> f a1
  -> f (t a2)
squash f = W.filter (not . null) . fmap f

stateManager
    :: (DomBuilder t m, MonadHold t m, Prerender js t m)
    => Text
    -> Event t AppTriggers
    -> m (AppState t)
stateManager route ft = do
    let upEvent = mergeWith (++) $ map (fmap (:[]))
          [ Up_ListAccounts <$ fmapMaybe (listToMaybe . _trigger_getAccounts) ft
          , Up_ConnectAccount <$> squash _trigger_connectAccount ft
          , Up_DelAccounts <$> squash _trigger_delAccounts ft
          , Up_ListRepos <$ fmapMaybe (listToMaybe . _trigger_getRepos) ft
          , Up_AddRepo <$> squash _trigger_addRepo ft
          , Up_DelRepos <$> squash _trigger_delRepos ft
          , Up_GetJobs <$ fmapMaybe (listToMaybe . _trigger_getJobs) ft
          , Up_CancelJobs <$> squash _trigger_cancelJobs ft
          , Up_RerunJobs <$> squash _trigger_rerunJobs ft
          ]
    let cfg = WebSocketConfig upEvent never True []
    ws <- startWebsocket route cfg
    let downEvent = _webSocket_recv ws
    accounts <- holdDyn mempty $
      fmapMaybe (fmap listToBeamMap . preview _Down_ConnectedAccounts) downEvent
    jobs <- holdDyn mempty $ fmapMaybe (fmap listToBeamMap . preview _Down_Jobs) downEvent
    repos <- holdDyn mempty $ fmapMaybe (fmap listToBeamMap . preview _Down_Repos) downEvent
    let serverAlert = fmapMaybe (preview _Down_Alert) downEvent

    return $ AppState accounts jobs repos serverAlert

listToBeamMap :: (Table a, Ord (PrimaryKey a f)) => [a f] -> BeamMap f a
listToBeamMap = M.fromList . map (\a -> (primaryKey a, a))

startWebsocket
  :: (DomBuilder t m, Prerender js t m)
  => Text
  -> WebSocketConfig t Up
  -> m (RawWebSocket t Down)
startWebsocket siteRoute wsCfg = do
  res <- prerender (pure neverWebSocket) $ do
    let (scheme,rest) = T.breakOn "://" siteRoute
        wsScheme = case scheme of
                     "http" -> "ws"
                     "https" -> "wss"
                     _ -> error $ "Invalid scheme: " ++ T.unpack scheme
    RawWebSocket r o e c <- jsonWebSocket (wsScheme <> rest <> "/ws") wsCfg
    return (RawWebSocket (fmapMaybe id r) o e c)
  let r = switch $ current $ _webSocket_recv <$> res
  let o = switch $ current $ _webSocket_open <$> res
  let e = switch $ current $ _webSocket_error <$> res
  let c = switch $ current $ _webSocket_close <$> res
  return $ RawWebSocket r o e c

--   = RawWebSocket { _webSocket_recv :: Event t a
--                  , _webSocket_open :: Event t ()
--                  , _webSocket_error :: Event t () -- eror event does not carry any data and is always
--                                                   -- followed by termination of the connection
--                                                   -- for details see the close event
--                  , _webSocket_close :: Event t ( Bool -- wasClean
--                                                , Word -- code
--                                                , Text -- reason
--                                                )
--                  }

neverWebSocket :: Reflex t => RawWebSocket t a
neverWebSocket = RawWebSocket never never never never
