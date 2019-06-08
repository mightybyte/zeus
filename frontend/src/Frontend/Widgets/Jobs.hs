{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Widgets.Jobs where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Ord
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Reflex.Network
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
import qualified Reflex.Dom.SemanticUI as SemUI
import           Text.Printf
------------------------------------------------------------------------------
import           Common.Types.BuildJob
import           Common.Types.JobStatus
import           Common.Types.RepoBuildInfo
import           Frontend.App
import           Frontend.AppState
import           Frontend.Widgets.Common
------------------------------------------------------------------------------

jobsWidget
  :: MonadAppIO r t m
  => m ()
jobsWidget = mdo
  as <- ask

  let jobMap = _as_jobs as
  let widget m = if M.null m
        then genericPlaceholder "Job history empty"
        else jobsList jobMap
  _ <- dyn (widget <$> jobMap)
  return ()

jobDuration :: BuildJob -> Maybe NominalDiffTime
jobDuration bj = do
  start <- _buildJob_startedAt bj
  end <- _buildJob_endedAt bj
  pure $ diffUTCTime end start

jobsList
  :: (MonadApp r t m, MonadIO m, MonadIO (Performable m))
  => Dynamic t (BeamMap Identity BuildJobT)
  -> m ()
jobsList as = do
  let mkField f _ v = el "td" $ do
        _ <- f v
        return never
  _ <- genericTableG def (M.mapKeys Down <$> as)
    [ ("Status", (\_ v -> el "td" $ dynStatusWidget (_buildJob_status <$> v)))
    , ("ID", mkField $ dynText . fmap (tshow . _buildJob_id))
    , ("Repository", \_ v -> el "td" (repoColumnWidget v) >> return never)
    , ("Git Ref", mkField $ dynText . fmap (_rbi_gitRef . _buildJob_repoBuildInfo))
    , ("Commit Hash", \_ v -> el "td" (commitWidget v) >> return never)
    , ("Author", \_ v -> el "td" (authorWidget v) >> return never)
    , ("Time", mkField dynJobTimeWidget)
    , ("", (\(Down k) v -> elClass "td" "right aligned collapsing" $
             cancelOrRerun k (_buildJob_status <$> v)))
    ]
  return ()

cancelOrRerun :: MonadApp r t m => BuildJobId -> Dynamic t JobStatus -> m (Event t ())
cancelOrRerun k dj = do
    _ <- networkView $ ffor dj $ \case
      JobInProgress -> cancelButton k
      JobCanceled -> rerunButton k
      JobTimedOut -> rerunButton k
      JobVanished -> rerunButton k
      JobFailed -> rerunButton k
      _ -> return ()
    return never

cancelButton :: MonadApp r t m => BuildJobId -> m ()
cancelButton k = do
    (e,_) <- elAttr' "span" ("class" =: "clickable" <>
                             "data-tooltip" =: "Cancel build" <>
                             "data-position" =: "bottom right"
                            ) $
      elAttr' "i" ("class" =: "cancel icon") blank
    triggerBatch trigger_cancelJobs $ [k] <$ domEvent Click e
    return ()

rerunButton :: MonadApp r t m => BuildJobId -> m ()
rerunButton k = do
    (e,_) <- elAttr' "span" ("class" =: "clickable" <>
                             "data-tooltip" =: "Re-run build" <>
                             "data-position" =: "bottom right"
                            ) $
      elAttr' "i" ("class" =: "redo icon") blank
    triggerBatch trigger_rerunJobs $ [k] <$ domEvent Click e
    return ()

repoColumnWidget
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t BuildJob
  -> m ()
repoColumnWidget dj = do
  let drbi = _buildJob_repoBuildInfo <$> dj
  let mkAttrs rbi = ("href" =: rbiRepoLink rbi <> "target" =: "_blank")
  elDynAttr "a" (mkAttrs <$> drbi) $ dynText (_rbi_repoFullName <$> drbi)

authorWidget
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t BuildJob
  -> m ()
authorWidget dj = do
    _ <- networkView $ mkAvatar . _rbi_pushAvatar . _buildJob_repoBuildInfo <$> dj
    text " "
    dynText $ _rbi_pushUser . _buildJob_repoBuildInfo <$> dj
  where
    mkAvatar Nothing = blank
    mkAvatar (Just url) = elAttr "img" ("src" =: url <> "class" =: "avatar") blank

commitWidget
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t BuildJob
  -> m ()
commitWidget dj = do
  let drbi = _buildJob_repoBuildInfo <$> dj
  let mkAttrs rbi = ("href" =: rbiCommitLink rbi <> "target" =: "_blank")
  elDynAttr "a" (mkAttrs <$> drbi) $ dynText (T.take 7 . _rbi_commitHash <$> drbi)

dynJobTimeWidget
  :: (DomBuilder t m, PostBuild t m, TriggerEvent t m, MonadHold t m, PerformEvent t m, MonadIO m, MonadIO (Performable m), MonadFix m)
  => Dynamic t BuildJob
  -> m ()
dynJobTimeWidget dj = do
  t <- liftIO getCurrentTime
  el "div" $ do
    dti <- clockLossy 0.5 t
    let showElapsed ti j =
              case jobDuration j of
                Just d -> formatDiffTime d
                Nothing -> do
                  if _buildJob_status j /= JobInProgress
                    then "--:--"
                    else
                      maybe "--:--" formatDiffTime $ do
                        s <- _buildJob_startedAt j
                        return $ diffUTCTime (_tickInfo_lastUTC ti) s
    icon (Static "clock") def
    dynText $ showElapsed <$> dti <*> dj
  let mkAttrs j = ("data-tooltip" =: maybe "" tshow (view buildJob_startedAt j) <>
                   "data-position" =: "bottom left")
  elDynAttr "div" (mkAttrs <$> dj) $ do
    icon (Static "calendar") def
    let f = maybe (text "") pastTimeWiget . view buildJob_startedAt
    void $ dyn $ f <$> dj

formatDiffTime :: NominalDiffTime -> Text
formatDiffTime t = T.pack $
    if h > 0
      then printf "%d:%02d:%02d" h m (round s :: Int)
      else printf "%02d:%02d" m (round s :: Int)
  where
    h :: Int
    h = truncate $ t / oneHour
    m :: Int
    m = truncate $ t / oneMinute
    s :: NominalDiffTime
    s = t - (fromIntegral h * oneHour) - (fromIntegral m * oneMinute)

pastTimeWiget
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadFix m, MonadIO m, MonadIO (Performable m))
  => UTCTime
  -> m ()
pastTimeWiget t = do
  ti <- clockLossy 5 t
  let calcDiff lastTick = diffUTCTime (_tickInfo_lastUTC lastTick) t
  dynText $ diffTimeToRelativeEnglish . calcDiff <$> ti

dynPastTimeWiget
  :: MonadAppIO r t m
  => Dynamic t UTCTime
  -> m ()
dynPastTimeWiget t = void $ dyn $ pastTimeWiget <$> t

showInt :: Int -> Text
showInt = tshow

diffTimeToRelativeEnglish :: NominalDiffTime -> Text
diffTimeToRelativeEnglish delta
  | delta < oneMinute = "Just now"
  | delta < oneMinute * 2 = "1 minute ago"
  | delta < oneHour = showInt (round $ delta / oneMinute) <> " minutes ago"
  | delta < oneHour * 2 = "an hour ago"
  | delta < oneDay = showInt (round $ delta / oneHour) <> " hours ago"
  | delta < oneDay * 2 = "1 day ago"
  | delta < oneWeek = showInt (round $ delta / oneDay) <> " days ago"
  | delta < oneWeek * 2 = "1 week ago"
  | delta < oneMonth = showInt (round $ delta / oneWeek) <> " weeks ago"
  | delta < oneMonth * 2 = "1 month ago"
  | delta < oneYear = showInt (round $ delta / oneMonth) <> " months ago"
  | delta < oneYear * 2 = "a year ago"
  | otherwise = showInt (round $ delta / oneYear) <> " years ago"

oneMinute :: NominalDiffTime
oneMinute = 60
oneHour :: NominalDiffTime
oneHour = oneMinute * 60
oneDay :: NominalDiffTime
oneDay = oneHour * 24
oneWeek :: NominalDiffTime
oneWeek = oneDay * 7
oneMonth :: NominalDiffTime
oneMonth = oneDay * 30
oneYear :: NominalDiffTime
oneYear = oneDay * 365

dynStatusWidget :: MonadAppIO r t m => Dynamic t JobStatus -> m (Event t ())
dynStatusWidget status = do
    let cfg = def & buttonConfig_color .~ Dyn (Just . statusColor <$> status)
                  & buttonConfig_basic .~ Static True
                  & buttonConfig_elConfig . classes .~ Static (Classes ["jobstatus"])
    _ <- SemUI.button cfg $ do
      icon (Dyn $ statusIcon <$> status) def
      dynText $ statusMessage <$> status
    return never

statusColor :: JobStatus -> Color
statusColor = \case
  JobPending -> Black
  JobInProgress -> Blue
  JobCanceled -> Grey
  JobTimedOut -> Grey
  JobVanished -> Grey
  JobFailed -> Red
  JobSucceeded -> Green

statusIcon :: JobStatus -> Text
statusIcon = \case
  JobPending -> "clock outline"
  JobInProgress -> "hourglass half"
  JobCanceled -> "x"
  JobTimedOut -> "x"
  JobVanished -> "question"
  JobFailed -> "ban"
  JobSucceeded -> "check"

statusMessage :: JobStatus -> Text
statusMessage = \case
  JobPending -> "pending"
  JobInProgress -> "running"
  JobCanceled -> "canceled"
  JobTimedOut -> "timed out"
  JobVanished -> "vanished"
  JobFailed -> "failed"
  JobSucceeded -> "passed"
