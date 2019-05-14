{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
import           Reflex.Time
------------------------------------------------------------------------------
import           Common.Types.BuildJob
import           Common.Types.RepoBuildInfo
import           Frontend.App
import           Frontend.AppState
import           Frontend.Widgets.Common
------------------------------------------------------------------------------

jobsWidget :: MonadApp t m => m ()
jobsWidget = mdo
  as <- ask

  let mkPair ca = (_buildJob_id ca, ca)
  let jobMap = M.fromList . map mkPair <$> _as_jobs as
  let widget m = if M.null m then jobsPlaceholder else jobsList jobMap
  ee <- dyn (widget <$> jobMap)
  return ()

jobDuration :: UTCTime -> BuildJob -> Maybe NominalDiffTime
jobDuration now bj = do
  start <- _buildJob_startedAt bj
  pure $ diffUTCTime (fromMaybe now $ _buildJob_endedAt bj) start

jobsList
  :: MonadApp t m
  => Dynamic t (Map Int BuildJob)
  -> m ()
jobsList as = do
  divClass "ui segment" $ do
    elClass "h1" "ui header" $ text "Jobs"
    let mkField f _ v = el "td" $ dynText (f <$> v) >> return ()
    now <- liftIO getCurrentTime
    del <- genericRemovableTable as
      [ ("Status", mkField $ tshow . _buildJob_status)
      , ("ID", mkField $ tshow . _buildJob_id)
      , ("Repo", mkField $ _rbi_repoFullName . _buildJob_repoBuildInfo)
      , ("Git Ref", mkField $ _rbi_gitRef . _buildJob_repoBuildInfo)
      , ("Commit Hash", mkField $ _rbi_commitHash . _buildJob_repoBuildInfo)
      , ("Duration", mkField $ maybe "Not started" diffTimeToRelativeEnglish . jobDuration now)
      --, ("", (\_ _ -> elClass "td" "right aligned collapsing" deleteButton))
      ]
    triggerBatch trigger_cancelJobs $ M.keys <$> del

pastTimeWiget
  :: MonadWidget t m
  => UTCTime
  -> m ()
pastTimeWiget t = do
  ti <- clockLossy 5 t
  dynText $ diffTimeToRelativeEnglish . _tickInfo_alreadyElapsed <$> ti

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

jobsPlaceholder :: MonadApp t m => m ()
jobsPlaceholder = do
  divClass "ui placeholder segment" $ do
    divClass "ui icon header" $ do
      elClass "i" "dont icon" blank
      text "Job history empty"
