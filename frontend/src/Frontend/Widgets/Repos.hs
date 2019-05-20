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

module Frontend.Widgets.Repos where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Data.Bool
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import           Database.Beam
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
import qualified Reflex.Dom.SemanticUI as SemUI
------------------------------------------------------------------------------
import           Common.Types.ConnectedAccount
import           Common.Types.Repo
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Widgets.Common
import           Frontend.Widgets.Form
------------------------------------------------------------------------------

reposWidget :: (MonadAppIO r t m, Prerender js t m) => m ()
reposWidget = mdo
  as <- ask
  let listToState = bool ListTable EmptyPlaceholder . null
  listState <- holdDyn EmptyPlaceholder $ leftmost
    [ listToState <$> leftmost
        [ updated (_as_accounts as)
        , tag (current $ _as_accounts as) showList]
    , AddForm <$ showForm
    ]
  let repoMap = _as_repos as
  let widget s = case s of
        EmptyPlaceholder -> genericPlaceholder "Not watching any repos" >> return def
        AddForm -> addRepo
        ListTable -> reposList repoMap
  ee <- dyn (widget <$> listState)
  showForm <- switch <$> hold never (tableAction_showAddForm <$> ee)
  showList <- switch <$> hold never (tableAction_showList <$> ee)
  return ()

textDynColumn
  :: MonadApp r t m
  => (a f -> Text)
  -> PrimaryKey a f
  -> Dynamic t (a f)
  -> m ()
textDynColumn f _ v = el "td" $ do
  dynText (f <$> v)
  return ()

reposList
  :: MonadAppIO r t m
  => Dynamic t (BeamMap Identity RepoT)
  -> m (TableAction t (RepoT Maybe))
reposList as = do
  add <- SemUI.button def $ text "Add Repository"
  del <- genericRemovableTable as
    [ ("ID", textDynColumn (tshow . _repo_id))
    --, ("Owner", textDynColumn _repo_owner)
    , ("Full Name", textDynColumn $ _repo_fullName)
    , ("Name", textDynColumn $ _repo_name)
    --, ("Clone Method", mkField $ _rbi_gitRef . _buildJob_repoBuildInfo)
    , ("Timeout", textDynColumn (tshow . _repo_timeout))
    --, ("Build Command", \_ v -> el "td" (commitWidget v) >> return never)
    --, ("", (\k v -> elClass "td" "right aligned collapsing" $ cancelButton k v))
    ]
  triggerBatch trigger_delRepos $ M.keys <$> del
  return $ TableAction add never

addRepo
  :: (MonadAppIO r t m, Prerender js t m)
  => m (TableAction t (RepoT Maybe))
addRepo = do
  semuiForm $ do
    dr <- newRepoForm unfilledRepo never
    divClass "field" $ do
      let as = addClassWhen "disabled" (not . isValidRepo <$> dr)
                 (manyClasses ["ui", "button"])
      (e1,_) <- elDynKlass' "button" as $ text "Add Repo"
      (e2,_) <- elAttr' "button" ("class" =: "ui button") $ text "Cancel"
      trigger trigger_addRepo $ tag (current dr) (domEvent Click e1)
      return $ TableAction never (leftmost [domEvent Click e1, domEvent Click e2])

unfilledRepo :: RepoT Maybe
unfilledRepo = Repo Nothing Nothing (ConnectedAccountId Nothing) Nothing Nothing Nothing Nothing Nothing

newRepoForm
  :: (MonadAppIO r t m, Prerender js t m)
  => RepoT Maybe
  -> Event t (RepoT Maybe)
  -> m (Dynamic t (RepoT Maybe))
newRepoForm iv sv = do
    accounts <- asks _as_accounts
    dmca <- labelledAs "Owning Account" $ do
      res <- prerender (pure $ pure (Nothing :: Maybe ConnectedAccountId)) $ fmap value $ SemUI.dropdown def Nothing never $
        -- (maybe (head accounts) _connectedAccount_name $ _repo_owner =<< iv)
        -- (fmap join $ _repo_owner <$$> sv) $
        TaggedDynamic $ text . accountText <$$> accounts
      return $ join res
    dn <- labelledAs "Repo name" $ textField
      (fromMaybe "" $ _repo_name iv)
      (fromMaybe "" . _repo_name <$> sv)
    dbc <- labelledAs "Build Command" $ textareaField
      (fromMaybe "" $ _repo_buildCmd iv)
      (fromMaybe "" . _repo_buildCmd <$> sv)
    dcm <- labelledAs "Clone Method" $ filledDropdown
      (fromMaybe HttpClone $ _repo_cloneMethod iv)
      (fmapMaybe id $ _repo_cloneMethod <$> sv)
    dt <- labelledAs "Timeout (in seconds)" $ readableField Nothing
      (_repo_timeout iv)
      (_repo_timeout <$> sv)
    return $ do
        n <- dn
        bc <- dbc
        cm <- dcm
        t <- dt
        accountMap <- accounts
        mca <- dmca
        pure $ case flip M.lookup accountMap =<< mca of
          Nothing -> unfilledRepo
          Just a -> do
            let aid = _connectedAccount_id a
                maid = ConnectedAccountId $ Just aid
                ownerName = _connectedAccount_name a
                fn = mkFullName (_connectedAccount_provider a) ownerName n
             in Repo Nothing (Just fn) maid (Just n) (Just cm) (Just bc) t Nothing
  where
    --showAccount a =
    --  (fromJust $ _connectedAccount_id a, accountText a)
    accountText a =
       providerUrl (_connectedAccount_provider a) <> "/" <>
              (_connectedAccount_name a)

mkFullName :: AccountProvider -> Text -> Text -> Text
mkFullName GitHub owner name = owner <> "/" <> name
mkFullName GitLab owner name = owner <> "/" <> name

isValidRepo :: RepoT Maybe -> Bool
isValidRepo (Repo _ (Just _) (ConnectedAccountId (Just _)) (Just _) (Just _) (Just _) (Just _) _) = True
isValidRepo _ = False
