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

reposWidget :: MonadApp t m => m ()
reposWidget = mdo
  as <- ask
  let listToState = bool ListTable EmptyPlaceholder . null
  listState <- holdDyn EmptyPlaceholder $ leftmost
    [ listToState <$> leftmost
        [ updated (_as_accounts as)
        , tag (current $ _as_accounts as) cancel]
    , AddForm <$ showForm
    ]
  let repoMap = _as_repos as
  let widget s = case s of
        EmptyPlaceholder -> genericPlaceholder "Not watching any repos" >> return def
        AddForm -> addRepo
        ListTable -> reposList repoMap
  ee <- dyn (widget <$> listState)
  showForm <- switch <$> hold never (tableAction_showAddForm <$> ee)
  cancel <- switch <$> hold never (tableAction_cancelAdd <$> ee)
  return ()

textDynColumn
  :: MonadWidget t m
  => (a f -> Text)
  -> PrimaryKey a f
  -> Dynamic t (a f)
  -> m (Event t ())
textDynColumn f _ v = el "td" $ do
  dynText (f <$> v)
  return never

reposList
  :: MonadApp t m
  => Dynamic t (BeamMap Identity RepoT)
  -> m (TableAction t (RepoT Maybe))
reposList as = do
  elClass "h1" "ui header" $ text "Repositories"
  add <- SemUI.button def $ text "Add Repository"
  _ <- genericTable as
    [ ("ID", textDynColumn (tshow . _repo_id))
    --, ("Owner", textDynColumn _repo_owner)
    , ("Full Name", textDynColumn $ _repo_fullName)
    , ("Name", textDynColumn $ _repo_name)
    --, ("Clone Method", mkField $ _rbi_gitRef . _buildJob_repoBuildInfo)
    , ("Timeout", textDynColumn (tshow . _repo_timeout))
    --, ("Build Command", \_ v -> el "td" (commitWidget v) >> return never)
    --, ("", (\k v -> elClass "td" "right aligned collapsing" $ cancelButton k v))
    ]
  --triggerBatch trigger_delAccounts $ M.keys <$> del
  return $ TableAction add never

addRepo
  :: MonadApp t m
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
      return $ TableAction never (domEvent Click e2)

unfilledRepo :: RepoT Maybe
unfilledRepo = Repo Nothing Nothing (ConnectedAccountId Nothing) Nothing Nothing Nothing Nothing

newRepoForm
  :: MonadApp t m
  => RepoT Maybe
  -> Event t (RepoT Maybe)
  -> m (Dynamic t (RepoT Maybe))
newRepoForm iv sv = do
    accounts <- asks _as_accounts
    dmca <- labelledAs "Owning Account" $
      SemUI.dropdown def Nothing never $
        -- (maybe (head accounts) _connectedAccount_name $ _repo_owner =<< iv)
        -- (fmap join $ _repo_owner <$$> sv) $
        TaggedDynamic $ text . accountText <$$> accounts
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
        mca <- value dmca
        pure $ case flip M.lookup accountMap =<< mca of
          Nothing -> unfilledRepo
          Just a -> do
            let aid = _connectedAccount_id a
                maid = ConnectedAccountId $ Just aid
                ownerName = _connectedAccount_name a
                fn = mkFullName (_connectedAccount_provider a) ownerName n
             in Repo Nothing (Just fn) maid (Just n) (Just cm) (Just bc) t
  where
    showAccount a =
      (fromJust $ _connectedAccount_id a, accountText a)
    accountText a =
       providerUrl (_connectedAccount_provider a) <> "/" <>
              (_connectedAccount_name a)

mkFullName :: AccountProvider -> Text -> Text -> Text
mkFullName GitHub owner name = owner <> "/" <> name
mkFullName GitLab owner name = owner <> "/" <> name

isValidRepo :: RepoT Maybe -> Bool
isValidRepo (Repo _ (Just _) (ConnectedAccountId (Just _)) (Just _) (Just _) (Just _) (Just _)) = True
isValidRepo _ = False
