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

module Frontend.Widgets.Repos where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import           Database.Beam
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Core
import qualified Reflex.Dom.SemanticUI as SemUI
------------------------------------------------------------------------------
import           Common.Route
import           Common.Types.ConnectedAccount
import           Common.Types.Repo
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Widgets.Common
import           Frontend.Widgets.Form
------------------------------------------------------------------------------

reposWidget
  :: (MonadAppIO (R CrudRoute) t m, Prerender js t m)
  => RoutedT t (R CrudRoute) m ()
reposWidget = mdo
  pb <- getPostBuild
  trigger trigger_getRepos pb
  as <- ask
  subRoute_ $ \case
    Crud_List -> reposList (_as_repos as)
    Crud_Create -> addRepo
  return ()

textDynColumn
  :: MonadApp r t m
  => (a f -> Text)
  -> PrimaryKey a f
  -> Dynamic t (a f)
  -> m (Event t ())
textDynColumn f _ v = el "td" $ do
  dynText (f <$> v)
  return never

reposList
  :: MonadAppIO r t m
  => Dynamic t (BeamMap Identity RepoT)
  -> m ()
reposList as = do
  add <- SemUI.button def $ text "Add Repository"
  setRoute $ (FR_Repos :/ Crud_Create :/ ()) <$ add

  _ <- genericTableG def as
    [ ("ID", textDynColumn (tshow . _repo_id))
    , ("Full Name", textDynColumn $ repoFullName)
    , ("Clone Method", textDynColumn $ tshow . _repo_cloneMethod)
    , ("Nix File", textDynColumn $ _repo_buildNixFile)
    , ("Timeout", textDynColumn (tshow . _repo_timeout))
    , ("", \k _ -> deleteColumn trigger_delRepos k)
    ]
  return ()

addRepo :: (MonadAppIO r t m, Prerender js t m) => m ()
addRepo = do
  semuiForm $ do
    dr <- newRepoForm unfilledRepo never
    divClass "field" $ do
      let as = addClassWhen "disabled" (not . isValidRepo <$> dr)
                 (manyClasses ["ui", "button"])
      (e1,_) <- elDynKlass' "button" as $ text "Add Repo"
      (e2,_) <- elAttr' "button" ("class" =: "ui button") $ text "Cancel"
      trigger trigger_addRepo $ tag (current dr) (domEvent Click e1)
      setRoute $ (FR_Repos :/ Crud_List :/ ()) <$ leftmost
        [domEvent Click e1, domEvent Click e2]
      return ()

unfilledRepo :: RepoT Maybe
unfilledRepo = Repo Nothing (ConnectedAccountId Nothing) Nothing Nothing Nothing Nothing Nothing Nothing

newRepoForm
  :: (MonadAppIO r t m, Prerender js t m)
  => RepoT Maybe
  -> Event t (RepoT Maybe)
  -> m (Dynamic t (RepoT Maybe))
newRepoForm iv sv = do
    accounts <- asks _as_accounts
    dmca <- labelledAs "Access Account" $
      accountDropdown accounts Nothing never
    drns <- divClass "field" $ do
      el "label" $ do
        text "Repo Namespace "
        let tip = "Everything after the domain but before the repository name.  In GitHub, this is just the user/org name.  In GitLab, this can be multiple nested subgroups."
        elAttr "span" ("data-tooltip" =: tip <> "data-position" =: "top left") $
          elAttr "i" ("class" =: "info circle icon") blank
      textField
        (fromMaybe "" $ _repo_namespace iv)
        (fromMaybe "" . _repo_namespace <$> sv)
    drn <- divClass "field" $ do
      el "label" $ do
        text "Repo name "
        let tip = "The repository name (the last component of the project's root URL)"
        elAttr "span" ("data-tooltip" =: tip <> "data-position" =: "top left") $
          elAttr "i" ("class" =: "info circle icon") blank
      textField
        (fromMaybe "" $ _repo_name iv)
        (fromMaybe "" . _repo_name <$> sv)
    let tip = "Path to .nix file in your repo that describes the build (default.nix, release.nix, etc)"
    let bnfLabel = do
          text "Build Nix File "
          elAttr "span" ("data-tooltip" =: tip <> "data-position" =: "top left") $
            elAttr "i" ("class" =: "info circle icon") blank
    dnf <- divClass "field" $ do
      el "label" $ bnfLabel
      ie <- inputElement $ def
        & inputElementConfig_initialValue .~ (fromMaybe "default.nix" $ _repo_buildNixFile iv)
        & inputElementConfig_setValue .~ (fromMaybe "default.nix" . _repo_buildNixFile <$> sv)
      return $ value ie
    dcm <- labelledAs "Clone Method" $ filledDropdown
      (fromMaybe HttpClone $ _repo_cloneMethod iv)
      (fmapMaybe id $ _repo_cloneMethod <$> sv)
    dt <- labelledAs "Timeout (in seconds)" $ readableField Nothing
      (maybe (Just 3600) Just $ _repo_timeout iv)
      (_repo_timeout <$> sv)
    return $ do
        rns <- drns
        rn <- drn
        nf <- dnf
        cm <- dcm
        t <- dt
        mca <- dmca
        pure $ case mca of
          Nothing -> unfilledRepo
          Just a -> do
            let aid = _connectedAccount_id a
                maid = ConnectedAccountId $ Just aid
                ownerName = _connectedAccount_name a
             in Repo Nothing maid (Just rn) (Just rns) (Just cm) (Just nf) t Nothing

accountDropdown
  :: forall r t m. MonadApp r t m
  => Dynamic t (BeamMap Identity ConnectedAccountT)
  -> Maybe ConnectedAccount
  -> Event t (Maybe ConnectedAccount)
  -> m (Dynamic t (Maybe ConnectedAccount))
accountDropdown accounts iv sv = do
  let vals = M.fromList . map mkPair . (Nothing:) . map Just . M.elems <$> accounts
      mkPair Nothing = (Nothing,"")
      mkPair (Just a) = (Just a, providerUrl (_connectedAccount_provider a) <> "/" <> _connectedAccount_name a)
  d <- dropdown iv vals $ def
         & setValue .~ sv
         & attributes .~ constDyn ("class" =: "ui dropdown selection")

  return $ value d

--accountDropdown
--  :: MonadApp r t m
--  => Dynamic t (BeamMap Identity ConnectedAccountT)
--  -> PrimaryKey ConnectedAccountT Maybe
--  -> Event t (PrimaryKey ConnectedAccountT Maybe)
--  -> m (Dynamic t (Maybe ConnectedAccount))
--accountDropdown accounts iv sv = do
--  ed <- networkView $ ffor accounts $ \as -> do
--    let scfg = SelectElementConfig
--                 (maybe "" tshow $ caKeyMaybeToId iv)
--                 (Just $ tshow . caKeyToInt <$> fmapMaybe caKeyMaybeToId sv)
--                 def
--    (se,_) <- selectElement scfg $ do
--      forM_ (M.toList as) $ \(k,a) -> do
--        let cfg = def
--                  & elementConfig_initialAttributes .~ (AttributeName Nothing "value" =: tshow k)
--        element "option" cfg $ text $ providerUrl (_connectedAccount_provider a) <> "/" <> _connectedAccount_name a
--    return $ join . fmap ((\aid -> M.lookup aid as)  . intToCaKey) . fromText <$> _selectElement_value se
--  dyndyn <- holdDyn (constDyn Nothing) ed
--  return $ join dyndyn


mkFullName :: AccountProvider -> Text -> Text -> Text
mkFullName GitHub owner name = owner <> "/" <> name
mkFullName GitLab owner name = owner <> "/" <> name

isValidRepo :: RepoT Maybe -> Bool
isValidRepo (Repo _ (ConnectedAccountId (Just _)) (Just _) (Just _) (Just _) (Just _) (Just _) _) = True
isValidRepo _ = False
