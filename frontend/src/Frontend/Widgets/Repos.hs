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
import           Data.Bool
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Common.Route
import           Common.Types.BinaryCache
import           Common.Types.Builder
import           Common.Types.ConnectedAccount
import           Common.Types.Platform
import           Common.Types.Repo
import           Common.Types.S3Cache
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Widgets.Common
import           Frontend.Widgets.Form
------------------------------------------------------------------------------

reposWidget
  :: MonadApp (R CrudRoute) t m
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
  :: (MonadApp r t m)
  => Dynamic t (BeamMap Identity RepoT)
  -> m ()
reposList as = do
  (e,_) <- elAttr' "button" ("class" =: "ui button") $ text "Add Repository"
  setRoute $ (FR_Repos :/ Crud_Create :/ ()) <$ domEvent Click e

  _ <- genericTableG def as
    [ ("ID", textDynColumn (tshow . _repo_id))
    , ("Full Name", textDynColumn $ repoFullName)
    , ("Nix File", textDynColumn $ _repo_buildNixFile)
    , ("Timeout", textDynColumn (tshow . _repo_timeout))
    , ("", \k _ -> deleteColumn trigger_delRepos k)
    ]
  return ()

addRepo :: MonadApp r t m => m ()
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
unfilledRepo = Repo Nothing (ConnectedAccountId Nothing) Nothing Nothing Nothing (Just mempty) (Just mempty) Nothing (BinaryCacheId Nothing) Nothing

newRepoForm
  :: MonadApp r t m
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
        let tip = "Everything after the domain but before the repository name.\nIn GitHub, this is just the user/org name.\nIn GitLab, this can be multiple nested subgroups."
        elAttr "span" ("data-tooltip" =: tip <>
                       "data-position" =: "top left" <>
                       "data-variation" =: "wide") $
          elAttr "i" ("class" =: "info circle icon") blank
        --divClass "ui button" $ elAttr "i" ("class" =: "info circle icon") blank
        --divClass "ui popup top left transition hidden" $ do
        --  text "Everything after the domain but before the repository name."
        --  el "br" blank
        --  text "In GitHub, this is just the user/org name."
        --  el "br" blank
        --  text "In GitLab, this can be multiple nested subgroups."
      textField
        (fromMaybe "" $ _repo_namespace iv)
        (fromMaybe "" . _repo_namespace <$> sv)
    drn <- divClass "field" $ do
      el "label" $ do
        text "Repo Name "
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

    caches <- asks _as_caches
    dcache <- labelledAs "S3 Cache" $
      cacheDropdown caches Nothing never

    dnf <- divClass "field" $ do
      el "label" $ bnfLabel
      ie <- inputElement $ def
        & inputElementConfig_initialValue .~ (fromMaybe "default.nix" $ _repo_buildNixFile iv)
        & inputElementConfig_setValue .~ (fromMaybe "default.nix" . _repo_buildNixFile <$> sv)
      return $ value ie

    let attrsTip = "Optional space-separated list of attributes to build"
    let attrsLabel = do
          text "Attributes to build "
          elAttr "span" ("data-tooltip" =: attrsTip <> "data-position" =: "top left") $
            elAttr "i" ("class" =: "info circle icon") blank
    das <- divClass "field" $ do
      el "label" $ attrsLabel
      ie <- inputElement $ def
        & inputElementConfig_initialValue .~ (maybe "" (T.unwords . unAttrList) $ _repo_attributesToBuild iv)
        & inputElementConfig_setValue .~ (maybe "" (T.unwords . unAttrList) . _repo_attributesToBuild <$> sv)
      return $ fmap (AttrList . T.words) $ value ie

    let platTip = "To get more platforms, add more builders"
    let platLabel = do
          text "Platforms to build with "
          elAttr "span" ("data-tooltip" =: platTip <> "data-position" =: "top left") $
            elAttr "i" ("class" =: "info circle icon") blank
    divClass "field" $ do
      el "label" platLabel
      -- Change this initial value
    dps <- platformWidget (S.singleton X86_64_Linux) never

    dt <- labelledAs "Timeout (in seconds)" $ readableField
      (maybe (Just 3600) Just $ _repo_timeout iv)
      (_repo_timeout <$> traceEvent "timeout sv" sv)
    return $ do
        rns <- drns
        rn <- drn
        nf <- dnf
        --cm <- dcm
        t <- dt
        mca <- dmca
        c <- dcache
        as <- das
        ps <- dps
        pure $ case mca of
          Nothing -> unfilledRepo
          Just a -> do
            let aid = _connectedAccount_id a
                maid = ConnectedAccountId $ Just aid
             in Repo Nothing maid (Just rn) (Just rns) (Just nf) (Just as)
                     (Just $ PlatformSet ps) t (cachePrimaryKey c) Nothing

cachePrimaryKey :: Maybe BinaryCache -> PrimaryKey BinaryCacheT (Nullable Maybe)
cachePrimaryKey Nothing = BinaryCacheId Nothing
cachePrimaryKey (Just (BinaryCache i _)) = BinaryCacheId (Just $ Just i)

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

cacheDropdown
  :: forall r t m. MonadApp r t m
  => Dynamic t (BeamMap Identity BinaryCacheT)
  -> Maybe BinaryCache
  -> Event t (Maybe BinaryCache)
  -> m (Dynamic t (Maybe BinaryCache))
cacheDropdown caches iv sv = do
  let vals = M.fromList . map mkPair . (Nothing:) . map Just . M.elems <$> caches
      mkPair Nothing = (Nothing,"")
      mkPair (Just a) = (Just a, "s3://" <> _s3Cache_bucket (_binaryCache_s3Cache a))
  d <- dropdown iv vals $ def
         & setValue .~ sv
         & attributes .~ constDyn ("class" =: "ui dropdown selection")

  return $ value d

platformWidget
  :: forall r t m. MonadApp r t m
  => Set Platform
  -> Event t (Set Platform)
  -> m (Dynamic t (Set Platform))
platformWidget iv sv = do
  bs <- asks _as_builders
  let mkPair a = (a,a)
  let ps = S.fromList . map (_builder_platform) . M.elems <$> bs
  res <- listWithKey (M.fromList . map mkPair . S.toList <$> ps) $ \plat _ ->
    singlePlatform plat iv sv
  return $ fmap (S.fromList . catMaybes) $ join $
    distributeListOverDyn .
    map (\(k,db) -> bool Nothing (Just k) <$> db) .
    M.toList <$> res

singlePlatform
  :: MonadApp r t m
  => Platform
  -> Set Platform
  -> Event t (Set Platform)
  -> m (Dynamic t Bool)
singlePlatform plat iv sv = divClass "inline field" $ do
  divClass "ui checkbox" $ do
    v <- checkbox (S.member plat iv) $ def
      & setValue .~ (S.member plat <$> sv)
    el "label" $ text $ tshow plat
    return $ value v

mkFullName :: AccountProvider -> Text -> Text -> Text
mkFullName GitHub owner name = owner <> "/" <> name
mkFullName GitLab owner name = owner <> "/" <> name

isValidRepo :: RepoT Maybe -> Bool
isValidRepo (Repo _ (ConnectedAccountId (Just _)) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (BinaryCacheId _) _) = True
isValidRepo _ = False
