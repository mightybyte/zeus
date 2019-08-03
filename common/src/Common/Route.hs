{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

------------------------------------------------------------------------------
import           Prelude hiding (id, (.))
import           Control.Category
import           Control.Lens
import           Data.Some (Some)
import qualified Data.Some as Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Functor.Sum
import qualified Obelisk.ExecutableConfig as ObConfig
import           Obelisk.Route
import           Obelisk.Route.TH
import           Reflex.Dom
------------------------------------------------------------------------------

-- This needs to match the BackendRoute_GithubHook line below. Will figure out
-- how to make it DRY later.
githubHookPath :: Text
githubHookPath = "hook/github"

gitlabHookPath :: Text
gitlabHookPath = "hook/gitlab"

data HookRoute :: * -> * where
  Hook_GitHub :: HookRoute ()
  Hook_GitLab :: HookRoute ()

deriveRouteComponent ''HookRoute

data CrudRoute :: * -> * where
  Crud_List :: CrudRoute ()
  Crud_Create :: CrudRoute ()
  --Crud_Update :: CrudRoute Int
  --Crud_Delete :: CrudRoute Int

deriveRouteComponent ''CrudRoute

data JobRoute :: * -> * where
  Job_List :: JobRoute ()
  Job_Output :: JobRoute Int

deriveRouteComponent ''JobRoute

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Cache :: BackendRoute [Text]
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Hook :: BackendRoute (R HookRoute)
  BackendRoute_Ping :: BackendRoute ()
  BackendRoute_RawBuildOut :: BackendRoute Text
  BackendRoute_Websocket :: BackendRoute ()

data FrontendRoute :: * -> * where
  FR_Home :: FrontendRoute ()
  FR_Jobs :: FrontendRoute (R JobRoute)
  FR_Repos :: FrontendRoute (R CrudRoute)
  FR_Accounts :: FrontendRoute (R CrudRoute)
  FR_Settings :: FrontendRoute ()

type FullRoute = Sum BackendRoute (ObeliskRoute FrontendRoute)

hookRouteEncoder
  :: Encoder (Either Text) (Either Text) (R HookRoute) PageName
hookRouteEncoder = pathComponentEncoder $ \case
  Hook_GitHub -> PathSegment "github" $ unitEncoder mempty
  Hook_GitLab -> PathSegment "gitlab" $ unitEncoder mempty

crudRouteEncoder
  :: Encoder (Either Text) (Either Text) (R CrudRoute) PageName
crudRouteEncoder = pathComponentEncoder $ \case
  Crud_List -> PathEnd $ unitEncoder mempty
  Crud_Create -> PathSegment "new" $ unitEncoder mempty

jobRouteEncoder
  :: Encoder (Either Text) (Either Text) (R JobRoute) PageName
jobRouteEncoder = pathComponentEncoder $ \case
  Job_List -> PathEnd $ unitEncoder mempty
  Job_Output -> PathSegment "output" (singlePathSegmentEncoder . unsafeTshowEncoder)

backendRouteEncoder
  :: Encoder (Either Text) Identity (R FullRoute) PageName
backendRouteEncoder =
  handleEncoder (\_ -> InR (ObeliskRoute_App FR_Home) :/ ()) $ pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Cache -> PathSegment "cache" pathOnlyEncoder
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Hook -> PathSegment "hook" hookRouteEncoder
      BackendRoute_Ping -> PathSegment "ping" $ unitEncoder mempty
      BackendRoute_RawBuildOut -> PathSegment "raw" singlePathSegmentEncoder
      BackendRoute_Websocket -> PathSegment "ws" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FR_Home -> PathEnd $ unitEncoder mempty
      FR_Jobs -> PathSegment "jobs" jobRouteEncoder
      FR_Repos -> PathSegment "repos" crudRouteEncoder
      FR_Accounts -> PathSegment "accounts" crudRouteEncoder
      FR_Settings -> PathSegment "settings" $ unitEncoder mempty

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

getAppRoute :: IO Text
getAppRoute = do
    mroute <- ObConfig.get "config/common/route"
    case mroute of
      Nothing -> fail "Error getAppRoute: config/common/route not defined"
      Just r -> return $ T.dropWhileEnd (== '/') $ T.strip r

-- | Provide a human-readable name for a given section
tabTitle :: DomBuilder t m => Some FrontendRoute -> m ()
tabTitle sfr@(Some.This sec) = case sec of
  FR_Home -> text $ frToText sfr
  FR_Jobs -> text $ frToText sfr
  FR_Repos -> text $ frToText sfr
  FR_Accounts -> text $ frToText sfr
  FR_Settings -> text $ frToText sfr

-- | Provide a human-readable name for a given section
frToText :: Some FrontendRoute -> Text
frToText (Some.This sec) = case sec of
  FR_Home -> "Home"
  FR_Jobs -> "Jobs"
  FR_Repos -> "Repos"
  FR_Accounts -> "Accounts"
  FR_Settings -> "Settings"


tabHomepage :: Some FrontendRoute -> R FrontendRoute
tabHomepage (Some.This sec) = sec :/ case sec of
  FR_Home -> ()
  FR_Jobs -> Job_List :/ ()
  FR_Repos -> Crud_List :/ ()
  FR_Accounts -> Crud_List :/ ()
  FR_Settings -> ()
