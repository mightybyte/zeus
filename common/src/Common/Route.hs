{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

------------------------------------------------------------------------------
import           Data.Dependent.Sum (DSum (..))
import           Data.Some (Some)
import qualified Data.Some as Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Functor.Identity
import           Data.Functor.Sum
import qualified Obelisk.ExecutableConfig as ObConfig
import           Obelisk.Route
import           Obelisk.Route.TH
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

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Hook :: BackendRoute (R HookRoute)
  BackendRoute_Ping :: BackendRoute ()
  BackendRoute_Websocket :: BackendRoute ()

data FrontendRoute :: * -> * where
  FR_Home :: FrontendRoute ()
  FR_Jobs :: FrontendRoute ()
  --FR_Repos :: FrontendRoute ()
  FR_Repos :: FrontendRoute (R CrudRoute)
  FR_Accounts :: FrontendRoute (R CrudRoute)

type FullRoute = Sum BackendRoute (ObeliskRoute FrontendRoute)

crudRouteEncoder
  :: Encoder (Either Text) (Either Text) (R CrudRoute) PageName
crudRouteEncoder = pathComponentEncoder $ \case
  Crud_List -> PathEnd $ unitEncoder mempty
  Crud_Create -> PathSegment "new" $ unitEncoder mempty

hookRouteEncoder
  :: Encoder (Either Text) (Either Text) (R HookRoute) PageName
hookRouteEncoder = pathComponentEncoder $ \case
  Hook_GitHub -> PathSegment "github" $ unitEncoder mempty
  Hook_GitLab -> PathSegment "gitlab" $ unitEncoder mempty

backendRouteEncoder
  :: Encoder (Either Text) Identity (R FullRoute) PageName
backendRouteEncoder =
  handleEncoder (\_ -> InR (ObeliskRoute_App FR_Home) :/ ()) $ pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Hook -> PathSegment "hook" hookRouteEncoder
      BackendRoute_Ping -> PathSegment "ping" $ unitEncoder mempty
      BackendRoute_Websocket -> PathSegment "ws" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FR_Home -> PathEnd $ unitEncoder mempty
      FR_Jobs -> PathSegment "jobs" $ unitEncoder mempty
      FR_Repos -> PathSegment "repos" crudRouteEncoder
      FR_Accounts -> PathSegment "accounts" crudRouteEncoder

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
tabTitle :: Some FrontendRoute -> Text
tabTitle (Some.This sec) = case sec of
  FR_Home -> "Home"
  FR_Jobs -> "Jobs"
  FR_Repos -> "Repos"
  FR_Accounts -> "Accounts"

-- | Provide a human-readable name for a route
frToText :: R FrontendRoute -> Text
frToText (sec :=> _) = tabTitle $ Some.This sec

tabHomepage :: Some FrontendRoute -> R FrontendRoute
tabHomepage (Some.This sec) = sec :/ case sec of
  FR_Home -> ()
  FR_Jobs -> ()
  FR_Repos -> Crud_List :/ ()
  FR_Accounts -> Crud_List :/ ()
