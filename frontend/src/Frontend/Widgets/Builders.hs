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

module Frontend.Widgets.Builders where

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
------------------------------------------------------------------------------
import           Common.Route
import           Common.Types.BinaryCache
import           Common.Types.Builder
import           Common.Types.Platform
import           Frontend.App
import           Frontend.AppState
import           Frontend.Common
import           Frontend.Widgets.Common
import           Frontend.Widgets.Form
------------------------------------------------------------------------------

buildersWidget
  :: MonadApp (R CrudRoute) t m
  => RoutedT t (R CrudRoute) m ()
buildersWidget = mdo
  pb <- getPostBuild
  trigger trigger_listBuilders pb
  as <- ask
  subRoute_ $ \case
    Crud_List -> buildersList (_as_builders as)
    Crud_Create -> addBuilder
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

buildersList
  :: (MonadApp r t m)
  => Dynamic t (BeamMap Identity BuilderT)
  -> m ()
buildersList as = do
  (e,_) <- elAttr' "button" ("class" =: "ui button") $ text "Add Builder"
  setRoute $ (FR_Builders :/ Crud_Create :/ ()) <$ domEvent Click e

  _ <- genericTableG def as
    [ ("ID", textDynColumn (tshow . _builder_id))
    , ("User", textDynColumn _builder_user)
    , ("Host", textDynColumn _builder_host)
    , ("Port", textDynColumn (tshow . _builder_port))
    , ("Platform", textDynColumn $ tshow . _builder_platform)
    , ("Max Builds", textDynColumn (tshow . _builder_maxBuilds))
    , ("Speed Factor", textDynColumn (tshow . _builder_speedFactor))
    , ("", \k _ -> deleteColumn trigger_deleteBuilders k)
    ]
  return ()

addBuilder :: MonadApp r t m => m ()
addBuilder = do
  semuiForm $ do
    dr <- newBuilderForm unfilledBuilder never
    divClass "field" $ do
      let as = addClassWhen "disabled" (not . isValidBuilder <$> dr)
                 (manyClasses ["ui", "button"])
      (e1,_) <- elDynKlass' "button" as $ text "Add Builder"
      (e2,_) <- elAttr' "button" ("class" =: "ui button") $ text "Cancel"
      trigger trigger_createBuilders $ tag (current dr) (domEvent Click e1)
      setRoute $ (FR_Builders :/ Crud_List :/ ()) <$ leftmost
        [domEvent Click e1, domEvent Click e2]
      return ()

unfilledBuilder :: BuilderT Maybe
unfilledBuilder = Builder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newBuilderForm
  :: MonadApp r t m
  => BuilderT Maybe
  -> Event t (BuilderT Maybe)
  -> m (Dynamic t (BuilderT Maybe))
newBuilderForm iv sv = do
    du <- divClass "field" $ do
      el "label" $ do
        text "User "
      textField
        (fromMaybe "" $ _builder_user iv)
        (fromMaybe "" . _builder_user <$> sv)
    dh <- divClass "field" $ do
      el "label" $ do
        text "Host "
      textField
        (fromMaybe "" $ _builder_host iv)
        (fromMaybe "" . _builder_host <$> sv)
    dport <- divClass "field" $ do
      el "label" $ do
        text "Port "
      readableField (maybe (Just 22) Just $ _builder_port iv) (_builder_port <$> sv)
    -- TODO Placeholder for platform
    dplat <- labelledAs "Platform" $
      platformDropdown [X86_64_Linux, X86_64_Darwin, I686_Linux] X86_64_Linux never

    dmmb <- labelledAs "Max Builds" $ readableField
      (maybe (Just 1) Just $ _builder_maxBuilds iv)
      (_builder_maxBuilds <$> sv)
    dmsf <- labelledAs "Speed Factor" $ readableField
      (maybe (Just 1) Just $ _builder_speedFactor iv)
      (_builder_speedFactor <$> sv)
    return $ do
      u <- du
      h <- dh
      port <- dport
      plat <- dplat
      mmb <- dmmb
      msf <- dmsf
      pure $ Builder Nothing (Just u) (Just h) port (Just plat) mmb msf Nothing Nothing

cachePrimaryKey :: Maybe BinaryCache -> PrimaryKey BinaryCacheT (Nullable Maybe)
cachePrimaryKey Nothing = BinaryCacheId Nothing
cachePrimaryKey (Just (BinaryCache i _)) = BinaryCacheId (Just $ Just i)

platformDropdown
  :: forall r t m. MonadApp r t m
  => [Platform]
  -> Platform
  -> Event t Platform
  -> m (Dynamic t Platform)
platformDropdown platforms iv sv = do
  let vals = M.fromList . map mkPair $ platforms
      mkPair a = (a, tshow a)
  d <- dropdown iv (constDyn vals) $ def
         & setValue .~ sv
         & attributes .~ constDyn ("class" =: "ui dropdown selection")

  return $ value d

isValidBuilder :: BuilderT Maybe -> Bool
isValidBuilder (Builder _ (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _) (Just _)) = True
isValidBuilder _ = False
