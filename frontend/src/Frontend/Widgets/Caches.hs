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

module Frontend.Widgets.Caches where

------------------------------------------------------------------------------
import           Control.Monad.Reader
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core
import           Reflex.Dom.SemanticUI
------------------------------------------------------------------------------
import           Common.Route
import           Common.Types.BinaryCache
import           Common.Types.S3Cache
import           Frontend.App
import           Frontend.AppState
import           Frontend.Widgets.Common
import           Frontend.Widgets.Form
------------------------------------------------------------------------------

cachesWidget
  :: (MonadApp r t m, Prerender js t m)
  => RoutedT t (R CrudRoute) m ()
cachesWidget = mdo
  pb <- getPostBuild
  trigger trigger_listCaches pb
  as <- ask
  subRoute_ $ \case
    Crud_List -> cacheList (_as_caches as)
    Crud_Create -> addCache
  return ()

addCache
  :: (MonadApp r t m, Prerender js t m)
  => m ()
addCache = do
  semuiForm $ do
    dc <- newCacheForm (BinaryCache Nothing Nothing Nothing Nothing) never
    divClass "field" $ do
      (e1,_) <- elAttr' "button" ("class" =: "ui button") $ text "Connect Cache"
      (e2,_) <- elAttr' "button" ("class" =: "ui button") $ text "Cancel"
      trigger trigger_addCache $ tag (current dc) (domEvent Click e1)
      setRoute $ (FR_Caches :/ Crud_List :/ ()) <$ leftmost
        [domEvent Click e1, domEvent Click e2]
      return ()

cacheList
  :: MonadApp r t m
  => Dynamic t (BeamMap Identity BinaryCacheT)
  -> m ()
cacheList as = do
    let mkField f _ v = el "td" $ dynText (f <$> v) >> return never
        widget accountMap =
          if M.null accountMap
            then accountPlaceholder
            else do
              (e,_) <- elAttr' "button" ("class" =: "ui button") $ text "Add Account"
              setRoute $ (FR_Caches :/ Crud_Create :/ ()) <$ domEvent Click e
              del <- genericTableG def (constDyn accountMap)
                [ ("ID", mkField $ tshow . _binaryCache_id)
                , ("Bucket", mkField $ _s3Cache_bucket . _binaryCache_s3Cache)
                , ("Region", mkField $ regionText . _s3Cache_region . _binaryCache_s3Cache)
                , ("", (\k _ -> deleteColumn trigger_delCaches k))
                ]
              triggerBatch trigger_delCaches $ M.keys <$> del
    _ <- dyn (widget <$> as)
    return ()

accountPlaceholder :: MonadApp r t m => m ()
accountPlaceholder = mdo
  divClass "ui placeholder segment" $ do
    divClass "ui icon header" $ do
      elClass "i" "dont icon" blank
      text "You haven't set up any S3 caches yet"
    (e,_) <- elAttr' "div" ("class" =: "ui primary button") $ text "Set Up Cache"
    setRoute $ (FR_Caches :/ Crud_Create :/ ()) <$ domEvent Click e

newCacheForm
  :: (MonadApp r t m, Prerender js t m)
  => BinaryCacheT Maybe
  -> Event t (BinaryCacheT Maybe)
  -> m (Dynamic t (BinaryCacheT Maybe))
newCacheForm iv sv = do
  dc <- s3CacheWidget (_binaryCache_s3Cache iv) (_binaryCache_s3Cache <$> sv)
  return $ do
    c <- dc
    pure $ BinaryCache Nothing c Nothing Nothing

s3CacheWidget
  :: (MonadApp r t m, Prerender js t m)
  => Maybe S3Cache
  -> Event t (Maybe S3Cache)
  -> m (Dynamic t (Maybe S3Cache))
s3CacheWidget iv sv = divClass "ui segment" $ do
    db :: Dynamic t Text <- divClass "field" $ do
      el "label" $ text "Bucket"
      v <- inputElement $ def
        & inputElementConfig_initialValue .~ maybe "" _s3Cache_bucket iv
        & inputElementConfig_setValue .~ (maybe "" _s3Cache_bucket <$> sv)
      return $ value v
    dr <- divClass "field" $ do
      el "label" $ text "Region"
      filledDropdown (maybe NorthVirginia _s3Cache_region iv)
                     (maybe NorthVirginia _s3Cache_region <$> sv)
    dak <- divClass "field" $ do
      el "label" $ text "Access Key"
      v <- inputElement $ def
        & inputElementConfig_initialValue .~ maybe "" _s3Cache_accessKey iv
        & inputElementConfig_setValue .~ (maybe "" _s3Cache_accessKey <$> sv)
      return $ value v
    dsk <- divClass "field" $ do
      el "label" $ do
        text "Secret Key "
        let tip = "Secret key not shown for security.  Leaving it empty will not clear it."
        elAttr "span" ("data-tooltip" =: tip <> "data-position" =: "top left") $
          elAttr "i" ("class" =: "info circle icon") blank

      v <- inputElement $ def
        & inputElementConfig_initialValue .~ maybe "" _s3Cache_secretKey iv
        & inputElementConfig_setValue .~ (maybe "" _s3Cache_secretKey <$> sv)
      return $ value v
    return $ do
      b <- db
      r <- dr
      ak <- dak
      sk <- dsk
      if any T.null [b, ak]
        then pure Nothing
        else pure $ Just $ S3Cache b r ak sk
