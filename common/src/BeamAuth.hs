{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module BeamAuth where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.Types
import           Database.Beam.Migrate.Generics
import           Database.Beam.Migrate.SQL
import           Scrub
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Password is clear when supplied by the user and encrypted later when
-- returned from the db.
data Password
  = ClearText Text
  | Encrypted ByteString
  deriving (Eq,Ord,Show,Read,Generic)


------------------------------------------------------------------------------
-- | Default strength level to pass into makePassword.
defaultStrength :: Int
defaultStrength = 12


-------------------------------------------------------------------------------
-- | The underlying encryption function, in case you need it for
-- external processing.
encrypt :: ByteString -> IO ByteString
encrypt = flip makePassword defaultStrength


-------------------------------------------------------------------------------
-- | The underlying verify function, in case you need it for external
-- processing.
verify
    :: Text                     -- ^ Cleartext
    -> ByteString               -- ^ Encrypted reference
    -> Bool
verify clear encrypted = verifyPassword (encodeUtf8 clear) encrypted


------------------------------------------------------------------------------
-- | Turn a 'ClearText' password into an 'Encrypted' password, ready to
-- be stuffed into a database.
encryptPassword :: Password -> IO Password
encryptPassword p@(Encrypted {}) = return p
encryptPassword (ClearText p)    = Encrypted `fmap` encrypt (encodeUtf8 p)


------------------------------------------------------------------------------
checkPassword :: Password -> Password -> Bool
checkPassword (ClearText pw) (Encrypted pw') = verify pw pw'
checkPassword (ClearText pw) (ClearText pw') = pw == pw'
checkPassword (Encrypted pw) (Encrypted pw') = pw == pw'
checkPassword _ _ =
  error "checkPassword failed. Make sure you pass ClearText passwords"


instance ToJSON Password where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Password

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Password where
  sqlValueSyntax = autoSqlValueSyntax

instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be Password where
  fromBackendRow = read . T.unpack <$> fromBackendRow

instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be Password where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

------------------------------------------------------------------------------
data UserT f = User
  { _user_id :: C f Int
  , _user_email :: C f Text
  , _user_password :: C f (Maybe Password)
  , _user_activatedAt :: C f (Maybe UTCTime)
  , _user_suspendedAt :: C f (Maybe UTCTime)
  , _user_rememberToken :: C f (Maybe Text)
  , _user_loginCount :: C f Int
  , _user_failedLoginCount :: C f Int
  , _user_lockedOutUntil :: C f (Maybe UTCTime)
  , _user_currentLoginAt :: C f (Maybe UTCTime)
  , _user_lastLoginAt :: C f (Maybe UTCTime)
  , _user_currentLoginIp :: C f (Maybe ByteString)
  , _user_lastLoginIp :: C f (Maybe ByteString)
  , _user_createdAt :: C f (Maybe UTCTime)
  , _user_updatedAt :: C f (Maybe UTCTime)
  , _user_resetToken :: C f (Maybe Text)
  , _user_resetRequestedAt :: C f (Maybe UTCTime)
  } deriving (Generic)

User
  (LensFor user_id)
  (LensFor user_name)
  (LensFor user_password)
  (LensFor user_activatedAt)
  (LensFor user_suspendedAt)
  (LensFor user_rememberToken)
  (LensFor user_loginCount)
  (LensFor user_failedLoginCount)
  (LensFor user_lockedOutUntil)
  (LensFor user_currentLoginAt)
  (LensFor user_lastLoginAt)
  (LensFor user_currentLoginIp)
  (LensFor user_lastLoginIp)
  (LensFor user_createdAt)
  (LensFor user_updatedAt)
  (LensFor user_resetToken)
  (LensFor user_resetRequestedAt)
  = tableLenses

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Eq (PrimaryKey UserT Identity)
deriving instance Eq (PrimaryKey UserT Maybe)
deriving instance Eq User
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show (PrimaryKey UserT Maybe)
deriving instance Show User
deriving instance Show (UserT Maybe)
deriving instance Default (UserT Maybe)

deriving instance Ord User
deriving instance Ord (PrimaryKey UserT Identity)
deriving instance Ord (PrimaryKey UserT Maybe)

instance ToJSON (PrimaryKey UserT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey UserT Identity)

instance ToJSON (PrimaryKey UserT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (PrimaryKey UserT Maybe)

instance ToJSON (UserT Identity) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (UserT Identity)

instance ToJSON (UserT Maybe) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (UserT Maybe)

instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = UserId . _user_id

userKeyToInt :: PrimaryKey UserT Identity -> Int
userKeyToInt (UserId k) = k

intToUserKey :: Int -> PrimaryKey UserT Identity
intToUserKey k = UserId k

userKeyIdToMaybe :: PrimaryKey UserT Identity -> PrimaryKey UserT Maybe
userKeyIdToMaybe (UserId k) = UserId (Just k)

userKeyMaybeToId :: PrimaryKey UserT Maybe -> Maybe (PrimaryKey UserT Identity)
userKeyMaybeToId (UserId (Just k)) = Just (UserId k)
userKeyMaybeToId (UserId Nothing) = Nothing
