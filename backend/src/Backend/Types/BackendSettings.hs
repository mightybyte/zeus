{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.Types.BackendSettings where

------------------------------------------------------------------------------
import           Control.Error
import           Data.Aeson
import           Data.Bits
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word
import           Database.Beam
import           Network.Socket
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf
------------------------------------------------------------------------------

aesonOpts :: Int -> Options
aesonOpts n = defaultOptions
  { fieldLabelModifier = drop n
  }

data Cidr = Cidr
  { _cidrIp :: Word32
  , _cidrMask :: Int
  } deriving (Eq,Ord,Show,Read,Generic)

parseOctet :: Text -> Maybe Word8
parseOctet t = do
  val :: Int <- readMay $ T.unpack t
  if val < 256 then pure (fromIntegral val) else Nothing

parseIp :: Text -> Either String Word32
parseIp t =
  case T.splitOn "." t of
    [a,b,c,d] -> fmap tupleToHostAddress $ (,,,)
      <$> note "Error: 'a' component of IP must be an int" (parseOctet a)
      <*> note "Error: 'b' component of IP must be an int" (parseOctet b)
      <*> note "Error: 'c' component of IP must be an int" (parseOctet c)
      <*> note "Error: 'd' component of IP must be an int" (parseOctet d)
    _ -> Left "IP must have the form a.b.c.d"

parseCidr :: Text -> Either String Cidr
parseCidr t =
  case T.breakOn "/" t of
    (ip,"") -> Cidr <$> parseIp ip <*> pure 32
    (ip,mask) -> Cidr <$> parseIp ip
                      <*> note "CIDR mask must be an int" (readMay $ T.unpack $ T.drop 1 mask)

showIp :: Word32 -> String
showIp ip = intercalate "." [show a, show b, show c, show d]
  where
    (a,b,c,d) = hostAddressToTuple ip

showCidr :: Cidr -> String
showCidr (Cidr i m) = showIp i <> "/" <> show m

numOnesToMask :: Int -> Word32
numOnesToMask n = shift (2 ^ n - 1) (32 - n)

matchesCidr :: Word32 -> Cidr -> Bool
matchesCidr ip cidr = (ip .&. mask) == (_cidrIp cidr .&. mask)
  where
    mask = numOnesToMask (_cidrMask cidr)

ipMatchTest :: TestTree
ipMatchTest =
    testGroup "matchesCidr" $
      f "184.72.104.138" 32 ++
      f "184.72.104.138" 24 ++
      f "184.72.104.138" 16 ++
      f "184.72.104.138" 8
  where
    f ipStr mask =
      let ip = either error id $ parseIp ipStr
       in testCase "mask matches itself" (matchesCidr ip (Cidr ip mask) @?= True) :
          map (twiddleCheck ip mask) [0..31]
    twiddleCheck ip mask n =
      testCase (printf "twiddle %d is correct" n) $
        matchesCidr (complementBit ip n) (Cidr ip mask) @?= twiddleMatches mask n
    twiddleMatches mask i = i < (32 - mask)

instance ToJSON Cidr where
    toJSON = String . T.pack . showCidr
    toEncoding = toEncoding . showCidr

instance FromJSON Cidr where
    parseJSON = withText "Cidr" (either (fail "Invalid Cidr format") pure . parseCidr)

------------------------------------------------------------------------------
-- | These are settings that should not be exposed to the frontend.
data BackendSettings = BackendSettings
  { _beSettings_webhookBaseUrl :: Maybe Text
  , _beSettings_ipWhitelist :: [Cidr]
  } deriving (Eq,Ord,Show,Read,Generic)

instance ToJSON BackendSettings where
    toEncoding = genericToEncoding (aesonOpts 12)

instance FromJSON BackendSettings where
    parseJSON = genericParseJSON (aesonOpts 12)
