{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Types.ProcMsg where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Monad
import           Data.Aeson
import           Data.Readable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           GHC.Generics
import           Text.Printf
------------------------------------------------------------------------------

data ProcMsgSource = CiMsg | BuildCommandMsg | StdoutMsg | StderrMsg
  deriving (Eq,Ord,Show,Read,Enum,Bounded,Generic)

instance ToJSON ProcMsgSource where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ProcMsgSource

instance Readable ProcMsgSource where
  fromText "CI" = return CiMsg
  fromText "CMD" = return BuildCommandMsg
  fromText "OUT" = return StdoutMsg
  fromText "ERR" = return StderrMsg
  fromText _ = mzero

prettyProcMsgSource :: ProcMsgSource -> String
prettyProcMsgSource CiMsg = "CI"
prettyProcMsgSource BuildCommandMsg = "CMD"
prettyProcMsgSource StdoutMsg = "OUT"
prettyProcMsgSource StderrMsg = "ERR"

data ProcMsg = ProcMsg
  { _procMsg_timestamp :: UTCTime
  , _procMsg_source :: ProcMsgSource
  , _procMsg_msg :: Text
  } deriving (Eq,Ord,Show,Read,Generic)

prettyProcMsg :: ProcMsg -> String
prettyProcMsg (ProcMsg t s m) =
  printf "%s [%s] %s" (prettyProcMsgSource s) (show t) m

parseProcMsg :: Text -> Either String ProcMsg
parseProcMsg msg = do
    ts <- note ("parseProcMsg: Error parsing timestamp: " <> t) $ readMay t
    src <- note ("parseProcMsg: Error parsing source: " <> t) $ fromText srcText
    return $ ProcMsg ts src $ T.drop 2 msg2
  where
    (a,msg2) = T.breakOn "] " msg
    (srcText, time) = T.breakOn " [" a
    t = T.unpack $ T.drop 2 time

textProcMsg :: Text -> IO ProcMsg
textProcMsg msg = do
    t <- getCurrentTime
    return $ ProcMsg t CiMsg msg

instance ToJSON ProcMsg where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ProcMsg
