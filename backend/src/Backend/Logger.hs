{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Logger where

------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Database.Beam
import           Database.Beam.Schema.Tables
import           Database.Beam.Sqlite
import           Database.SQLite.Simple
------------------------------------------------------------------------------
import           Backend.Db
import           Common.Types.Builder
import           Common.Types.ZeusMsg
------------------------------------------------------------------------------

logZeus :: Connection -> [ZeusMsgT Maybe] -> IO ()
logZeus conn ms = do
    runBeamSqlite conn $ do
      let msgExpr (ZeusMsg _ (BuilderId (Just b)) (Just t) (Just l) (Just msg)) =
            Just $ ZeusMsg default_ (val_ $ BuilderId b) (val_ t) (val_ l) (val_ msg)
          msgExpr _ = Nothing
      runInsert $ insert (_ciDb_zeusMsgs ciDb) $ insertExpressions $
        catMaybes $ map msgExpr ms
  
logText :: Connection -> LogLevel -> MBuilderId -> Text -> IO ()
logText conn level (BuilderId builderId) msg = do
    t <- getCurrentTime
    logZeus conn [ZeusMsg Nothing (BuilderId (Just builderId)) (Just t)
                          (Just level) (Just msg)]

logTexts :: Connection -> LogLevel -> MBuilderId -> [Text] -> IO ()
logTexts conn level (BuilderId builderId) msgs = do
    t <- getCurrentTime
    logZeus conn (map (ZeusMsg Nothing (BuilderId (Just builderId)) (Just t)
                          (Just level) . Just) msgs)

logStr :: Connection -> LogLevel -> MBuilderId -> String -> IO ()
logStr conn level builderId msg = logText conn level builderId $ T.pack msg


fromNullable :: Columnar' (Nullable w) a -> Columnar' w (Maybe a)
fromNullable ~(Columnar' x) = Columnar' x

toNullable   :: Columnar' w (Maybe a) -> Columnar' (Nullable w) a
toNullable ~(Columnar' x) = Columnar' x

