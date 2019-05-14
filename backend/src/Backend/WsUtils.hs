module Backend.WsUtils where

------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import           Snap.Core
------------------------------------------------------------------------------

wsHandler :: MonadSnap m => (WS.Connection -> IO ()) -> m ()
wsHandler m = do
    WS.runWebSocketsSnap $ \pc -> do
      conn <- WS.acceptRequest pc
      WS.forkPingThread conn 10
      m conn

wsReceive :: FromJSON a => WS.Connection -> IO (Either String a)
wsReceive conn = do
    dm <- WS.receiveDataMessage conn
    return $ eitherDecode' $ dataToBs dm

wsSend :: ToJSON a => WS.Connection -> a -> IO ()
wsSend conn v = WS.sendTextData conn $ encode v

dataToBs :: WS.DataMessage -> BSL.ByteString
dataToBs (WS.Text bs _) = bs
dataToBs (WS.Binary bs) = bs
