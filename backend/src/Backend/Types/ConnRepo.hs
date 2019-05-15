{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.Types.ConnRepo
  ( ConnId
  , ConnRepo
  , newConnRepo
  , addConnection
  , removeConnection
  , broadcast
  ) where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Aeson
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Network.WebSockets as WS
------------------------------------------------------------------------------
import           Common.Api
import           Backend.WsUtils
------------------------------------------------------------------------------

newtype ConnId = ConnId Int
  deriving (Eq,Ord,Show,Read,Enum)

newtype ConnRepo = ConnRepo
  { _connRepoIORef :: IORef (ConnId, Map ConnId WS.Connection)
  }

newConnRepo :: IO ConnRepo
newConnRepo = do
  ref <- newIORef (ConnId 0, mempty)
  return $ ConnRepo ref

addConnection :: WS.Connection -> ConnRepo -> IO (ConnId)
addConnection conn (ConnRepo ref) = atomicModifyIORef ref f
  where
    f (next, m) = ((succ next, M.insert next conn m), next)

removeConnection :: ConnId -> ConnRepo -> IO ()
removeConnection cid (ConnRepo ref) = atomicModifyIORef ref f
  where
    f (next, m) = ((next, M.delete cid m), ())

broadcast :: ConnRepo -> Down -> IO ()
broadcast (ConnRepo cRef) cmd = do
  (_,conns) <- readIORef cRef
  forM_ (M.elems conns) $ \(conn) -> do
    wsSend conn cmd
