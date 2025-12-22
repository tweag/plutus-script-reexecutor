module PSR.Websocket.Server where

import Network.WebSockets (
  PendingConnection,
  acceptRequest,
  sendTextData,
  withPingThread,
 )
import Data.Aeson qualified as Aeson
import Control.Concurrent.STM.TChan (dupTChan, readTChan)
import Control.Monad.STM qualified as STM
import Control.Monad (forever)

import PSR.HTTP.API ()
import PSR.Events.Interface (Events(..))

wsServer :: Events -> PendingConnection -> IO ()
wsServer events pendingConn = do
    conn <- acceptRequest pendingConn 
    inputChannel <- STM.atomically $ dupTChan events.getEventsChannel 

    withPingThread conn 30 (pure ()) $ forever $ do
      event <- STM.atomically $ readTChan inputChannel 
      sendTextData conn $ Aeson.encode event 

