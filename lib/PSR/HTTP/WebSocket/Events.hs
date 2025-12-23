module PSR.HTTP.WebSocket.Events where

import Control.Concurrent.STM.TChan (dupTChan, readTChan)
import Control.Monad (forever, when)
import Control.Monad.STM qualified as STM
import Data.Aeson qualified as Aeson
import Network.WebSockets (
    PendingConnection,
    acceptRequest,
    sendTextData,
    withPingThread,
 )

import PSR.Events.Interface (EventFilterParams, Events (..), eventMatchesFilter)
import PSR.HTTP.API ()

eventsWebSocketHandler :: Events -> EventFilterParams -> PendingConnection -> IO ()
eventsWebSocketHandler events filterParams pendingConn = do
    conn <- acceptRequest pendingConn
    inputChannel <- STM.atomically $ dupTChan events.getEventsChannel

    withPingThread conn 30 (pure ()) $ forever $ do
        event <- STM.atomically $ readTChan inputChannel
        when (eventMatchesFilter filterParams event) $
            sendTextData conn (Aeson.encode event)
