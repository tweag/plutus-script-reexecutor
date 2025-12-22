module PSR.Events where

import Control.Monad.STM qualified as STM
import Data.Time (getCurrentTime)
import PSR.Storage.Interface (Storage(..))
import PSR.Events.Interface
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)

withEvents :: Storage -> (Events -> IO ()) -> IO () 
withEvents s act = do 
  eventsChannel <- newBroadcastTChanIO 

  let
    getEventsChannel = eventsChannel

  let
    addCancellationEvent blockHeader scriptHash = do
      createdAt <- getCurrentTime
      STM.atomically $ writeTChan eventsChannel $ Event
        { eventType = Cancellation 
        , blockHeader
        , createdAt
        , payload = CancellationPayload scriptHash 
        }
      s.addCancellationEvent blockHeader scriptHash

  let
    addSelectionEvent blockHeader = do
      createdAt <- getCurrentTime
      STM.atomically $ writeTChan eventsChannel $ Event
        { eventType = Selection 
        , blockHeader
        , createdAt
        , payload = SelectionPayload
        }
      s.addSelectionEvent blockHeader

  let
    getEvents = s.getEvents

  let
    addExecutionEvent blockHeader executionPayload = do
      createdAt <- getCurrentTime
      STM.atomically $ writeTChan eventsChannel $ Event
        { eventType = Execution 
        , blockHeader
        , createdAt
        , payload = ExecutionPayload executionPayload
        }
      s.addExecutionEvent blockHeader executionPayload

  act $ Events {..}








