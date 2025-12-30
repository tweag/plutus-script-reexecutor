module PSR.Events where

import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Monad.STM qualified as STM
import Data.Time (getCurrentTime)
import PSR.Events.Interface
import PSR.Storage.Interface (Storage (..))

withEvents :: Storage -> (Events -> IO ()) -> IO ()
withEvents s act = do
    eventsChannel <- newBroadcastTChanIO

    let
        getEventsChannel = eventsChannel

    let
        addCancellationEvent blockHeader scriptHash = do
            createdAt <- getCurrentTime
            STM.atomically $
                writeTChan eventsChannel $
                    Event
                        { eventType = Cancellation
                        , blockHeader
                        , createdAt
                        , payload = CancellationPayload scriptHash
                        }
            s.addCancellationEvent blockHeader scriptHash

    let
        addSelectionEvent blockHeader = do
            createdAt <- getCurrentTime
            STM.atomically $
                writeTChan eventsChannel $
                    Event
                        { eventType = Selection
                        , blockHeader
                        , createdAt
                        , payload = SelectionPayload
                        }
            s.addSelectionEvent blockHeader

    let
        getEvents = s.getEvents

    let
        addExecutionEvent blockHeader payload@ExecutionEventPayload{..} = do
            createdAt <- getCurrentTime
            STM.atomically $
                writeTChan eventsChannel $
                    Event
                        { eventType = Execution
                        , blockHeader
                        , createdAt
                        , payload = ExecutionPayload payload
                        }
            executionContextId <- s.addExecutionContext blockHeader context
            s.addExecutionEvent executionContextId traceLogs evalError exUnits

    act $ Events{..}
