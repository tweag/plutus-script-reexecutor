module PSR.Events where

import Cardano.Api qualified as C
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Monad.STM qualified as STM
import Data.Foldable (for_)
import Data.Time (getCurrentTime)
import PSR.Events.Interface
import PSR.Storage.Interface (Storage (..))
import PSR.Types (BlockStatus (..))

withEvents :: Maybe Storage -> (Events -> IO ()) -> IO ()
withEvents maybeStorage act = do
    eventsChannel <- newBroadcastTChanIO

    let
        getEventsChannel = eventsChannel

    let
        addRollbackEvent slotNo blockHash = do
            createdAt <- getCurrentTime
            blocksCancelled <-
                case maybeStorage of
                    Nothing -> pure []
                    Just s -> s.addRollbackEvent slotNo blockHash
            STM.atomically $
                writeTChan eventsChannel $
                    Event
                        { eventType = Rollback
                        , blockHash
                        , slotNo
                        , createdAt
                        , payload = RollbackPayload blocksCancelled
                        , blockStatus = BSCancelled
                        }

    let
        addSelectionEvent blockHeader = do
            createdAt <- getCurrentTime
            let (C.BlockHeader slotNo blockHash blockNo) = blockHeader
            STM.atomically $
                writeTChan eventsChannel $
                    Event
                        { eventType = Selection
                        , blockHash
                        , slotNo
                        , createdAt
                        , payload = SelectionPayload blockNo
                        , blockStatus = BSUnknown
                        }
            for_ maybeStorage $ \s ->
                s.addSelectionEvent blockHeader

    let
        addExecutionEvent blockHeader executionContextId payload@ExecutionEventPayload{..} = do
            createdAt <- getCurrentTime

            for_ maybeStorage $ \s ->
                s.addExecutionEvent executionContextId traceLogs evalError exUnits

            let (C.BlockHeader slotNo blockHash blockNo) = blockHeader
            let event =
                    Event
                        { eventType = Execution
                        , blockHash
                        , slotNo
                        , createdAt
                        , payload = ExecutionPayload blockNo payload
                        , blockStatus = BSUnknown
                        }
            STM.atomically $ writeTChan eventsChannel event
            pure event

    let
        addExecutionContext bh ec = case maybeStorage of
            Nothing -> pure $ ExecutionContextId 0 -- in case of no storage we return the null id because it will be ignored
            Just s -> s.addExecutionContext bh ec

    act $ Events{..}
