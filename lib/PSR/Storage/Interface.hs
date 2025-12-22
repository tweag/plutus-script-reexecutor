module PSR.Storage.Interface (
    Storage (..),
) where

import PSR.Events.Interface
    ( Event, EventFilterParams, ExecutionEventPayload )

import Cardano.Api (
    BlockHeader,
    ScriptHash,
 )

data Storage = Storage
    { addExecutionEvent :: BlockHeader -> ExecutionEventPayload -> IO ()
    , addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    , addSelectionEvent :: BlockHeader -> IO ()
    , getEvents :: EventFilterParams -> IO [Event]
    }
