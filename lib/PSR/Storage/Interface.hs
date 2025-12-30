module PSR.Storage.Interface (
    Storage (..),
    ExecutionContextId (..),
) where

import Cardano.Ledger.Plutus (ExUnits)
import PSR.Events.Interface (
    EvalError,
    Event,
    EventFilterParams,
    ExecutionContext,
    TraceLogs,
 )

import Cardano.Api (
    BlockHeader,
    ScriptHash,
 )

newtype ExecutionContextId = ExecutionContextId {getExecutionContextId :: Integer}

data Storage = Storage
    { addExecutionEvent :: ExecutionContextId -> TraceLogs -> Maybe EvalError -> ExUnits -> IO ()
    , addExecutionContext :: BlockHeader -> ExecutionContext -> IO ExecutionContextId
    , addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    , addSelectionEvent :: BlockHeader -> IO ()
    , getEvents :: EventFilterParams -> IO [Event]
    }
