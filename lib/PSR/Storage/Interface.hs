module PSR.Storage.Interface (
    Storage (..),
) where

import Cardano.Api (
    BlockHeader,
    ScriptHash,
 )
import Cardano.Ledger.Plutus (ExUnits)
import Data.Text (Text)
import PSR.Events.Interface (
    EvalError,
    Event,
    EventFilterParams,
    ExecutionContext,
    ExecutionContextId,
    TraceLogs,
 )

data Storage = Storage
    { addExecutionEvent :: ExecutionContextId -> TraceLogs -> Maybe EvalError -> ExUnits -> IO ()
    , addExecutionContext :: BlockHeader -> ExecutionContext -> IO ExecutionContextId
    , addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    , addSelectionEvent :: BlockHeader -> IO ()
    , getEvents :: EventFilterParams -> IO [Event]
    , -- Returns the recent ExecutionContext that contains the provided name or script hash
      getExecutionContextByNameOrScriptHash :: Text -> IO (BlockHeader, ExecutionContextId, ExecutionContext)
    }
