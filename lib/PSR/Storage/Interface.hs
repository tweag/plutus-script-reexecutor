module PSR.Storage.Interface (
    Storage (..),
    FilterBy (..),
) where

import Cardano.Api (
    BlockHeader,
    Hash,
    SlotNo,
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

data FilterBy = ByNameOrHash Text | ByTxId Text | ByContextId Integer

data Storage = Storage
    { addExecutionEvent :: ExecutionContextId -> TraceLogs -> Maybe EvalError -> ExUnits -> IO ()
    , addExecutionContext :: BlockHeader -> ExecutionContext -> IO ExecutionContextId
    , addRollbackEvent :: SlotNo -> Hash BlockHeader -> IO [Hash BlockHeader]
    , addSelectionEvent :: BlockHeader -> IO ()
    , getEvents :: EventFilterParams -> IO [Event]
    , -- Returns the recent ExecutionContext that contains the provided name or script hash
      getExecutionContexts :: [FilterBy] -> IO [(BlockHeader, ExecutionContextId, ExecutionContext)]
    }
