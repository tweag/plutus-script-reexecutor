{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Use &&" -}
{- HLINT ignore "Use ||" -}
module PSR.Events.Interface where

import Cardano.Api (
    BlockHeader,
    ScriptHash,
    TxId,
 )
import Cardano.Api qualified as C
import Cardano.Ledger.Plutus (CostModel, ExUnits)
import Control.Concurrent.STM.TChan (TChan)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import PlutusLedgerApi.Common (Data, MajorProtocolVersion, PlutusLedgerLanguage)

data EventType
    = Execution
    | Selection
    | Cancellation
    deriving (Eq, Show, Generic)

data EventPayload
    = ExecutionPayload ExecutionEventPayload
    | CancellationPayload ScriptHash
    | SelectionPayload
    deriving (Generic)

data Event = Event
    { eventType :: EventType
    , blockHeader :: BlockHeader
    , createdAt :: UTCTime
    , payload :: EventPayload
    }
    deriving (Generic)

newtype TraceLogs = TraceLogs {getTraceLogs :: [Text]} deriving (Eq, Show, Generic)

data ScriptInfo = ScriptInfo
    { hash :: C.ScriptHash
    , name :: Maybe Text
    }
    deriving (Show, Generic)

data ExecutionContext = ExecutionContext
    { transactionHash :: TxId
    , targetScript :: ScriptInfo
    , shadowScript :: ScriptInfo
    , ledgerLanguage :: PlutusLedgerLanguage
    , majorProtocolVersion :: MajorProtocolVersion
    , datum :: Maybe Data
    , redeemer :: Maybe Data
    , scriptContext :: Data
    , exMaxBudget :: ExUnits
    , costModel :: CostModel
    }
    deriving (Show, Generic)

newtype EvalError = EvalError Text deriving (Show) via Text

newtype ExecutionContextId = ExecutionContextId {getExecutionContextId :: Integer}

data ExecutionEventPayload = ExecutionEventPayload
    { traceLogs :: TraceLogs
    , evalError :: Maybe EvalError
    , exUnits :: ExUnits
    , context :: ExecutionContext
    }
    deriving (Generic)

data EventFilterParams = EventFilterParams
    { _eventFilterParam_type :: Maybe EventType
    , _eventFilterParam_time_begin :: Maybe UTCTime
    , _eventFilterParam_time_end :: Maybe UTCTime
    , _eventFilterParam_slot_begin :: Maybe Integer
    , _eventFilterParam_slot_end :: Maybe Integer
    , _eventFilterParam_limit :: Maybe Integer
    , _eventFilterParam_offset :: Maybe Integer
    , _eventFilterParam_target_name_or_script_hash :: Maybe Text
    , _eventFilterParam_shadow_name_or_script_hash :: Maybe Text
    }
    deriving (Generic)

data Events = Events
    { addExecutionEvent :: BlockHeader -> ExecutionContextId -> ExecutionEventPayload -> IO Event
    , addExecutionContext :: BlockHeader -> ExecutionContext -> IO ExecutionContextId
    , addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    , addSelectionEvent :: BlockHeader -> IO ()
    , getEventsChannel :: TChan Event
    }

eventMatchesFilter :: EventFilterParams -> Event -> Bool
eventMatchesFilter
    ( EventFilterParams
            typ
            time_begin
            time_end
            slot_begin
            slot_end
            _limit
            _offset
            target_name_or_script_hash
            shadow_name_or_script_hash
        )
    event =
        and
            [ check (event.eventType ==) typ
            , check (event.createdAt >=) time_begin
            , check (event.createdAt <=) time_end
            , check (slotNo >=) slot_begin
            , check (slotNo <=) slot_end
            , or
                [ isNothing target_name_or_script_hash
                , target_name_or_script_hash == mTargetScriptName
                , target_name_or_script_hash == mTargetScriptHash
                ]
            , or
                [ isNothing shadow_name_or_script_hash
                , shadow_name_or_script_hash == mShadowScriptName
                , shadow_name_or_script_hash == mShadowScriptHash
                ]
            ]
      where
        check :: (a -> Bool) -> Maybe a -> Bool
        check = maybe True

        C.BlockHeader (fromIntegral . C.unSlotNo -> slotNo) _hash _blockno = event.blockHeader

        (mShadowScriptName, mShadowScriptHash) = case event.payload of
            ExecutionPayload eep -> (eep.context.shadowScript.name, Just $ C.textShow eep.context.shadowScript.hash)
            CancellationPayload{} -> (Nothing, Nothing)
            SelectionPayload -> (Nothing, Nothing)

        (mTargetScriptName, mTargetScriptHash) = case event.payload of
            ExecutionPayload eep -> (eep.context.targetScript.name, Just $ C.textShow eep.context.targetScript.hash)
            CancellationPayload hash -> (Nothing, Just $ C.textShow hash) -- TODO: we can lookup the the script name from the configmap
            SelectionPayload -> (Nothing, Nothing)
