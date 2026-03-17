{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Use &&" -}
{- HLINT ignore "Use ||" -}
module PSR.Events.Interface where

import Cardano.Api (
    BlockHeader,
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
    | Rollback
    deriving (Eq, Show, Generic)

data EventPayload
    = ExecutionPayload C.BlockNo ExecutionEventPayload
    | RollbackPayload [C.Hash C.BlockHeader]
    | SelectionPayload C.BlockNo
    deriving (Generic)

data Event = Event
    { eventType :: EventType
    , blockHash :: C.Hash BlockHeader
    , slotNo :: C.SlotNo
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
    , addRollbackEvent :: C.SlotNo -> C.Hash BlockHeader -> IO ()
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

        slotNo = fromIntegral . C.unSlotNo $ event.slotNo

        (mShadowScriptName, mShadowScriptHash) = case event.payload of
            ExecutionPayload _ eep -> (eep.context.shadowScript.name, Just $ C.textShow eep.context.shadowScript.hash)
            RollbackPayload{} -> (Nothing, Nothing)
            SelectionPayload{} -> (Nothing, Nothing)

        (mTargetScriptName, mTargetScriptHash) = case event.payload of
            ExecutionPayload _ eep -> (eep.context.targetScript.name, Just $ C.textShow eep.context.targetScript.hash)
            RollbackPayload{} -> (Nothing, Nothing)
            SelectionPayload{} -> (Nothing, Nothing)
