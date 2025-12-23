{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Use &&" -}
{- HLINT ignore "Use ||" -}
module PSR.Events.Interface where

import Control.Concurrent.STM.TChan (TChan)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import Cardano.Api (
    BlockHeader,
    ScriptHash,
    TxId,
 )
import Cardano.Api qualified as C
import Data.Maybe (isNothing)

data EventType
    = Execution
    | Selection
    | Cancellation
    deriving (Eq, Generic)

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

data ExecutionEventPayload = ExecutionEventPayload
    { transactionHash :: TxId
    , scriptHash :: ScriptHash
    , scriptName :: Maybe Text
    , trace :: Text
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
    , _eventFilterParam_name_or_script_hash :: Maybe Text
    }
    deriving (Generic)

data Events = Events
    { addExecutionEvent :: BlockHeader -> ExecutionEventPayload -> IO ()
    , addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    , addSelectionEvent :: BlockHeader -> IO ()
    , getEventsChannel :: TChan Event
    , getEvents :: EventFilterParams -> IO [Event]
    }

eventMatchesFilter :: EventFilterParams -> Event -> Bool
eventMatchesFilter (EventFilterParams typ time_begin time_end slot_begin slot_end _limit _offset name_or_script_hash) event =
    and
        [ check (event.eventType ==) typ
        , check (event.createdAt >=) time_begin
        , check (event.createdAt <=) time_end
        , check (slotNo >=) slot_begin
        , check (slotNo <=) slot_end
        , or
            [ isNothing name_or_script_hash
            , name_or_script_hash == mScriptName
            , name_or_script_hash == mScriptHashText
            ]
        ]
  where
    check :: (a -> Bool) -> Maybe a -> Bool
    check = maybe True

    C.BlockHeader (fromIntegral . C.unSlotNo -> slotNo) _hash _blockno = event.blockHeader

    mScriptName = case event.payload of
        ExecutionPayload eep -> eep.scriptName
        CancellationPayload{} -> Nothing
        SelectionPayload -> Nothing

    mScriptHashText = fmap C.textShow $ case event.payload of
        ExecutionPayload eep -> Just eep.scriptHash
        CancellationPayload hash -> Just hash
        SelectionPayload -> Nothing
