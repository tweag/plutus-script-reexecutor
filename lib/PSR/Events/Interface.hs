{- HLINT ignore "Use newtype instead of data" -}
module PSR.Events.Interface where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Control.Concurrent.STM.TChan (TChan)

import Cardano.Api (
    BlockHeader,
    ScriptHash,
    TxId,
 )

data EventType
    = Execution
    | Selection
    | Cancellation
    deriving (Generic)

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
    } deriving (Generic)

data ExecutionEventPayload = ExecutionEventPayload
    { transactionHash :: TxId
    , scriptHash :: ScriptHash
    , scriptName :: Maybe Text
    , trace :: Text
    } deriving (Generic)

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

