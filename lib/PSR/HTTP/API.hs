{- HLINT ignore "Use newtype instead of data" -}
{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.HTTP.API (
    ServerAPI,
    EventType (..),
    EventFilterParams (..),
    SiteRoutes (..),
    EventRoutes (..),
    siteApi,
    Event(..),
) where

import Cardano.Api (BlockHeader(..))

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant
import Servant.QueryParam.Record (RecordParam)
import Servant.QueryParam.TypeLevel (DropPrefix, Eval, Exp)

instance ToJSON BlockHeader where
  toJSON (BlockHeader slotNo hash blockNo) = object
    [ "slot" .= toJSON slotNo
    , "block_hash" .= toJSON hash
    , "block_no" .= toJSON blockNo
    ]

data Event = Event
    { eventType :: EventType
    , blockHeader :: BlockHeader
    , created_at :: UTCTime
    }
    deriving (Generic)

instance ToJSON Event

data DropPrefixExp :: sym -> Exp sym
type instance Eval (DropPrefixExp sym) = DropPrefix sym

data EventType
    = Execution
    | Selection
    | Cancellation
    deriving (Generic)

instance ToJSON EventType

instance FromHttpApiData EventType where
    parseQueryParam = \case
        "execution" -> pure Execution
        "selection" -> pure Selection
        "cancellation" -> pure Cancellation
        _ -> Left "Unknown event type"

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

type EventFilterParams' = RecordParam DropPrefixExp EventFilterParams

data EventRoutes route = EventRoutes
    { allEvents :: route :- EventFilterParams' :> Get '[JSON] [Event]
    , namedEvents :: route :- EventFilterParams' :> Capture "name_or_script_hash" Text :> Get '[JSON] [Event]
    }
    deriving (Generic)

data SiteRoutes route = SiteRoutes
    { events :: route :- "events" :> NamedRoutes EventRoutes
    }
    deriving (Generic)

type ServerAPI = NamedRoutes SiteRoutes

siteApi :: Proxy ServerAPI
siteApi = Proxy
