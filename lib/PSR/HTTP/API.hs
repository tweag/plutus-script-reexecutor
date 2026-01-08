{- HLINT ignore "Use newtype instead of data" -}
{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.HTTP.API (
    ServerAPI,
    SiteRoutes (..),
    EventRoutes (..),
    EventsWebSockets (..),
    Event (..),
    ExecuteParams (..),
    siteApi,
) where

import Cardano.Api (BlockHeader (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import PSR.Events.Interface
import PlutusLedgerApi.Common (MajorProtocolVersion (..), PlutusLedgerLanguage (..))
import Servant
import Servant.API.WebSocket (WebSocketPending)
import Servant.QueryParam.Record (RecordParam)
import Servant.QueryParam.TypeLevel (DropPrefix, Eval, Exp)

instance ToJSON BlockHeader where
    toJSON (BlockHeader slotNo hash blockNo) =
        object
            [ "slot" .= toJSON slotNo
            , "block_hash" .= toJSON hash
            , "block_no" .= toJSON blockNo
            ]

deriving newtype instance ToJSON TraceLogs
deriving newtype instance ToJSON EvalError
deriving newtype instance ToJSON MajorProtocolVersion
deriving instance ToJSON PlutusLedgerLanguage

instance ToJSON ExecutionEventPayload where
    toJSON ExecutionEventPayload{..} =
        object
            [ "transactionHash" .= toJSON context.transactionHash
            , "scriptHash" .= toJSON context.scriptHash
            , "scriptName" .= toJSON context.scriptName
            , "exUnits" .= toJSON exUnits
            , "traceLogs" .= toJSON traceLogs
            , "evalError" .= toJSON evalError
            ]

instance ToJSON EventPayload
instance ToJSON Event

data DropPrefixExp :: sym -> Exp sym
type instance Eval (DropPrefixExp sym) = DropPrefix sym

instance ToJSON EventType

instance FromHttpApiData EventType where
    parseQueryParam = \case
        "execution" -> pure Execution
        "selection" -> pure Selection
        "cancellation" -> pure Cancellation
        _ -> Left "Unknown event type"

type EventFilterParams' = RecordParam DropPrefixExp EventFilterParams

data EventRoutes route = EventRoutes
    { allEvents ::
        route :- EventFilterParams' :> Get '[JSON] [Event]
    , namedEvents ::
        route :- EventFilterParams' :> Capture "name_or_script_hash" Text :> Get '[JSON] [Event]
    }
    deriving (Generic)

data EventsWebSockets route = EventsWebSockets
    { allEventsWebSocket ::
        route :- EventFilterParams' :> WebSocketPending
    , namedEventsWebSocket ::
        route :- EventFilterParams' :> Capture "name_or_script_hash" Text :> WebSocketPending
    }
    deriving (Generic)

data ExecuteParams = ExecuteParams
    { _ep_context_id :: Maybe Integer
    , _ep_tx_id :: Maybe Text
    }
    deriving (Generic)

data SiteRoutes route = SiteRoutes
    { events ::
        route :- "events" :> NamedRoutes EventRoutes
    , eventsWebSockets ::
        route :- "events-ws" :> NamedRoutes EventsWebSockets
    , execute :: route :- "execute" :> Capture "name_or_script_hash" Text :> RecordParam DropPrefixExp ExecuteParams :> Get '[JSON] [Event]
    }
    deriving (Generic)

type ServerAPI = NamedRoutes SiteRoutes

siteApi :: Proxy ServerAPI
siteApi = Proxy
