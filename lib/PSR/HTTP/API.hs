{- HLINT ignore "Use newtype instead of data" -}
{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.HTTP.API (
    ServerAPI,
    SiteRoutes (..),
    EventRoutes (..),
    Event(..),
    siteApi,
) where

import PSR.Events.Interface (Event(..), EventType(..), EventFilterParams(..), EventPayload(..), ExecutionEventPayload(..))

import Cardano.Api (BlockHeader(..))

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
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

instance ToJSON ExecutionEventPayload 
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
