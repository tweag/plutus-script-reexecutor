{- HLINT ignore "Use newtype instead of data" -}
module PSR.HTTP.API (
    ServerAPI,
    EventType (..),
    FilterQueryParams (..),
    SiteRoutes (..),
    EventRoutes (..),
    siteApi,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Servant
import Servant.QueryParam.Record (RecordParam)
import Servant.QueryParam.TypeLevel (DropPrefix, Eval, Exp)

data Event = Event
    { content :: Text
    }
    deriving (Generic, Show)

instance FromJSON Event
instance ToJSON Event

data DropPrefixExp :: sym -> Exp sym
type instance Eval (DropPrefixExp sym) = DropPrefix sym

data EventType
    = Execution
    | Selection
    | Cancellation
    deriving (Generic)

instance FromHttpApiData EventType where
    parseQueryParam = \case
        "execution" -> pure Execution
        "selection" -> pure Selection
        "cancellation" -> pure Cancellation
        _ -> Left "Unknown event type"

data FilterQueryParams = FilterQueryParams
    { _filterQueryParam_type :: Maybe EventType
    , _filterQueryParam_time_begin :: Maybe UTCTime
    , _filterQueryParam_time_end :: Maybe UTCTime
    , _filterQueryParam_slot_begin :: Maybe Integer
    , _filterQueryParam_slot_end :: Maybe Integer
    , _filterQueryParam_limit :: Maybe Integer
    , _filterQueryParam_offset :: Maybe Integer
    , _filterQueryParam_name :: Maybe Text
    }
    deriving (Generic)

type EventFilterParams = RecordParam DropPrefixExp FilterQueryParams

data EventRoutes route = EventRoutes
    { allEvents :: route :- EventFilterParams :> Get '[JSON] [Event]
    , namedEvents :: route :- EventFilterParams :> Capture "script_hash_or_name" Text :> Get '[JSON] [Event]
    }
    deriving (Generic)

data SiteRoutes route = SiteRoutes
    { events :: route :- "events" :> NamedRoutes EventRoutes
    }
    deriving (Generic)

type ServerAPI = NamedRoutes SiteRoutes

siteApi :: Proxy ServerAPI
siteApi = Proxy
