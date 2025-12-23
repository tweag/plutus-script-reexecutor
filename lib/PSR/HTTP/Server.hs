{- HLINT ignore "Use <|>" -}
{- HLINT ignore "Avoid lambda using `infix`" -}
module PSR.HTTP.Server (
    run,
) where

import PSR.Events.Interface (EventFilterParams (..), Events (..))
import PSR.HTTP.API as API
import PSR.HTTP.WebSocket.Events (eventsWebSocketHandler)

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Text (Text)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Prometheus (prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.QueryParam.Server.Record ()
import Servant.Server.Generic (AsServer)

import Network.WebSockets.Connection (PendingConnection)

server :: Events -> Server ServerAPI
server events = siteH
  where
    siteH :: SiteRoutes AsServer
    siteH =
        SiteRoutes
            { events = eventsH
            , eventsWebSockets = eventsWSH
            }

    eventsH :: EventRoutes AsServer
    eventsH =
        EventRoutes
            { allEvents =
                \params -> eventsHandler params Nothing
            , namedEvents =
                \params -> eventsHandler params . Just
            }

    eventsHandler filterParams mName =
        liftIO $ events.getEvents (filtersWithName mName filterParams)

    eventsWSH :: EventsWebSockets AsServer
    eventsWSH =
        EventsWebSockets
            { allEventsWebSocket =
                \params conn -> eventsWSHandler params Nothing conn
            , namedEventsWebSocket =
                \params name_or_hash conn -> eventsWSHandler params (Just name_or_hash) conn
            }

    eventsWSHandler :: EventFilterParams -> Maybe Text -> PendingConnection -> Handler ()
    eventsWSHandler filterParams mName pconn =
        liftIO $ eventsWebSocketHandler events (filtersWithName mName filterParams) pconn

    filtersWithName :: Maybe Text -> EventFilterParams -> EventFilterParams
    filtersWithName Nothing filterParams = filterParams
    filtersWithName (Just name) filterParams =
        filterParams{_eventFilterParam_name_or_script_hash = Just name}

run :: Events -> Warp.Port -> IO ()
run events port = do
    _ <- register ghcMetrics
    let mainApp = serve siteApi (server events)
    Warp.run port (prometheus def mainApp)
