{- HLINT ignore "Use <|>" -}
module PSR.HTTP.Server (
    run,
) where

import PSR.HTTP.API as API
import PSR.Events.Interface (Events(..), EventFilterParams(..))
import PSR.Websocket.Server (wsServer)

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Prometheus (prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.QueryParam.Server.Record ()
import Servant.Server.Generic (AsServer)

import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (
  defaultConnectionOptions,
 )

server :: Events -> Server ServerAPI
server Events{getEvents} = siteH
  where
    siteH :: SiteRoutes AsServer
    siteH =
        SiteRoutes
            { events = eventsH
            }

    eventsH :: EventRoutes AsServer
    eventsH =
        EventRoutes
            { allEvents = \params -> eventsHandler params Nothing
            , namedEvents = \params -> eventsHandler params . Just
            }

    eventsHandler filterParams' mName = do
        -- The capture parameter `name_or_script_hash` has a higher priority over the query param
        let nameOrScriptHashFilterParameter = maybe (_eventFilterParam_name_or_script_hash filterParams') Just mName
        let filterParams = filterParams' {_eventFilterParam_name_or_script_hash = nameOrScriptHashFilterParameter}
        liftIO $ getEvents filterParams

run :: Events -> Warp.Port -> IO ()
run events port = do
    _ <- register ghcMetrics
    let mainApp = websocketsOr defaultConnectionOptions (wsServer events) $ serve siteApi (server events)
    Warp.run port (prometheus def mainApp) 
