module PSR.HTTP.Server (
    run,
) where

import PSR.HTTP.API as API
import PSR.Storage.Interface (Storage(..), Event(..))

import Data.Functor ((<&>))
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Prometheus (prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.QueryParam.Server.Record ()
import Servant.Server.Generic (AsServer)

server :: Storage -> Server ServerAPI
server Storage{..} = siteH
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
        events <- liftIO $ getEvents filterParams
        pure $ events <&> \e -> API.Event e.eventType e.blockHeader e.createdAt

run :: Storage -> Warp.Port -> IO ()
run storage port = do
    _ <- register ghcMetrics
    Warp.run port (prometheus def $ serve siteApi (server storage))
