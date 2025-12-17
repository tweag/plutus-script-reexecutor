module PSR.HTTP.Server (
    run,
) where

import PSR.HTTP.API
import PSR.Storage.Interface (Storage (..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Default (def)
import Log (LogT (unLogT), LoggerEnv (leComponent))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Log (mkLogMiddleware)
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
        -- The capture parameter `name` has a higher priority over the query param
        let nameFilterParameter = maybe (_filterQueryParam_name filterParams') Just mName
        let filterParams = filterParams'{_filterQueryParam_name = nameFilterParameter}
        _events <- liftIO $ getEvents filterParams
        pure []

run :: LoggerEnv -> Storage -> Warp.Port -> IO ()
run logEnv storage port = do
    _ <- register ghcMetrics
    logMiddleware' <- flip runReaderT logEnv{leComponent = "PSR-http"} $ unLogT mkLogMiddleware
    Warp.run port (logMiddleware' $ \_id -> prometheus def $ serve siteApi (server storage))
