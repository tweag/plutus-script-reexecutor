{- HLINT ignore "Use <|>" -}
{- HLINT ignore "Avoid lambda using `infix`" -}
module PSR.HTTP.Server (
    run,
) where

import Cardano.Api qualified as C
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Traversable (forM)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Prometheus (prometheus)
import Network.WebSockets.Connection (PendingConnection)
import PSR.ConfigMap (ConfigMap (..))
import PSR.Evaluation
import PSR.Events.Interface (EvalError (..), EventFilterParams (..), Events (..), ExecutionContext (..), ExecutionEventPayload (..), TraceLogs (..))
import PSR.HTTP.API as API
import PSR.HTTP.WebSocket.Events (eventsWebSocketHandler)
import PSR.Storage.Interface (FilterBy (..), Storage (..))
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.QueryParam.Server.Record ()
import Servant.Server.Generic (AsServer)

server :: ConfigMap -> Storage -> Events -> Server ServerAPI
server cm storage events = siteH
  where
    siteH :: SiteRoutes AsServer
    siteH =
        SiteRoutes
            { events = eventsH
            , eventsWebSockets = eventsWSH
            , execute = executeH
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

    executeH :: Text -> ExecuteParams -> Handler [Event]
    executeH nameOrHash ExecuteParams{..} = do
        let filters = ByNameOrHash nameOrHash : catMaybes [ByTxId <$> _ep_tx_id, ByContextId <$> _ep_context_id]
        contexts <- liftIO $ storage.getExecutionContexts filters
        executions <-
            forM contexts $ \(blockHeader, eci, context@ExecutionContext{..}) ->
                forM (Map.lookup scriptHash cm.cmScripts) $ \rs -> do
                    (_, exUnits, logs, evalError') <- tryRunScriptInContext rs context
                    liftIO $
                        events.addExecutionEvent blockHeader eci $
                            ExecutionEventPayload
                                { traceLogs = TraceLogs logs
                                , exUnits
                                , evalError = EvalError . C.docToText . C.pretty <$> evalError'
                                , context
                                }
        pure $ catMaybes executions

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

run :: ConfigMap -> Storage -> Events -> Warp.Port -> IO ()
run cm storage events port = do
    _ <- register ghcMetrics
    let mainApp = serve siteApi (server cm storage events)
    Warp.run port (prometheus def mainApp)
