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
import PSR.Events.Interface (EvalError (..), EventFilterParams (..), Events (..), ExecutionContext (..), ExecutionEventPayload (..), ScriptInfo (..), TraceLogs (..))
import PSR.HTTP.API as API
import PSR.HTTP.WebSocket.Events (eventsWebSocketHandler)
import PSR.Storage.Interface (FilterBy (..), Storage (..))
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.QueryParam.Server.Record ()
import Servant.Server.Generic (AsServer)

server :: ConfigMap -> Maybe Storage -> Events -> Server ServerAPI
server cm maybeStorage events = siteH
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

    eventsHandler filterParams mName = case maybeStorage of
        Nothing -> pure []
        Just storage -> liftIO $ storage.getEvents (filtersWithTargetName mName filterParams)

    executeH :: Text -> ExecuteParams -> Handler [Event]
    executeH nameOrHash ExecuteParams{..} = do
        -- TODO: Allow to filter by shadow nameOrHash?
        let filters = ByNameOrHash nameOrHash : catMaybes [ByTxId <$> _ep_tx_id, ByContextId <$> _ep_context_id]

        case maybeStorage of
            Nothing -> pure []
            Just storage -> do
                contexts <- liftIO $ storage.getExecutionContexts filters
                executions <-
                    forM contexts $ \(blockHeader, eci, context@ExecutionContext{..}) ->
                        forM (Map.lookup targetScript.hash cm.cmShadowScripts) $ \rss -> forM rss $ \rs -> do
                            (_, exUnits, logs, evalError') <- tryRunScriptInContext rs context
                            liftIO $
                                events.addExecutionEvent blockHeader eci $
                                    ExecutionEventPayload
                                        { traceLogs = TraceLogs logs
                                        , exUnits
                                        , evalError = EvalError . C.docToText . C.pretty <$> evalError'
                                        , context
                                        }
                pure $ concat $ catMaybes executions

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
        liftIO $ eventsWebSocketHandler events (filtersWithTargetName mName filterParams) pconn

    filtersWithTargetName :: Maybe Text -> EventFilterParams -> EventFilterParams
    filtersWithTargetName Nothing filterParams = filterParams
    filtersWithTargetName (Just name) filterParams =
        filterParams{_eventFilterParam_target_name_or_script_hash = Just name}

run :: ConfigMap -> Maybe Storage -> Events -> Warp.Port -> IO ()
run cm maybeStorage events port = do
    _ <- register ghcMetrics
    let mainApp = serve siteApi (server cm maybeStorage events)
    Warp.run port (prometheus def mainApp)
