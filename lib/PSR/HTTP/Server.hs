module PSR.HTTP.Server (
    run,
) where

import PSR.HTTP.API

import Data.Default (def)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Prometheus (prometheus)
import Prometheus (register)
import Prometheus.Metric.GHC (ghcMetrics)
import Servant
import Servant.QueryParam.Server.Record ()
import Servant.Server.Generic ()

serverApi :: Proxy ServerAPI
serverApi = Proxy

server :: Server ServerAPI
server = eventsH
  where
    eventsH params = (eventsHandler params Nothing) :<|> (eventsHandler params . Just)

    eventsHandler FilterQueryParams{} _mName = do
        pure []

run :: Warp.Port -> IO ()
run port = do
    _ <- register ghcMetrics

    Warp.run port (prometheus def $ serve serverApi server)
