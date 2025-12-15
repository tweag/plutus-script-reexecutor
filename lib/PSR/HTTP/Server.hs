module PSR.HTTP.Server (
    run,
) where

import PSR.HTTP.API
import PSR.Storage.Interface (Storage(..))

import Control.Monad.IO.Class (liftIO)
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

server :: Storage -> Server ServerAPI 
server Storage{..} = eventsH
  where
    eventsH params = (eventsHandler params Nothing) :<|> (eventsHandler params . Just)

    eventsHandler filterParams' mName = do
      -- The capture parameter `name` has a higher priority over the query param
      let nameFilterParameter = maybe (_filterQueryParam_name filterParams') Just mName
      let filterParams = filterParams' { _filterQueryParam_name = nameFilterParameter }
      _events <- liftIO $ getEvents filterParams
      pure []

run :: Storage -> Warp.Port -> IO ()
run storage port = do
  _ <- register ghcMetrics
  Warp.run port (prometheus def $ serve serverApi (server storage))
