--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Concurrent.Async qualified as Async
import Options
import Options.Applicative
import PSR.ConfigMap qualified as CM
import PSR.HTTP qualified as HTTP
import PSR.Storage.SQLite qualified as Storage
import PSR.Streaming qualified as Streaming

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    config@CM.ConfigMap{..} <-
        CM.readConfigMap scriptYaml networkId socketPath >>= either error pure

    start <- maybe (C.chainTipToChainPoint <$> C.getLocalChainTip cmLocalNodeConn) pure cmStart
    let points = [start]

    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."

    Storage.withSqliteStorage sqlitePath $ \storage ->
        Async.withAsync (HTTP.run storage httpServerPort) $ \serverAsync -> do
            Async.link serverAsync
            Streaming.mainLoop config points
