--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Errors
import Options

import Cardano.Api qualified as C
import Control.Concurrent.Async qualified as Async
import Options.Applicative
import PSR.ConfigMap qualified as CM
import PSR.HTTP qualified as HTTP
import PSR.Logging qualified as Logging
import PSR.Logging.LogBase qualified as LogBase
import PSR.Storage.SQLite qualified as Storage
import PSR.Streaming qualified as Streaming
import PSR.Types

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    config@CM.ConfigMap{..} <- CM.readConfigMap scriptYaml networkId socketPath >>= either error pure

    start <- maybe (C.chainTipToChainPoint <$> C.getLocalChainTip _cmLocalNodeConn) pure _cmStart
    let points = [start]

    LogBase.withStdOutLogging "Plutus Script Re-executor" $ \(logger, loggerEnv) -> do
        Storage.withSqliteStorage sqlitePath $ \storage ->
            Async.withAsync (HTTP.run loggerEnv storage httpServerPort) $ \serverAsync -> do
                Async.link serverAsync

                let appConf = AppConfig logger config storage
                res <- runApp appConf $ do
                    Logging.logMsgR Logging.Message "Started..."
                    Streaming.mainLoop points
                either (print @PSRErrors) return res
