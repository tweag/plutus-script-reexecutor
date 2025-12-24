--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Concurrent.Async qualified as Async
import Data.Function ((&))
import Options
import Options.Applicative
import PSR.ConfigMap qualified as CM
import PSR.Events qualified as Events
import PSR.Events.Interface (Events (..))
import PSR.HTTP qualified as HTTP
import PSR.Storage.SQLite qualified as Storage
import PSR.Streaming (LiveEvent (..), liveEventStream)
import PSR.Types (pCompact)
import Streamly.Data.Fold.Prelude as Fold
import Streamly.Data.Stream.Prelude as Stream
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    Options{..} <- execParser psrOpts
    config@CM.ConfigMap{..} <-
        CM.readConfigMap scriptYaml networkId socketPath >>= either error pure

    start <- maybe (C.chainTipToChainPoint <$> C.getLocalChainTip cmLocalNodeConn) pure cmStart
    let points = [start]

    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."

    Storage.withSqliteStorage sqlitePath $ \storage ->
        Events.withEvents storage $ \events ->
            Async.withAsync (HTTP.run events httpServerPort) $ \serverAsync -> do
                Async.link serverAsync
                liveEventStream config points
                    & Stream.trace debugPrint
                    & Stream.mapM (consumeEvent events)
                    & Stream.fold Fold.drain
  where
    debugPrint :: LiveEvent -> IO ()
    debugPrint (LeReexecutedTransaction res) = pCompact res
    debugPrint _ = pure ()

    consumeEvent :: Events -> LiveEvent -> IO ()
    consumeEvent events (LeRollForward h) = events.addSelectionEvent h
    consumeEvent _ _ = pure ()
