--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Concurrent.Async qualified as Async
import Data.Foldable (for_)
import Options
import Options.Applicative (customExecParser, helpLongEquals, prefs, showHelpOnEmpty)
import PSR.ConfigMap qualified as CM
import PSR.Events qualified as Events
import PSR.Events.FileLogger qualified as EventsFileLogger
import PSR.HTTP qualified as HTTP
import PSR.Storage.SQLite qualified as Storage
import PSR.Streaming qualified as Streaming
import Streamly.Internal.Unicode.String (str)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    commandParser <- mkPsrCommands
    let parserPrefs = prefs $ helpLongEquals <> showHelpOnEmpty
    customExecParser parserPrefs commandParser >>= \case
        (Run opts) -> run opts
        GenerateScriptsExample -> generateScriptsExample

run :: Options -> IO ()
run Options{..} = do
    config@CM.ConfigMap{..} <-
        CM.readConfigMap scriptYaml networkId socketPath leashId >>= either error pure

    start <- maybe (C.chainTipToChainPoint <$> C.getLocalChainTip cmLocalNodeConn) pure cmStart
    let points = [start]

    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Starting..."

    let
        withMaybeStorage act = case sqlitePath of
            Nothing -> do
                putStrLn "Not using storage."
                act Nothing
            Just dbPath -> do
                putStrLn $ "Using sqlite storage at " <> dbPath
                Storage.withSqliteStorage dbPath (act . Just)

    withMaybeStorage $ \maybeStorage ->
        Events.withEvents maybeStorage $ \events -> do
            let runMainLoop = Streaming.mainLoop events config points
            let runWebServer = HTTP.run config maybeStorage events httpServerPort
            let runFileLogger = for_ logsPath (EventsFileLogger.run events)
            Async.mapConcurrently_ id [runWebServer, runFileLogger, runMainLoop]

generateScriptsExample :: IO ()
generateScriptsExample = do
    putStrLn
        [str|scripts:
  # The script hash of the script on-chain that we want to substitute.
  # In this example it's just a random hash, so please replace with your own.
  - script_hash: \"b6a7467ea1deb012808ef4e87b5ff371e85f7142d7b356a40d9b42a0\"

    # Script name or alias for easier identification (optional).
    name: always_succeeds

    # The script that we want to run instead of the original script identified by the script hash above,
    # in this case it's AlwaysSucceeds.
    cborHex: \"4e4d01000033222220051200120011\"

# Example of the starting point:
# start:
#   tag: ChainPoint
#   slot: 11617
#   blockHash: 9b65597bb73e21d5b58a1f5958f8b95324b142727efb2746c577998e93df3463
|]
