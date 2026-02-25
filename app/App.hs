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
    -- NOTE: # is the identifier used in str for string interpolation. Example
    -- [str|Hello, #{world}|].
    --
    -- We can escape this using ##. # at the end of the string removes the
    -- newline. So we use ## if # is at the end.
    putStrLn
        [str|
# Example of the starting point:
# start:
#   tag: ChainPoint
#   slot: 11617
#   blockHash: 9b65597bb73e21d5b58a1f5958f8b95324b142727efb2746c577998e93df3463

# sync_initial_local_state | empty_initial_local_state | without_local_state
running_mode: sync_initial_local_state

scripts:
  # The target script hash that we want to re-run locally.
  # In this example it's just a random hash, so please replace with your own.
  - script_hash: "22734734b6b0410cf4a0e3bd731fb98c55ac2b2a27b1eecb8d3b438c"
    # List of substitution scripts we want to run on wherever the target script
    # is found.
    substitutions:
      - # Name of the substitution script for easier identification (optional)
        name: "Local Policy"
        # The hash of the substitution script. The plutus-script-reexecutor will
        # check and fail if the hash does not match.
        ##
        # Tip: Leave this as an empty string and let plutus-script-reexecutor
        # error out with the expected script hash.
        hash: "6bfbd8fc6567153cbaacdcd0ee9fff9e69ba2a0eb62c129b303ade19"
        source:
          # The source of the substitution script. This can either be a
          # "file_path" or a "cbor_hex".
          cbor_hex: "4e4d01000033222220051200120011"
          # file_path: "local-config/policy-debug.plutus"

|]
