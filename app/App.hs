--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Concurrent.Async qualified as Async
import Options
import Options.Applicative (customExecParser, helpLongEquals, prefs, showHelpOnEmpty)
import PSR.ConfigMap qualified as CM
import PSR.Events qualified as Events
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
        CM.readConfigMap scriptYaml networkId socketPath >>= either error pure

    start <- maybe (C.chainTipToChainPoint <$> C.getLocalChainTip cmLocalNodeConn) pure cmStart
    let points = [start]

    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."

    Storage.withSqliteStorage sqlitePath $ \storage ->
        Events.withEvents storage $ \events ->
            Async.withAsync (HTTP.run config storage events httpServerPort) $ \serverAsync -> do
                Async.link serverAsync
                Streaming.mainLoop events config points

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
