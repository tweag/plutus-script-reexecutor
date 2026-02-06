module Options (Options (..), Command (..), mkPsrCommands) where

import Cardano.Api
import Data.Foldable (fold)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Ouroboros.Network.Protocol.LocalStateQuery.Type (LeashID (..))
import System.Environment.Blank (getEnvDefault)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- Q: I wonder if we can push everything into the ConfigMap and just take the
-- yaml file path as the only optional CLI option.
-- 1. It's very likely the socketPath and networkId don't ever change.
-- 2. Since yaml already allows composition via other yaml files, common config
--    can be shared.

data Options = Options
    { socketPath :: SocketPath
    , networkId :: NetworkId
    , scriptYaml :: FilePath
    , httpServerPort :: Port
    , logsPath :: Maybe FilePath
    , sqlitePath :: Maybe FilePath
    , leashId :: LeashID
    }
    deriving (Show, Eq)

mkParseOptions :: IO (Parser Options)
mkParseOptions = do
    nodeSocketPath <- getEnvDefault "CARDANO_NODE_SOCKET_PATH" ""
    nodeNetworkId <- getEnvDefault "CARDANO_NODE_NETWORK_ID" ""
    pure $
        Options
            <$> optSocketPath nodeSocketPath
            <*> optNetworkId nodeNetworkId
            <*> optScriptYaml
            <*> optHTTPServerPort
            <*> optional optLogsPath
            <*> optional optSqlitePath
            <*> optLeashId
  where
    optSocketPath nodeSocketPath =
        ( File
            <$> strOption
                ( long "node-socket"
                    <> metavar "CARDANO_NODE_SOCKET_PATH"
                    <> help "Path to the cardano-node socket"
                )
        )
            <|> ( case nodeSocketPath of
                    "" -> empty
                    _ -> pure $ File nodeSocketPath
                )

    optNetworkId nodeNetworkId =
        optMainNet
            <|> optTestNet
            <|> ( case readMaybe nodeNetworkId of
                    Nothing -> empty
                    Just networkId -> pure $ Testnet $ NetworkMagic networkId
                )
    optMainNet = Mainnet <$ flag' () (long "mainnet")
    optTestNet =
        Testnet . NetworkMagic
            <$> option
                auto
                ( long "testnet-magic"
                    <> metavar "CARDANO_NODE_TESTNET_MAGIC"
                    <> help "Network magic"
                )
    optScriptYaml =
        strOption
            ( long "script-yaml"
                <> metavar "SCRIPT_YAML"
                <> help "Path to script.yaml"
            )
    optHTTPServerPort =
        option
            auto
            ( long "http-server-port"
                <> metavar "HTTP_SERVER_PORT"
                <> help "Port of the http server"
                <> value 8080
            )
    optLogsPath =
        strOption
            ( long "logs-path"
                <> metavar "LOGS_PATH"
                <> help "Path to the logs file"
            )
    optSqlitePath =
        strOption
            ( long "sqlite-path"
                <> metavar "SQLITE_PATH"
                <> help "Path to sqlite database"
            )
    optLeashId =
        LeashID
            <$> option
                auto
                ( long "leash-id"
                    <> metavar "LEASH_ID"
                    <> help "ID to use when leashing the node"
                )

mkPsrOpts :: IO (ParserInfo Options)
mkPsrOpts = do
    parseOptions <- mkParseOptions
    pure $
        info
            (parseOptions <**> helper)
            (progDesc "Run the plutus-script-reexecutor service")

data Command = Run Options | GenerateScriptsExample

mkPsrCommands :: IO (ParserInfo Command)
mkPsrCommands = do
    psrOpts <- mkPsrOpts
    let parser =
            subparser $
                fold
                    [ command "run" (Run <$> psrOpts)
                    , command "generate-scripts-example" (info (pure GenerateScriptsExample) (progDesc "Generate scripts.yaml example file"))
                    ]
    pure $
        info
            (parser <**> helper)
            idm
