module Options where

import Cardano.Api
import Network.Wai.Handler.Warp (Port)
import Options.Applicative

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
    , sqlitePath :: FilePath
    }
    deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions =
    Options
        <$> optSocketPath
        <*> optNetworkId
        <*> optScriptYaml
        <*> optHTTPServerPort
        <*> optSqlitePath
  where
    optSocketPath =
        File
            <$> strOption
                ( long "socket"
                    <> metavar "PATH"
                    <> help "Path to the cardano-node socket"
                )

    optNetworkId = optMainNet <|> optTestNet
    optMainNet = Mainnet <$ flag' () (long "mainnet")
    optTestNet =
        Testnet . NetworkMagic
            <$> option
                auto
                ( long "testnet"
                    <> metavar "MAGIC"
                    <> help "Network magic"
                )
    optScriptYaml =
        strOption
            ( long "script-yaml"
                <> metavar "PATH"
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
    optSqlitePath =
        strOption
            ( long "sqlite-path"
                <> metavar "SQLITE_PATH"
                <> help "Path to sqlite database"
                <> value "psr.db"
            )

psrOpts :: ParserInfo Options
psrOpts =
    info
        (parseOptions <**> helper)
        ( fullDesc
            <> header "plutus-script-reexecutor"
        )
