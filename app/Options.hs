module Options where

import Cardano.Api
import Options.Applicative

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

data Options = Options
    { socketPath :: SocketPath
    , networkId :: NetworkId
    , scriptYaml :: FilePath
    }
    deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions =
    Options
        <$> optSocketPath
        <*> optNetworkId
        <*> optScriptYaml
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

psrOpts :: ParserInfo Options
psrOpts =
    info
        (parseOptions <**> helper)
        ( fullDesc
            <> header "plutus-script-reexecutor"
        )
