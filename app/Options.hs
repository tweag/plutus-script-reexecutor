module Options where

import Cardano.Api
import Options.Applicative

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

data Options = Options
    { socketPath :: SocketPath
    , networkId :: NetworkId
    }
    deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions =
    Options
        <$> optSocketPath
        <*> optNetworkId
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

psrOpts :: ParserInfo Options
psrOpts =
    info
        (parseOptions <**> helper)
        ( fullDesc
            <> header "plutus-script-reexecutor"
        )
