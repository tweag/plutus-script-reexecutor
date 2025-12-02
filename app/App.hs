--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (ChainPoint (..), File (File), NetworkId (..), NetworkMagic (NetworkMagic), SocketPath)
import Data.Function ((&))
import Options.Applicative
import PSR.Streaming (streamChainSyncEvents)
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream

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
        <$> ( File
                <$> strOption
                    ( long "socket"
                        <> metavar "PATH"
                        <> help "Path to the cardano-node socket"
                    )
            )
        <*> ( (Mainnet <$ flag' () (long "mainnet"))
                <|> Testnet
                . NetworkMagic
                <$> option
                    auto
                    ( long "testnet"
                        <> metavar "MAGIC"
                        <> help "Network magic"
                    )
            )

psrOpts :: ParserInfo Options
psrOpts =
    info
        (parseOptions <**> helper)
        ( fullDesc
            <> progDesc "Print all the things"
            <> header "plutus-script-reexecutor"
        )

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    let points = [ChainPointAtGenesis]
    putStrLn "Started..."
    streamChainSyncEvents socketPath networkId points
        & Stream.fold (Fold.drainMapM print)
