--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (
    ChainPoint (..),
    File (File),
    NetworkId (..),
    NetworkMagic (NetworkMagic),
    SocketPath,
 )
import Cardano.Api qualified as C
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Options.Applicative
import PSR.Streaming
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

-- TODO: Move CLI related logic into a separate module

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

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- TODO: Move this into another module.
-- TODO: Make this logic more efficient.
-- NOTE: The idea is to check if the script policy is triggered by either due to
-- Minting or Spending.
shouldTrackTransaction ::
    C.LocalNodeConnectInfo ->
    Set C.PolicyId ->
    ChainPoint ->
    Transaction ->
    IO Bool
shouldTrackTransaction conn confPolicySet cp (Transaction _ tx) =
    if isUsedInMintingMode
        then pure True
        else isUsedInSpendingMode
  where
    hasIntersectionWithConf = not . Set.null . Set.intersection confPolicySet

    isUsedInMintingMode =
        hasIntersectionWithConf $ getPolicySet $ getMintedValue tx

    isUsedInSpendingMode :: IO Bool
    isUsedInSpendingMode = do
        umap <- queryInputUtxoMap conn cp tx
        let valueSetList = getPolicySet . getTxOutValue <$> Map.elems umap
            combinedPolicySet = Set.unions valueSetList
        pure $ hasIntersectionWithConf combinedPolicySet

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    let points = [ChainPointAtGenesis]
    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."
    let configPolicies = [] :: [C.PolicyId]
        confPolicySet = Set.fromList configPolicies
        conn = mkLocalNodeConnectInfo networkId socketPath
    streamChainSyncEvents conn points -- Stream m ChainSyncEvent
    -- TODO: Try to replace "concatMap" with "unfoldEach".
        & Stream.concatMap
            (Stream.fromList . (\(a, b) -> (a,) <$> b) . getEventTransactions)
        & Stream.filterM (uncurry (shouldTrackTransaction conn confPolicySet))
        & Stream.fold (Fold.drainMapM print)
