--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (
    ChainPoint (..),
 )
import Cardano.Api qualified as C
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Yaml (decodeFileThrow)
import Options
import Options.Applicative
import PSR.ConfigMap qualified as CM
import PSR.Streaming
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream
import System.Exit (exitFailure)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- TODO: Move this into another module.
-- TODO: Make this logic more efficient.
-- NOTE: The idea is to check if the script policy is triggered by either due to
-- Minting or Spending.
shouldTrackTransaction ::
    C.LocalNodeConnectInfo ->
    Map.Map C.PolicyId CM.ScriptDetails ->
    ChainPoint ->
    Transaction ->
    IO Bool
shouldTrackTransaction conn confPolicySet cp (Transaction _ tx) =
    if isUsedInMintingMode
        then pure True
        else isUsedInSpendingMode
  where
    hasIntersectionWithConf = not . Set.null . Set.intersection (Map.keysSet confPolicySet)

    isUsedInMintingMode =
        hasIntersectionWithConf $ getPolicySet $ getMintedValue tx

    isUsedInSpendingMode :: IO Bool
    isUsedInSpendingMode = do
        umap <- queryInputUtxoMap conn cp tx
        let valueSetList = getPolicySet . getTxOutValue <$> Map.elems umap
            combinedPolicySet = Set.unions valueSetList
        let foundPolicies = Map.restrictKeys confPolicySet combinedPolicySet
        pure $ not $ Map.null foundPolicies

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    CM.ConfigMap{..} <- decodeFileThrow scriptYaml
    -- TODO: Remove
    -- TODO: Is there really no better way to parse a hash???
    hash <- case C.deserialiseFromJSON "\"082efb838a6a38c435bf5a1c823569ad224fab42034d265b95f88665ee27877f\"" of
        Left err -> do
            print err
            exitFailure
        Right h -> pure h
    let points = maybe [C.ChainPoint 173166134 hash] pure start -- [ChainPointAtGenesis]
    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."

    let confPolicySet = Map.fromList [(CM.script_hash x, x) | x <- scripts]
        conn = mkLocalNodeConnectInfo networkId socketPath
    streamChainSyncEvents conn points -- Stream m ChainSyncEvent
    -- TODO: Try to replace "concatMap" with "unfoldEach".
        & Stream.concatMap
            (Stream.fromList . (\(a, b) -> (a,) <$> b) . getEventTransactions)
        & Stream.filterM (uncurry (shouldTrackTransaction conn confPolicySet))
        & Stream.fold (Fold.drainMapM print)
