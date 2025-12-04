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
import Streamly.Data.Scanl.Prelude qualified as Scanl
import Streamly.Data.Stream.Prelude qualified as Stream
import System.Exit (exitFailure)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

data IntersectionResult
    = IrMinting (Map.Map C.PolicyId CM.ScriptDetails)
    | IrSpendingOnly (Map.Map C.PolicyId CM.ScriptDetails)
    deriving (Show)

-- Q: What information is required to re-execute the transaction locally?
-- NOTE: It's a good idea to carry all the information query as it may be used
--       later in the pipeline when constructing the script context. Ie. carry
--       the minting value and the input UTxOs of the transactions that we
--       queried.
-- TODO: Move this into another module.
-- TODO: Make this logic more efficient.
-- NOTE: The idea is to check if the script policy is triggered by either due to
--       Minting or Spending.
getIntersectingPolicies ::
    C.LocalNodeConnectInfo ->
    Map.Map C.PolicyId CM.ScriptDetails ->
    ChainPoint ->
    Transaction ->
    IO (Maybe IntersectionResult)
getIntersectingPolicies conn confPolicySet cp (Transaction _ tx) =
    if Map.null intersectionInMintingMode
        then do
            res <- intersectionInSpendingMode
            pure $ if Map.null res then Nothing else Just $ IrSpendingOnly res
        else pure $ Just $ IrMinting intersectionInMintingMode
  where
    intersectionInMintingMode =
        Map.restrictKeys confPolicySet $ getPolicySet $ getMintedValue tx

    intersectionInSpendingMode = do
        umap <- queryInputUtxoMap conn cp tx
        let valueSetList = getPolicySet . getTxOutValue <$> Map.elems umap
            combinedPolicySet = Set.unions valueSetList
        pure $ Map.restrictKeys confPolicySet combinedPolicySet

-- Pairs the current set of transactions with the previous chainpoint
trackPreviousChainPoint ::
    (Monad m) =>
    Scanl.Scanl m (ChainPoint, [Transaction]) (ChainPoint, [Transaction])
trackPreviousChainPoint =
    snd <$> Scanl.mkScanl step initial
  where
    step (prev, _) (new, txs) = (new, (prev, txs))
    initial =
        ( error "trackPreviousChainPoint: Use postscanl"
        , error "trackPreviousChainPoint: Use postscanl"
        )

isByron :: ChainSyncEvent -> Bool
isByron (RollForward (C.BlockInMode C.ByronEra _) _) = True
isByron _ = False

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

    let confPolicyMap = Map.fromList [(CM.script_hash x, x) | x <- scripts]
        conn = mkLocalNodeConnectInfo networkId socketPath
    streamChainSyncEvents conn points
        & Stream.filter (not . isByron)
        & fmap getEventTransactions
        & Stream.postscanl trackPreviousChainPoint
        -- TODO: Try to replace "concatMap" with "unfoldEach".
        & Stream.concatMap (Stream.fromList . (\(a, b) -> (a,) <$> b))
        & Stream.mapM
            ( \a ->
                fmap (a,)
                    <$> uncurry (getIntersectingPolicies conn confPolicyMap) a
            )
        & Stream.catMaybes
        & Stream.fold (Fold.drainMapM print)
