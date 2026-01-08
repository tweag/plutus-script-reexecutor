{-# LANGUAGE RankNTypes #-}

module PSR.ContextBuilder (
    BlockContext (..),
    TransactionContext (..),
    mkBlockContext,
    mkTransactionContext,
    evaluateTransaction,
    proveAlonzoEraOnwards,
    ContextBuilderMetrics,
    initialiseContextBuilderMetrics,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C hiding (Certificate)
import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Alonzo qualified as Alonzo
import Control.Exception (evaluate)
import Control.Monad (guard)
import Data.Foldable (foldl', toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import PSR.Chain
import PSR.ConfigMap (ConfigMap (..), ResolvedScript (..))
import PSR.Evaluation.Api (evaluateTransactionExecutionUnitsShelley)
import PSR.Metrics (Summary, observeDuration, regSummary)
import PSR.Types

-- NOTE: We should use a more stable module.
import Cardano.Api.Experimental.Certificate qualified as C

--------------------------------------------------------------------------------
-- Block Context
--------------------------------------------------------------------------------

proveAlonzoEraOnwards :: C.ShelleyBasedEra era -> Maybe (C.AlonzoEraOnwards era)
proveAlonzoEraOnwards = \case
    C.ShelleyBasedEraAlonzo -> Just C.AlonzoEraOnwardsAlonzo
    C.ShelleyBasedEraBabbage -> Just C.AlonzoEraOnwardsBabbage
    C.ShelleyBasedEraConway -> Just C.AlonzoEraOnwardsConway
    C.ShelleyBasedEraDijkstra -> Just C.AlonzoEraOnwardsDijkstra
    _ -> Nothing

-- NOTE: Our decisions are made based on the context built. At different stages
-- of the pipeline the context we are looking at keeps increasing and our
-- decisions are based on that.

-- Q: Is there a better way to represent this?

-- Context1 is essentially acts like the global environment.
data BlockContext era where
    BlockContext ::
        { ctxBlockHeader :: C.BlockHeader
        , ctxPrevChainPoint :: C.ChainPoint
        , -- NOTE: Use C.convert to get C.ShelleyBasedEra
          ctxAlonzoEraOnwards :: C.AlonzoEraOnwards era
        , ctxTransactions :: [C.Tx era]
        , ctxInputUtxoMap :: C.UTxO era
        , -- NOTE: The protocol parameters (and hence the cost models) may change
          -- in a running node. It may be okay to poll this at the block boundary.
          ctxPParams :: L.PParams (C.ShelleyLedgerEra era)
        , ctxEraHistory :: C.EraHistory
        , ctxSysStart :: C.SystemStart
        } ->
        BlockContext era

data ContextBuilderMetrics = ContextBuilderMetrics
    { mkBlockContext_query :: Summary
    , mkTransactionContext_runtime :: Summary
    }

initialiseContextBuilderMetrics :: IO ContextBuilderMetrics
initialiseContextBuilderMetrics = do
    mkBlockContext_query <- regSummary "mkBlockContext_query" ""
    mkTransactionContext_runtime <- regSummary "mkTransactionContext_runtime" ""
    pure ContextBuilderMetrics{..}

mkBlockContext ::
    ContextBuilderMetrics ->
    C.BlockHeader ->
    C.LocalNodeConnectInfo ->
    C.ChainPoint ->
    C.AlonzoEraOnwards era ->
    [C.Tx era] ->
    IO (BlockContext era)
mkBlockContext metrics bh conn prevCp era txs = do
    let sbe = C.convert era
        query =
            BlockContext bh prevCp era txs
                <$> utxoMapQuery sbe txs
                <*> pParamsQuery sbe
                <*> eraHistoryQuery
                <*> sysStartQuery
    -- NOTE: We can catch CostModelsQueryException and choose to retry or skip.
    observeDuration metrics.mkBlockContext_query $
        runLocalStateQueryExpr conn prevCp query

--------------------------------------------------------------------------------
-- Transaction Context
--------------------------------------------------------------------------------

data TransactionContext era where
    TransactionContext ::
        { ctxBlockHeader :: C.BlockHeader
        , ctxTransaction :: C.Tx era
        , ctxRelevantScripts :: Map.Map C.ScriptHash ResolvedScript
        , ctxTransactionExecutionResult :: TransactionExecutionResult
        } ->
        TransactionContext era

getMintPolicies :: C.Tx era -> Set C.ScriptHash
getMintPolicies =
    Set.mapMonotonic C.unPolicyId
        . getPolicySet
        . C.txMintValueToValue
        . C.txMintValue
        . C.getTxBodyContent
        . C.getTxBody

getInputScriptAddrs ::
    Map C.TxIn (C.TxOut C.CtxUTxO era) -> C.Tx era -> Set C.ScriptHash
getInputScriptAddrs utxoMap tx =
    let utxoList = Map.elems $ Map.restrictKeys utxoMap $ getTxInSet tx
     in Set.fromList $ mapMaybe getTxOutScriptAddr utxoList

getCertifyingScriptHashes :: C.Tx era -> Set C.ScriptHash
getCertifyingScriptHashes tx =
    case C.txCertificates (C.getTxBodyContent (C.getTxBody tx)) of
        C.TxCertificatesNone -> Set.empty
        C.TxCertificates _ certMap ->
            Set.fromAscList . mapMaybe unwrapAndExtract . map fst $
                OMap.toAscList certMap
  where
    unwrapAndExtract ::
        C.Certificate (C.ShelleyLedgerEra era) -> Maybe C.ScriptHash
    unwrapAndExtract (C.Certificate txCert) =
        C.fromShelleyScriptHash <$> L.getScriptWitnessTxCert txCert

getRewardingScriptHashes :: C.Tx era -> Set C.ScriptHash
getRewardingScriptHashes tx =
    case C.txWithdrawals (C.getTxBodyContent (C.getTxBody tx)) of
        C.TxWithdrawalsNone -> Set.empty
        C.TxWithdrawals _ withdrawals ->
            Set.fromList $ mapMaybe extractHashFromTuple withdrawals
  where
    extractHashFromTuple :: (C.StakeAddress, C.Coin, w) -> Maybe C.ScriptHash
    extractHashFromTuple (stakeAddr, _, _) =
        case stakeAddr of
            C.StakeAddress _ cred ->
                case cred of
                    L.ScriptHashObj hash -> Just $ C.fromShelleyScriptHash hash
                    _ -> Nothing

getNonEmptyIntersection ::
    ConfigMap ->
    BlockContext era ->
    C.Tx era ->
    Maybe (Map.Map C.ScriptHash ResolvedScript)
getNonEmptyIntersection ConfigMap{..} BlockContext{..} tx = do
    let inpUtxoMap = C.unUTxO ctxInputUtxoMap
        interestingScripts =
            Map.restrictKeys cmScripts $
                Set.unions
                    [ getMintPolicies tx
                    , getInputScriptAddrs inpUtxoMap tx
                    , getCertifyingScriptHashes tx
                    , getRewardingScriptHashes tx
                    ]
    guard (not $ Map.null interestingScripts)
    pure interestingScripts

-- Exists to force evaluation to happen here, instead of leaving it unevaluated
-- in the TransactionContext
mkTransactionContext ::
    ContextBuilderMetrics -> ConfigMap -> BlockContext era -> C.Tx era -> IO (Maybe (TransactionContext era))
mkTransactionContext metrics cm bc tx =
    observeDuration metrics.mkTransactionContext_runtime $ do
        let res = mkTransactionContext' cm bc tx
        -- Ensure the result is actually forced so the metrics are accurate
        !_ <- evaluate (maybe () forceExecutionResults res)
        pure res
  where
    -- Forces the [Either _ _] so all elements are in WHNF, which should be enough
    -- to ensure the work of evaluating the script is no longer a thunk.
    forceExecutionResults :: TransactionContext era -> ()
    forceExecutionResults = foldl' (flip seq) () . toList . ctxTransactionExecutionResult

mkTransactionContext' ::
    ConfigMap -> BlockContext era -> C.Tx era -> Maybe (TransactionContext era)
mkTransactionContext' cm bc tx = do
    nei <- getNonEmptyIntersection cm bc tx
    let eres = evaluateTransaction bc tx nei
    pure $ TransactionContext bc.ctxBlockHeader tx nei eres

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

evaluateTransaction ::
    forall era.
    BlockContext era ->
    C.Tx era ->
    Map.Map C.ScriptHash ResolvedScript ->
    TransactionExecutionResult
evaluateTransaction BlockContext{..} (C.ShelleyTx era tx) scriptMap = do
    case ctxAlonzoEraOnwards of
        C.AlonzoEraOnwardsAlonzo -> runEvaluation
        C.AlonzoEraOnwardsBabbage -> runEvaluation
        C.AlonzoEraOnwardsConway -> runEvaluation
        C.AlonzoEraOnwardsDijkstra -> runEvaluation
  where
    runEvaluation ::
        (L.Script (C.ShelleyLedgerEra era) ~ Alonzo.AlonzoScript (C.ShelleyLedgerEra era)) =>
        TransactionExecutionResult
    runEvaluation =
        evaluateTransactionExecutionUnitsShelley
            subMap
            era
            ctxSysStart
            (C.toLedgerEpochInfo ctxEraHistory)
            (C.LedgerProtocolParameters ctxPParams)
            ctxInputUtxoMap
            tx

    mkLedgerScript ResolvedScript{..} =
        case rsScriptFileContent of
            Just scrInAny ->
                case C.toScriptInEra (C.convert ctxAlonzoEraOnwards) scrInAny of
                    Nothing -> Nothing
                    Just scriptInEra -> Just (C.toShelleyScript scriptInEra)
            _ -> Nothing
    subMap = Map.mapMaybe mkLedgerScript scriptMap
