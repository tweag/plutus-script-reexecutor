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
    getSpendProjectedUtxoMap,
    selectScriptTriggeredTxs,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo qualified as Alonzo
import Control.Exception (evaluate)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (foldl', toList)
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
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
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Scanl (Scanl)
import Streamly.Data.Scanl qualified as Scanl
import Streamly.Data.Stream qualified as Stream

--------------------------------------------------------------------------------
-- Metrics
--------------------------------------------------------------------------------

data ContextBuilderMetrics = ContextBuilderMetrics
    { mkBlockContext_query :: Summary
    , mkTransactionContext_runtime :: Summary
    }

initialiseContextBuilderMetrics :: IO ContextBuilderMetrics
initialiseContextBuilderMetrics = do
    mkBlockContext_query <- regSummary "mkBlockContext_query" ""
    mkTransactionContext_runtime <- regSummary "mkTransactionContext_runtime" ""
    pure ContextBuilderMetrics{..}

--------------------------------------------------------------------------------
-- Block Context
--------------------------------------------------------------------------------

proveAlonzoEraOnwards :: C.ShelleyBasedEra era -> Maybe (C.AlonzoEraOnwards era)
proveAlonzoEraOnwards = \case
    C.ShelleyBasedEraAlonzo -> Just C.AlonzoEraOnwardsAlonzo
    C.ShelleyBasedEraBabbage -> Just C.AlonzoEraOnwardsBabbage
    C.ShelleyBasedEraConway -> Just C.AlonzoEraOnwardsConway
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

-- NOTE: This is a costly function, but we only run it once.
getSpendProjectedUtxoMap ::
    C.LocalNodeConnectInfo ->
    C.ChainPoint ->
    C.ShelleyBasedEra era ->
    Set C.ScriptHash ->
    IO (Map C.TxIn C.ScriptHash)
getSpendProjectedUtxoMap conn cp sbe confHashes = do
    C.UTxO umap <- runLocalStateQueryExpr conn cp (utxoWholeQuery sbe)
    pure $ Map.filter (flip Set.member confHashes) $ Map.mapMaybe getTxOutScriptAddr umap

--------------------------------------------------------------------------------
-- Transaction Context
--------------------------------------------------------------------------------

getOutputUtxoMap :: C.Tx era -> Map C.TxIn (C.TxOut C.CtxTx era)
getOutputUtxoMap tx =
    Map.fromList $ zipWith mkEntry [0 ..] outs
  where
    body = C.getTxBody tx
    tid = C.getTxId body
    outs = C.txOuts (C.getTxBodyContent body)
    mkEntry ix out = (C.TxIn tid (C.TxIx ix), out)

buildUtxoMap ::
    -- | Script hashes provided in the configuration.
    Set C.ScriptHash ->
    {- | Minimal state where all the values are contained in the set of script
    hashes provided.
    -}
    Map C.TxIn C.ScriptHash ->
    -- | Input transaction
    C.Tx era ->
    -- | The new minimal state and the script hashes ejected from the map
    (Map C.TxIn C.ScriptHash, Set C.ScriptHash)
buildUtxoMap confHashes utxoMap tx =
    let toEject = Map.restrictKeys utxoMap $ getTxInSet tx
        toAdd =
            Map.filter (flip Set.member confHashes) $
                Map.mapMaybe getTxOutScriptAddr (getOutputUtxoMap tx)
        newUtxoMap = Map.union (Map.difference utxoMap toEject) toAdd
        ejectedElements = Set.fromList $ Map.elems toEject
     in (newUtxoMap, ejectedElements)

buildUtxoMapScan ::
    (Monad m) =>
    Set C.ScriptHash ->
    Map C.TxIn C.ScriptHash ->
    Scanl m (C.Tx era) (Map C.TxIn C.ScriptHash, (C.Tx era, Set C.ScriptHash))
buildUtxoMapScan confHashes initialUtxoMap = Scanl.mkScanl step initial
  where
    initial = (initialUtxoMap, error "buildUtxoMapScan: Use with postscan")
    step (utxoMap, _) tx = (tx,) <$> buildUtxoMap confHashes utxoMap tx

transactionScan ::
    (Monad m) =>
    -- | Script hashes provided in the configuration.
    Set C.ScriptHash ->
    -- | Previous minimal UTxO state
    Map C.TxIn C.ScriptHash ->
    Scanl
        m
        (C.Tx era)
        ( Map C.TxIn C.ScriptHash -- UTxO state after tx is applied
        , ( -- Transaction that is applied
            C.Tx era
          , -- Script hashes in the transaction that meet the criteria
            Set C.ScriptHash
          )
        )
transactionScan confHashes initialUtxoMap =
    second withAllIntersectingPolicies
        <$> buildUtxoMapScan confHashes initialUtxoMap
  where
    getNonSpendIntersectingPolicies tx =
        Set.intersection confHashes $
            Set.unions
                [ getMintPolicies tx
                , getCertifyingScriptHashes tx
                , getRewardingScriptHashes tx
                ]
    withAllIntersectingPolicies (tx, spendIntersectingPolicies) =
        let nonSpendIntersectingPolicies =
                getNonSpendIntersectingPolicies tx
            allIntersectingPolicies =
                Set.union nonSpendIntersectingPolicies spendIntersectingPolicies
         in (tx, allIntersectingPolicies)

-- QUESTION: Does it make sense to specialize the folds to Identity?
selectScriptTriggeredTxs ::
    Set C.ScriptHash ->
    Map C.TxIn C.ScriptHash ->
    [C.Tx era] ->
    (Map C.TxIn C.ScriptHash, [C.Tx era])
selectScriptTriggeredTxs confHashes initialUtxoMap txList =
    Stream.fromList txList
        & Stream.postscanl (transactionScan confHashes initialUtxoMap)
        & fmap (second maybeIntersection)
        & Stream.fold
            ( Fold.tee
                (maybe initialUtxoMap fst <$> Fold.latest)
                (Fold.lmap snd (Fold.catMaybes Fold.toList))
            )
        & runIdentity
  where
    maybeIntersection (tx, intersection) =
        if Set.null intersection then Nothing else Just tx

data TransactionContext era where
    TransactionContext ::
        { ctxBlockHeader :: C.BlockHeader
        , ctxTransaction :: C.Tx era
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

getCertifyingScriptHashes :: C.Tx era -> Set C.ScriptHash
getCertifyingScriptHashes tx =
    case C.txCertificates (C.getTxBodyContent (C.getTxBody tx)) of
        C.TxCertificatesNone -> Set.empty
        C.TxCertificates _ certMap ->
            Set.fromAscList . mapMaybe unwrapAndExtract . map fst $
                OMap.toAscList certMap
  where
    unwrapAndExtract :: C.Certificate era -> Maybe C.ScriptHash
    unwrapAndExtract cert =
        case cert of
            C.ShelleyRelatedCertificate C.ShelleyToBabbageEraShelley txCert ->
                C.fromShelleyScriptHash <$> L.getScriptWitnessTxCert txCert
            C.ShelleyRelatedCertificate C.ShelleyToBabbageEraAllegra txCert ->
                C.fromShelleyScriptHash <$> L.getScriptWitnessTxCert txCert
            C.ShelleyRelatedCertificate C.ShelleyToBabbageEraMary txCert ->
                C.fromShelleyScriptHash <$> L.getScriptWitnessTxCert txCert
            C.ShelleyRelatedCertificate C.ShelleyToBabbageEraAlonzo txCert ->
                C.fromShelleyScriptHash <$> L.getScriptWitnessTxCert txCert
            C.ShelleyRelatedCertificate C.ShelleyToBabbageEraBabbage txCert ->
                C.fromShelleyScriptHash <$> L.getScriptWitnessTxCert txCert
            C.ConwayCertificate C.ConwayEraOnwardsConway txCert ->
                C.fromShelleyScriptHash <$> L.getScriptWitnessTxCert txCert

getRewardingScriptHashes :: C.Tx era -> Set C.ScriptHash
getRewardingScriptHashes tx =
    case C.txWithdrawals (C.getTxBodyContent (C.getTxBody tx)) of
        C.TxWithdrawalsNone -> Set.empty
        C.TxWithdrawals _ withdrawals ->
            Set.fromList $ mapMaybe extractHashFromTuple withdrawals
  where
    extractHashFromTuple :: (C.StakeAddress, L.Coin, w) -> Maybe C.ScriptHash
    extractHashFromTuple (stakeAddr, _, _) =
        case stakeAddr of
            C.StakeAddress _ cred ->
                case cred of
                    L.ScriptHashObj hash -> Just $ C.fromShelleyScriptHash hash
                    _ -> Nothing

-- Exists to force evaluation to happen here, instead of leaving it unevaluated
-- in the TransactionContext
mkTransactionContext ::
    ContextBuilderMetrics ->
    ConfigMap ->
    BlockContext era ->
    C.Tx era ->
    IO (TransactionContext era)
mkTransactionContext metrics cm bc tx =
    observeDuration metrics.mkTransactionContext_runtime $ do
        let res = mkTransactionContext' cm bc tx
        -- Ensure the result is actually forced so the metrics are accurate
        !_ <- evaluate (forceExecutionResults res)
        pure res
  where
    -- Forces the [Either _ _] so all elements are in WHNF, which should be enough
    -- to ensure the work of evaluating the script is no longer a thunk.
    forceExecutionResults :: TransactionContext era -> ()
    forceExecutionResults = foldl' (flip seq) () . toList . ctxTransactionExecutionResult

mkTransactionContext' ::
    ConfigMap -> BlockContext era -> C.Tx era -> TransactionContext era
mkTransactionContext' cm bc tx = do
    let eres = evaluateTransaction bc tx (cmScripts cm)
     in TransactionContext bc.ctxBlockHeader tx eres

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

-- TODO: ResolvedScript is something that never changes and can be pretty
-- large. It may be worth putting "Map.Map C.ScriptHash [ResolvedScript]" in a
-- compact region.
evaluateTransaction ::
    forall era.
    BlockContext era ->
    C.Tx era ->
    Map.Map C.ScriptHash [ResolvedScript] ->
    TransactionExecutionResult
evaluateTransaction BlockContext{..} (C.ShelleyTx era tx) scriptMap = do
    case ctxAlonzoEraOnwards of
        C.AlonzoEraOnwardsAlonzo -> runEvaluation
        C.AlonzoEraOnwardsBabbage -> runEvaluation
        C.AlonzoEraOnwardsConway -> runEvaluation
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

    -- TODO: Report this error to the user.
    mkLedgerScript ResolvedScript{..} = do
        scr <- C.toScriptInEra (C.convert ctxAlonzoEraOnwards) rsScriptFileContent
        pure $ (rsName, C.toShelleyScript scr)
    subMap = Map.map (mapMaybe mkLedgerScript) scriptMap
