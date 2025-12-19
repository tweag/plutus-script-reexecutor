{-# LANGUAGE RankNTypes #-}

module PSR.ContextBuilder (
    BlockContext (..),
    TransactionContext (..),
    mkBlockContext,
    mkTransactionContext,
    evaluateTransaction,
    proveAlonzoEraOnwards,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Alonzo qualified as Alonzo
import Control.Monad (guard)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import PSR.Chain
import PSR.ConfigMap (ConfigMap (..), ResolvedScript (..))
import PSR.Evaluation.Api (evaluateTransactionExecutionUnitsShelley)
import PSR.Types (pCompact)

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
        { ctxPrevChainPoint :: C.ChainPoint
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

-- deriving instance Show (BlockContext era)

mkBlockContext ::
    C.LocalNodeConnectInfo ->
    C.ChainPoint ->
    C.AlonzoEraOnwards era ->
    [C.Tx era] ->
    IO (BlockContext era)
mkBlockContext conn prevCp era txs = do
    let sbe = C.convert era
        query =
            BlockContext prevCp era txs
                <$> utxoMapQuery sbe txs
                <*> pParamsQuery sbe
                <*> eraHistoryQuery
                <*> sysStartQuery
    -- NOTE: We can catch CostModelsQueryException and choose to retry or skip.
    runLocalStateQueryExpr conn prevCp query

--------------------------------------------------------------------------------
-- Transaction Context
--------------------------------------------------------------------------------

data TransactionContext era where
    TransactionContext ::
        { ctxTransaction :: C.Tx era
        , ctxRelevantScripts :: Map.Map C.ScriptHash ResolvedScript
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

getNonEmptyIntersection ::
    ConfigMap ->
    BlockContext era ->
    C.Tx era ->
    Maybe (Map.Map C.ScriptHash ResolvedScript)
getNonEmptyIntersection ConfigMap{..} BlockContext{..} tx = do
    let inpUtxoMap = C.unUTxO ctxInputUtxoMap
        interestingScripts =
            Map.restrictKeys cmScripts $
                Set.union (getMintPolicies tx) (getInputScriptAddrs inpUtxoMap tx)
    guard (not $ Map.null interestingScripts)
    pure interestingScripts

mkTransactionContext ::
    ConfigMap -> BlockContext era -> C.Tx era -> IO (Maybe (TransactionContext era))
mkTransactionContext cm bc tx = do
    let res = evaluateTransaction cm bc tx
    pCompact res
    case getNonEmptyIntersection cm bc tx of
        Nothing -> pure Nothing
        Just nei -> pure $ Just $ TransactionContext tx nei

evaluateTransaction ::
    forall era.
    ConfigMap ->
    BlockContext era ->
    C.Tx era ->
    Map
        C.ScriptWitnessIndex
        (Either C.ScriptExecutionError ([Text], C.ExecutionUnits))
evaluateTransaction ConfigMap{..} BlockContext{..} (C.ShelleyTx era tx) = do
    case ctxAlonzoEraOnwards of
        C.AlonzoEraOnwardsAlonzo -> runEvaluation
        C.AlonzoEraOnwardsBabbage -> runEvaluation
        C.AlonzoEraOnwardsConway -> runEvaluation
        C.AlonzoEraOnwardsDijkstra -> runEvaluation
  where
    runEvaluation ::
        (L.Script (C.ShelleyLedgerEra era) ~ Alonzo.AlonzoScript (C.ShelleyLedgerEra era)) =>
        Map
            C.ScriptWitnessIndex
            (Either C.ScriptExecutionError ([Text], C.ExecutionUnits))
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
    subMap = Map.mapMaybe mkLedgerScript cmScripts
