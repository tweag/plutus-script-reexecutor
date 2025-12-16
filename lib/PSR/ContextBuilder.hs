{-# LANGUAGE RankNTypes #-}

module PSR.ContextBuilder (
    BlockContext (..),
    TransactionContext (..),
    mkBlockContext,
    mkTransactionContext,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Ledger.Api.Scripts qualified as S
import Cardano.Ledger.Plutus qualified as L
import Control.Monad (guard)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import PSR.Chain
import PSR.ConfigMap (ConfigMap (..), ResolvedScript (..), ScriptEvaluationParameters (..))
import PlutusLedgerApi.Common
import PlutusLedgerApi.V1.EvaluationContext qualified as V1
import PlutusLedgerApi.V2.EvaluationContext qualified as V2
import PlutusLedgerApi.V3.EvaluationContext qualified as V3

--------------------------------------------------------------------------------
-- Block Context
--------------------------------------------------------------------------------

-- NOTE: Our decisions are made based on the context built. At different stages
-- of the pipeline the context we are looking at keeps increasing and our
-- decisions are based on that.

-- Q: Is there a better way to represent this?

-- Context1 is essentially acts like the global environment.
data BlockContext era where
    BlockContext ::
        { ctxPrevChainPoint :: C.ChainPoint
        , ctxShelleyBasedEra :: C.ShelleyBasedEra era
        , ctxTransactions :: [C.Tx era]
        , ctxInputUtxoMap :: Map C.TxIn (C.TxOut C.CtxUTxO era)
        , -- NOTE: The protocol parameters (and hence the cost models) may change
          -- in a running node. It may be okay to poll this at the block boundary.
          ctxCostModels :: S.CostModels
        } ->
        BlockContext era

deriving instance Show (BlockContext era)

mkBlockContext ::
    C.LocalNodeConnectInfo ->
    C.ChainPoint ->
    C.ShelleyBasedEra era ->
    [C.Tx era] ->
    IO (BlockContext era)
mkBlockContext conn prevCp era txs = do
    let query =
            (,)
                <$> utxoMapQuery era txs
                <*> costModelsQuery era
    -- NOTE: We can catch CostModelsQueryException and choose to retry or skip.
    (umap, costs) <- runLocalStateQueryExpr conn prevCp query
    pure $
        BlockContext
            { ctxPrevChainPoint = prevCp
            , ctxShelleyBasedEra = era
            , ctxTransactions = txs
            , ctxInputUtxoMap = C.unUTxO umap
            , ctxCostModels = costs
            }

--------------------------------------------------------------------------------
-- Transaction Context
--------------------------------------------------------------------------------

data TransactionContext era where
    TransactionContext ::
        { ctxTransaction :: C.Tx era
        , ctxRelevantScripts :: Map.Map C.ScriptHash ResolvedScript
        , ctxEvaluationContext :: Map C.ScriptHash EvaluationContext
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
    let interestingScripts =
            Map.restrictKeys cmScripts $
                Set.union (getMintPolicies tx) (getInputScriptAddrs ctxInputUtxoMap tx)
    guard (not $ Map.null interestingScripts)
    pure $ interestingScripts

makeEvaluationContext ::
    S.CostModels ->
    PlutusLedgerLanguage ->
    C.ExceptT String IO EvaluationContext
makeEvaluationContext params lang = case lang of
    PlutusV1 -> run L.PlutusV1 V1.mkEvaluationContext
    PlutusV2 -> run L.PlutusV2 V2.mkEvaluationContext
    PlutusV3 -> run L.PlutusV3 V3.mkEvaluationContext
  where
    run lng f = case Map.lookup lng (L.costModelsValid params) of
        Just costs -> C.modifyError show . fmap fst . runWriterT $ f (L.getCostModelParams costs)
        Nothing -> C.throwError $ "Unknown cost model for lang: " ++ show lang

getScriptEvaluationContext ::
    BlockContext era ->
    Map C.ScriptHash ResolvedScript ->
    IO (Maybe (Map C.ScriptHash EvaluationContext))
getScriptEvaluationContext BlockContext{..} relevantScripts = do
    let langs :: Map C.ScriptHash PlutusLedgerLanguage
        langs = Map.mapMaybe (fmap (sepLanguage . fst) . rsScriptForEvaluation) relevantScripts
    res <- C.runExceptT $ traverse (makeEvaluationContext ctxCostModels) langs
    case res of
        Left err -> Nothing <$ putStrLn err
        Right evalContexts -> do
            C.liftIO $ print (Map.keys evalContexts)
            pure $ Just $ evalContexts

mkTransactionContext ::
    ConfigMap -> BlockContext era -> C.Tx era -> IO (Maybe (TransactionContext era))
mkTransactionContext cm bc tx = do
    case getNonEmptyIntersection cm bc tx of
        Nothing -> pure Nothing
        Just nei -> do
            msec <- getScriptEvaluationContext bc nei
            pure $ case msec of
                Nothing -> Nothing
                Just sec -> Just $ TransactionContext tx nei sec
