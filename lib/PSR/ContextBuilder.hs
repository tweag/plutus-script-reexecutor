{-# LANGUAGE RankNTypes #-}

module PSR.ContextBuilder (
    Context0 (..),
    Context1 (..),
    Context2 (..),
    Context3 (..),
    mkContext0,
    getMintPolicies,
    mkContext1,
    getInputScriptAddrs,
    mkContext2,
    mkContext3,
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

data Context0 era where
    Context0 ::
        { ctxPrevChainPoint :: C.ChainPoint
        , ctxShelleyBasedEra :: C.ShelleyBasedEra era
        , ctxTransactions :: [C.Tx era]
        } ->
        Context0 era

deriving instance Show (Context0 era)

-- Context1 is essentially acts like the global environment.
data Context1 era where
    Context1 ::
        { context0 :: Context0 era
        , ctxInputUtxoMap :: Map C.TxIn (C.TxOut C.CtxUTxO era)
        , -- NOTE: The protocol parameters (and hence the cost models) may change
          -- in a running node. It may be okay to poll this at the block boundary.
          ctxCostModels :: S.CostModels
        } ->
        Context1 era

deriving instance Show (Context1 era)

--------------------------------------------------------------------------------
-- Transaction Context
--------------------------------------------------------------------------------

data Context2 era where
    Context2 ::
        { context1 :: Context1 era
        , ctxTransaction :: C.Tx era
        , ctxRelevantScripts :: Map.Map C.ScriptHash ResolvedScript
        } ->
        Context2 era

data Context3 era where
    Context3 ::
        { context2 :: Context2 era
        , ctxEvaluationContext :: Map C.ScriptHash EvaluationContext
        } ->
        Context3 era

-- NOTE: The final context should have everything required to run the
-- transaction locally.

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mkContext0 ::
    C.ChainPoint -> C.ShelleyBasedEra era -> [C.Tx era] -> Context0 era
mkContext0 cp era txs =
    Context0
        { ctxPrevChainPoint = cp
        , ctxShelleyBasedEra = era
        , ctxTransactions = txs
        }

-- NOTE: We should add more capabilities to our default monad.
mkContext1 :: C.LocalNodeConnectInfo -> Context0 era -> IO (Context1 era)
mkContext1 conn c0@Context0{..} = do
    let query =
            (,)
                <$> utxoMapQuery ctxShelleyBasedEra ctxTransactions
                <*> costModelsQuery ctxShelleyBasedEra
    -- NOTE: We can catch CostModelsQueryException and choose to retry or skip.
    (umap, costs) <- runLocalStateQueryExpr conn ctxPrevChainPoint query
    pure $
        Context1
            { context0 = c0
            , ctxInputUtxoMap = C.unUTxO umap
            , ctxCostModels = costs
            }

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

mkContext2 ::
    ConfigMap ->
    Context1 era ->
    C.Tx era ->
    Maybe (Context2 era)
mkContext2 ConfigMap{..} ctx1@Context1{..} tx = do
    let interestingScripts =
            Map.restrictKeys cmScripts $
                Set.union (getMintPolicies tx) (getInputScriptAddrs ctxInputUtxoMap tx)
    guard (not $ Map.null interestingScripts)
    pure $ Context2 ctx1 tx interestingScripts

makeEvaluationContext :: S.CostModels -> PlutusLedgerLanguage -> C.ExceptT String IO EvaluationContext
makeEvaluationContext params lang = case lang of
    PlutusV1 -> run L.PlutusV1 V1.mkEvaluationContext
    PlutusV2 -> run L.PlutusV2 V2.mkEvaluationContext
    PlutusV3 -> run L.PlutusV3 V3.mkEvaluationContext
  where
    run lng f = case Map.lookup lng (L.costModelsValid params) of
        Just costs -> C.modifyError show . fmap fst . runWriterT $ f (L.getCostModelParams costs)
        Nothing -> C.throwError $ "Unknown cost model for lang: " ++ show lang

mkContext3 :: Context2 era -> IO (Maybe (Context3 era))
mkContext3 ctx2 = do
    let langs :: Map C.ScriptHash PlutusLedgerLanguage
        langs = Map.mapMaybe (fmap (sepLanguage . fst) . rsScriptForEvaluation) ctxRelevantScripts
    res <- C.runExceptT $ traverse (makeEvaluationContext ctxCostModels) langs
    case res of
        Left err -> Nothing <$ putStrLn err
        Right evalContexts -> do
            C.liftIO $ print (Map.keys evalContexts)
            pure $ Just $ Context3 ctx2 evalContexts
  where
    Context2{..} = ctx2
    Context1{..} = context1
