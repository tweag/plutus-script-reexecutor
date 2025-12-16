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
import Cardano.Ledger.Alonzo.PParams (ppCostModelsL)
import Cardano.Ledger.Api.Scripts qualified as S
import Cardano.Ledger.Plutus qualified as L
import Control.Monad (guard)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro
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

-- Context0 is immediately derivable from the transactions.
data Context0 era where
    Context0 ::
        { ctxPrevChainPoint :: C.ChainPoint
        , ctxShelleyBasedEra :: C.ShelleyBasedEra era
        , ctxTransactions :: [C.Tx era]
        } ->
        Context0 era

deriving instance Show (Context0 era)

eraFromContext :: Context0 era -> (C.ShelleyBasedEra era -> r) -> r
eraFromContext (Context0{ctxShelleyBasedEra}) f = f ctxShelleyBasedEra

-- NOTE: To build contexts other than Context0, we will need to query the node.

data Context1 era where
    Context1 ::
        { context0 :: Context0 era
        , ctxInputUtxoMap :: Map C.TxIn (C.TxOut C.CtxUTxO era)
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
        , -- TODO: This should probably be state?
          ctxCostModels :: S.CostModels
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

mkContext1 :: C.LocalNodeConnectInfo -> Context0 era -> IO (Context1 era)
mkContext1 conn c0@Context0{..} = do
    umap <- queryInputUtxoMap conn ctxPrevChainPoint ctxShelleyBasedEra ctxTransactions
    pure $
        Context1
            { context0 = c0
            , ctxInputUtxoMap = umap
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

costModelsForEra :: C.LocalNodeConnectInfo -> C.ChainPoint -> C.ShelleyBasedEra era -> C.ExceptT String IO S.CostModels
costModelsForEra cmLocalNodeConn cp era = do
    let q = C.queryProtocolParameters era
    r <-
        C.liftIO $
            C.executeLocalStateQueryExpr
                cmLocalNodeConn
                (C.SpecificPoint cp)
                q
    case r of
        Left err -> C.throwError $ show err
        Right (Left err) -> C.throwError $ show err
        Right (Right (Left err)) -> C.throwError $ show err
        Right (Right (Right res)) -> case era of
            C.ShelleyBasedEraAlonzo -> pure $ res ^. ppCostModelsL
            C.ShelleyBasedEraConway -> pure $ res ^. ppCostModelsL
            _ -> C.throwError $ "Unsupported era? " ++ show era

mkContext3 :: ConfigMap -> Context2 era -> IO (Maybe (Context3 era))
mkContext3 ConfigMap{..} ctx2@Context2{..} = do
    res <- C.runExceptT $ do
        let
            ctx0 = context0 context1
            costModelForEra :: C.ExceptT String IO S.CostModels
            costModelForEra =
                eraFromContext
                    ctx0
                    (costModelsForEra cmLocalNodeConn (ctxPrevChainPoint ctx0))

        costs <- costModelForEra
        let langs :: Map C.ScriptHash PlutusLedgerLanguage
            langs = Map.mapMaybe (fmap (sepLanguage . fst) . rsScriptForEvaluation) ctxRelevantScripts
        res <- traverse (makeEvaluationContext costs) langs
        C.liftIO $ print (Map.keys res)
        pure (res, costs)

    case res of
        Left err -> Nothing <$ putStrLn err
        Right (evalContexts, costs) -> pure $ Just $ Context3 ctx2 costs evalContexts
