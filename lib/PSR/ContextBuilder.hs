{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module PSR.ContextBuilder (
    Context0 (..),
    Context1 (..),
    mkContext0,
    getMintPolicies,
    mkContext1,
    getSpendPolicies,
    TxContext (..),
    writeContext,
    getContext,
    writeContextEra,
    getContextEra,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum ((==>))
import Data.Functor.Identity (Identity (..))
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Type.Equality (TestEquality (testEquality), type (:~:) (Refl))
import PSR.Chain
import PSR.Streaming

data EraCtxKey era a where
    EraTransaction :: EraCtxKey era (C.Tx era)
    EraMintValue :: EraCtxKey era (C.TxMintValue C.ViewTx era)
    EraInputUtxoMap :: EraCtxKey era (Map C.TxIn (C.TxOut C.CtxUTxO era))

deriveGEq ''EraCtxKey
deriveGCompare ''EraCtxKey
deriveGShow ''EraCtxKey

-- deriveArgDict ''EraCtxKey

type EraContext era = DMap (EraCtxKey era) Identity

data CtxKey a where
    CtxChainPoint :: CtxKey C.ChainPoint
    CtxPrevChainPoint :: CtxKey C.ChainPoint

deriveGEq ''CtxKey
deriveGCompare ''CtxKey
deriveGShow ''CtxKey
deriveArgDict ''CtxKey

data TxContext where
    TxContext ::
        { ctx :: DMap CtxKey Identity
        , theEra :: C.ShelleyBasedEra era
        , eraCtx :: EraContext era
        } ->
        TxContext

writeContext :: CtxKey a -> a -> TxContext -> TxContext
writeContext key val txctx = txctx{ctx = DMap.insert key (pure val) (ctx txctx)}

getContext :: CtxKey a -> TxContext -> Maybe a
getContext key = fmap runIdentity . DMap.lookup key . ctx

writeContextEra :: forall era a. (C.IsShelleyBasedEra era) => EraCtxKey era a -> a -> TxContext -> Maybe TxContext
writeContextEra key val (TxContext ctx era eraMap) = case testEquality era (C.shelleyBasedEra @era) of
    Nothing -> Nothing
    Just Refl -> Just (TxContext ctx era (DMap.insert key (pure val) eraMap))

getContextEra :: forall era a. (C.IsShelleyBasedEra era) => EraCtxKey era a -> TxContext -> Maybe a
getContextEra key (TxContext _ era eraMap) = case testEquality era (C.shelleyBasedEra @era) of
    Nothing -> Nothing
    Just Refl -> runIdentity <$> DMap.lookup key eraMap

convertEra :: C.ShelleyBasedEra era -> EraCtxKey anyEra a -> EraCtxKey era b
convertEra _ EraTransaction = EraTransaction
convertEra _ EraMintValue = EraMintValue
convertEra _ EraInputUtxoMap = EraInputUtxoMap

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- NOTE: Our decisions are made based on the context built. At different stages
-- of the pipeline the context we are looking at keeps increasing and our
-- decisions are based on that.

-- Q: Is there a better way to represent this?

-- Context0 is immediately derivable from the transactions.
data Context0 where
    Context0 ::
        { ctxPrevChainPoint :: C.ChainPoint
        , ctxTransaction :: C.Tx era
        , ctxMintValue :: C.TxMintValue C.ViewTx era
        } ->
        Context0

deriving instance Show Context0

-- NOTE: To build contexts other than Context0, we will need to query the node.

data Context1 where
    Context1 ::
        { context0 :: Context0
        , ctxInputUtxoMap :: Map C.TxIn (C.TxOut C.CtxUTxO era)
        } ->
        Context1

deriving instance Show Context1

-- NOTE: The final context should have everything required to run the
-- transaction locally.

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mkContext0 :: C.ChainPoint -> Transaction -> Context0
mkContext0 cp (Transaction tx) =
    Context0
        { ctxPrevChainPoint = cp
        , ctxTransaction = tx
        , ctxMintValue = C.txMintValue . C.getTxBodyContent . C.getTxBody $ tx
        }

getMintPolicies :: Context0 -> Set C.PolicyId
getMintPolicies Context0{..} = getPolicySet $ C.txMintValueToValue ctxMintValue

mkContext1 :: C.LocalNodeConnectInfo -> Context0 -> IO Context1
mkContext1 conn c0@Context0{..} = do
    umap <- queryInputUtxoMap conn ctxPrevChainPoint ctxTransaction
    pure $
        Context1
            { context0 = c0
            , ctxInputUtxoMap = umap
            }

getSpendPolicies :: Context1 -> Set C.PolicyId
getSpendPolicies Context1{..} =
    Set.unions $ getPolicySet . getTxOutValue <$> Map.elems ctxInputUtxoMap
