module PSR.ContextBuilder (
    Context0 (..),
    Context1 (..),
    mkContext0,
    getMintPolicies,
    mkContext1,
    getSpendPolicies,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import PSR.Chain
import PSR.Streaming

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
