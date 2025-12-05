module PSR.Chain (
    getEventTransactions,
    queryInputUtxoMap,
    getTxOutValue,
    mkLocalNodeConnectInfo,
    getPolicySet,
)
where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (SocketPath)
import Cardano.Api qualified as C
import Control.Exception (Exception, throw)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import PSR.Streaming

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

data QueryException
    = QeAcquiringFailure C.AcquiringFailure
    | QeUnsupportedNtcVersionError C.UnsupportedNtcVersionError
    | QeEraMismatch C.EraMismatch
    deriving stock (Show)
    deriving anyclass (Exception)

queryInputUtxoMap ::
    forall era.
    C.LocalNodeConnectInfo ->
    C.ChainPoint ->
    C.Tx era ->
    IO (Map C.TxIn (C.TxOut C.CtxUTxO era))
queryInputUtxoMap conn cp tx = do
    let txBody = C.getTxBody tx
        txBodyContent = C.getTxBodyContent txBody
        era = case txBody of
            C.ShelleyTxBody era' _ _ _ _ _ -> era'
    let txInsSet = Set.fromList $ map fst $ C.txIns txBodyContent
        query = C.queryUtxo era $ C.QueryUTxOByTxIn txInsSet
    res <- C.executeLocalStateQueryExpr conn (C.SpecificPoint cp) query
    case res of
        Left err -> throw $ QeAcquiringFailure err
        Right (Left err) -> throw $ QeUnsupportedNtcVersionError err
        Right (Right (Left err)) -> throw $ QeEraMismatch err
        Right (Right (Right val)) -> pure $ C.unUTxO val

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

mkLocalNodeConnectInfo :: C.NetworkId -> SocketPath -> C.LocalNodeConnectInfo
mkLocalNodeConnectInfo networkId socketPath =
    C.LocalNodeConnectInfo
        { C.localConsensusModeParams =
            -- This a parameter needed only for the Byron era.
            -- Since the Byron era is over and the parameter has never
            -- changed it is ok to hardcode this.
            C.CardanoModeParams (C.EpochSlots 21600)
        , C.localNodeNetworkId = networkId
        , C.localNodeSocketPath = socketPath
        }

--------------------------------------------------------------------------------
-- Projections
--------------------------------------------------------------------------------

getTransactions :: C.BlockInMode -> [Transaction]
getTransactions bim =
    case bim of
        C.BlockInMode _ blk -> Transaction <$> C.getBlockTxs blk

getEventTransactions :: ChainSyncEvent -> (C.ChainPoint, [Transaction])
getEventTransactions (RollForward bim cp) =
    (C.chainTipToChainPoint cp, getTransactions bim)
getEventTransactions (RollBackward cp _) = (cp, [])

getTxOutValue :: C.TxOut ctx era -> C.Value
getTxOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val

getPolicySet :: C.Value -> Set C.PolicyId
getPolicySet val = Map.keysSet (C.valueToPolicyAssets val)
