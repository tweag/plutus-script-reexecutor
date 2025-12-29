module PSR.Chain (
    getEventBlock,
    sysStartQuery,
    eraHistoryQuery,
    utxoMapQuery,
    pParamsQuery,
    getTxOutValue,
    mkLocalNodeConnectInfo,
    getPolicySet,
    getTxOutScriptAddr,
    getTxInSet,
    runLocalStateQueryExpr,
)
where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (SocketPath)
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Control.Exception (Exception, throw)
import Data.Functor.Identity (Identity (..))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import PSR.Types

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

data QueryException
    = QeAcquiringFailure C.AcquiringFailure
    | QeUnsupportedNtcVersionError C.UnsupportedNtcVersionError
    | QeEraMismatch C.EraMismatch
    deriving stock (Show)
    deriving anyclass (Exception)

getTxInSet :: C.Tx era -> Set C.TxIn
getTxInSet tx =
    let txBody = C.getTxBody tx
        txBodyContent = C.getTxBodyContent txBody
     in Set.fromList $ map fst $ C.txIns txBodyContent

utxoMapQuery ::
    C.ShelleyBasedEra era ->
    [C.Tx era] ->
    C.LocalStateQueryExpr block point C.QueryInMode r IO (C.UTxO era)
utxoMapQuery era txList = do
    let txInsSet = Set.unions $ getTxInSet <$> txList
    res <- C.queryUtxo era $ C.QueryUTxOByTxIn txInsSet
    case res of
        Left err -> throw $ QeUnsupportedNtcVersionError err
        Right (Left err) -> throw $ QeEraMismatch err
        Right (Right val) -> pure val

pParamsQuery ::
    C.ShelleyBasedEra era ->
    C.LocalStateQueryExpr block point C.QueryInMode r IO (L.PParams (C.ShelleyLedgerEra era))
pParamsQuery era = do
    res <- C.queryProtocolParameters era
    case res of
        Left err -> throw $ QeUnsupportedNtcVersionError err
        Right (Left err) -> throw $ QeEraMismatch err
        Right (Right val) -> pure val

eraHistoryQuery :: C.LocalStateQueryExpr block point C.QueryInMode r IO C.EraHistory
eraHistoryQuery = do
    res <- C.queryEraHistory
    case res of
        Left err -> throw $ QeUnsupportedNtcVersionError err
        Right val -> pure val

sysStartQuery :: C.LocalStateQueryExpr block point C.QueryInMode r IO C.SystemStart
sysStartQuery = do
    res <- C.querySystemStart
    case res of
        Left err -> throw $ QeUnsupportedNtcVersionError err
        Right val -> pure val

runLocalStateQueryExpr ::
    C.LocalNodeConnectInfo ->
    C.ChainPoint ->
    C.LocalStateQueryExpr C.BlockInMode C.ChainPoint C.QueryInMode () IO a ->
    IO a
runLocalStateQueryExpr conn cp query = do
    res <- C.executeLocalStateQueryExpr conn (C.SpecificPoint cp) query
    case res of
        Left err -> throw $ QeAcquiringFailure err
        Right val -> pure val

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

getBlock :: C.BlockInMode -> Maybe Block
getBlock (C.BlockInMode cera blk) = do
    era <- runIdentity (C.requireShelleyBasedEra cera)
    pure $ Block (C.getBlockHeader blk) era $ C.getBlockTxs blk

getEventBlock :: ChainSyncEvent -> (C.ChainPoint, Maybe Block)
getEventBlock (RollForward bim cp) = (C.chainTipToChainPoint cp, getBlock bim)
getEventBlock (RollBackward cp _) = (cp, Nothing)

getTxOutValue :: C.TxOut ctx era -> C.Value
getTxOutValue (C.TxOut _ val _ _) = C.txOutValueToValue val

getTxOutScriptAddr :: C.TxOut ctx era -> Maybe C.ScriptHash
getTxOutScriptAddr
    (C.TxOut (C.AddressInEra _ (C.ShelleyAddress _ pCred _)) _ _ _) =
        case C.fromShelleyPaymentCredential pCred of
            C.PaymentCredentialByScript sHash -> Just sHash
            _ -> Nothing
getTxOutScriptAddr _ = Nothing

getPolicySet :: C.Value -> Set C.PolicyId
getPolicySet val = Map.keysSet (C.valueToPolicyAssets val)
