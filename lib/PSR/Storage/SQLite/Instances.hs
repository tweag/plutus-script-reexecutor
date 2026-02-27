{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.Storage.SQLite.Instances where

import Cardano.Api (
    BlockHeader (..),
    BlockNo (..),
    Hash,
    ScriptHash (..),
    SlotNo (..),
    TxId,
    deserialiseFromRawBytes,
    serialiseToRawBytes,
 )
import Cardano.Api qualified as C
import Cardano.Ledger.Binary (mkVersion64)
import Cardano.Ledger.Binary qualified as L
import Cardano.Ledger.Plutus (ExUnits (..), decodeCostModel)
import Cardano.Ledger.Plutus qualified as L
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.ByteString (ByteString, toStrict)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Database.SQLite.Simple hiding (execute, executeNamed, query, queryNamed)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)
import PSR.Events.Interface
import PlutusLedgerApi.Common (Data (..), MajorProtocolVersion (..), PlutusLedgerLanguage (..))

deriving instance Generic BlockHeader
deriving instance Show BlockHeader
deriving instance FromRow BlockHeader

instance ToField PlutusLedgerLanguage where
    toField = \case
        PlutusV1 -> toField (1 :: Integer)
        PlutusV2 -> toField (2 :: Integer)
        PlutusV3 -> toField (3 :: Integer)

instance FromField PlutusLedgerLanguage where
    fromField f = do
        fromField f >>= \case
            (1 :: Integer) -> pure PlutusV1
            (2 :: Integer) -> pure PlutusV2
            (3 :: Integer) -> pure PlutusV3
            _ -> returnError ConversionFailed f "Failed to parse PlutusLedgerLanguage"

deriving newtype instance ToField MajorProtocolVersion
deriving newtype instance FromField MajorProtocolVersion

deriving newtype instance ToField BlockNo
deriving newtype instance FromField BlockNo

deriving newtype instance ToField SlotNo
deriving newtype instance FromField SlotNo

deriving newtype instance ToField ExecutionContextId
deriving newtype instance FromField ExecutionContextId

deriving newtype instance ToField EvalError
deriving newtype instance FromField EvalError

instance ToField (Hash BlockHeader) where
    toField hash = toField $ serialiseToRawBytes hash

instance FromField (Hash BlockHeader) where
    fromField f = do
        bs <- fromField f
        case deserialiseFromRawBytes (C.proxyToAsType Proxy) bs of
            Right v -> pure v
            Left err -> returnError ConversionFailed f (show err)

instance ToField ScriptHash where
    toField hash = toField $ serialiseToRawBytes hash

instance FromField ScriptHash where
    fromField f = do
        bs <- fromField f
        case deserialiseFromRawBytes C.AsScriptHash bs of
            Right v -> pure v
            Left err -> returnError ConversionFailed f (show err)

instance ToField TxId where
    toField txId = toField $ serialiseToRawBytes txId

instance FromField TxId where
    fromField f = do
        bs <- fromField f
        case deserialiseFromRawBytes C.AsTxId bs of
            Right v -> pure v
            Left err -> returnError ConversionFailed f (show err)

instance ToField EventType where
    toField Execution = toField ("execution" :: Text)
    toField Selection = toField ("selection" :: Text)
    toField Cancellation = toField ("cancellation" :: Text)

instance FromField EventType where
    fromField f = do
        fromField f >>= \case
            ("execution" :: Text) -> pure Execution
            "cancellation" -> pure Cancellation
            "selection" -> pure Selection
            _ -> returnError ConversionFailed f "Failed to parse event type"

deriving newtype instance Aeson.ToJSON TraceLogs
deriving newtype instance Aeson.FromJSON TraceLogs

instance ToField TraceLogs where
    toField traceLogs = toField $ Aeson.encodeToLazyText traceLogs

instance FromField TraceLogs where
    fromField f = do
        fromField f >>= \s ->
            case Aeson.eitherDecodeStrictText s of
                Right logs -> pure $ TraceLogs logs
                Left err -> returnError ConversionFailed f $ "Failed to parse trace logs: " <> err

instance ToField Data where
    toField = toField . toStrict . serialise @Data

instance FromField Data where
    fromField f = do
        fromField f >>= \s ->
            case deserialiseOrFail s of
                Right d -> pure d
                Left err -> returnError ConversionFailed f $ "Failed to parse data: " <> show err

instance FromRow (Maybe ExecutionContext) where
    fromRow = do
        transactionHash <- maybeField
        targetScriptHash <- maybeField
        targetScriptName <- maybeField

        let
            targetScript = (`ScriptInfo` targetScriptName) <$> targetScriptHash

        shadowScriptHash <- maybeField
        shadowScriptName <- maybeField

        let
            shadowScript = (`ScriptInfo` shadowScriptName) <$> shadowScriptHash

        ledgerLanguage <- maybeField
        majorProtocolVersion <- maybeField
        datum <- maybeField
        redeemer <- maybeField
        scriptContext <- maybeField
        exBudgetMaxCpu <- maybeField
        exBudgetMaxMem <- maybeField
        let exMaxBudget = liftA2 ExUnits (fromInteger <$> exBudgetMaxCpu) (fromInteger <$> exBudgetMaxMem)
        costModel <-
            fieldWith $ \f ->
                fromField f >>= \case
                    Nothing -> pure Nothing
                    Just (bs :: ByteString) -> do
                        lang <- case ledgerLanguage of
                            Just PlutusV1 -> pure L.PlutusV1
                            Just PlutusV2 -> pure L.PlutusV2
                            Just PlutusV3 -> pure L.PlutusV3
                            _ -> returnError ConversionFailed f "Failed to parse cost model: ledger language is incorrect"
                        version <- case mkVersion64 . fromIntegral . getMajorProtocolVersion <$> majorProtocolVersion of
                            Just (Just v) -> pure v
                            _ -> returnError ConversionFailed f "Failed to parse version"

                        case L.decodeFullDecoder' version "CostModel" (decodeCostModel lang) bs of
                            Right v -> pure $ Just v
                            Left err -> returnError ConversionFailed f $ "Failed to parse cost model: " <> show err
        pure $
            ExecutionContext
                <$> transactionHash
                <*> targetScript
                <*> shadowScript
                <*> ledgerLanguage
                <*> majorProtocolVersion
                <*> pure datum
                <*> pure redeemer
                <*> scriptContext
                <*> exMaxBudget
                <*> costModel

maybeField :: (FromField a) => RowParser (Maybe a)
maybeField =
    fieldWith $
        fromField >=> \case
            Just v -> pure v
            _ -> pure Nothing
