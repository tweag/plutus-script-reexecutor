{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.Storage.SQLite where

import Cardano.Api (
    BlockHeader (..),
    BlockNo (..),
    Hash (..),
    ScriptHash (..),
    SlotNo (..),
    TxId,
    deserialiseFromRawBytes,
    serialiseToRawBytes,
 )
import Cardano.Api qualified as C
import Cardano.Ledger.Binary (mkVersion64)
import Cardano.Ledger.Binary qualified as L
import Cardano.Ledger.Plutus (CostModel, ExUnits (..), decodeCostModel, encodeCostModel)
import Cardano.Ledger.Plutus qualified as L
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.ByteString (ByteString, toStrict)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Foldable (forM_)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)
import PSR.Events.Interface
import PSR.Storage.Interface
import PlutusLedgerApi.Common (Data (..), MajorProtocolVersion (..), PlutusLedgerLanguage (..))

withSqliteStorage :: FilePath -> (Storage -> IO ()) -> IO ()
withSqliteStorage dbPath act =
    withConnection dbPath $ \sqliteConn -> do
        storage <- mkStorage sqliteConn
        act storage

mkStorage :: Connection -> IO Storage
mkStorage conn = do
    initSchema conn
    pure $ Storage{..}
  where
    getOrCreateBlockId :: BlockHeader -> IO Integer
    getOrCreateBlockId (BlockHeader slotNo hash blockNo) = do
        execute
            conn
            "INSERT OR IGNORE INTO block (block_no, slot_no, hash) values (?, ?, ?)"
            (blockNo, slotNo, hash)

        rows <- query conn "SELECT block_id from block where block_no = ? and slot_no = ? and hash = ?" (blockNo, slotNo, hash)
        case rows of
            [Only blockId :: Only Integer] -> return blockId
            _ -> error "Can't find the inserted block"

    getOrCreateCostModelParamsId :: MajorProtocolVersion -> CostModel -> IO Integer
    getOrCreateCostModelParamsId (MajorProtocolVersion v) costModel = do
        version <- mkVersion64 $ fromIntegral v
        let params = L.serialize version $ encodeCostModel costModel
        execute
            conn
            "INSERT OR IGNORE INTO cost_model_params (params) values (?)"
            (Only params)

        rows <- query conn "SELECT params_id from cost_model_params where params = ?" (Only params)
        case rows of
            [Only paramsId :: Only Integer] -> return paramsId
            _ -> error "Can't find the inserted block"

    addExecutionEvent :: ExecutionContextId -> TraceLogs -> Maybe EvalError -> ExUnits -> IO ()
    addExecutionEvent eci logs evalError exUnits =
        withTransaction conn $ do
            let
                ExUnits{exUnitsMem, exUnitsSteps} = exUnits
                params =
                    [ ":context_id" := eci
                    , ":trace_logs" := logs
                    , ":eval_error" := evalError
                    , ":exec_budget_cpu" := toInteger exUnitsSteps
                    , ":exec_budget_mem" := toInteger exUnitsMem
                    ]
            executeNamed
                conn
                "INSERT INTO execution_event \
                \ (context_id, trace_logs, eval_error, exec_budget_cpu, exec_budget_mem) \
                \ values (:context_id, :trace_logs, :eval_error, :exec_budget_cpu, :exec_budget_mem)"
                params

    addExecutionContext :: BlockHeader -> ExecutionContext -> IO ExecutionContextId
    addExecutionContext blockHeader ExecutionContext{..} = do
        withTransaction conn $ do
            blockId <- getOrCreateBlockId blockHeader
            costModelParamsId <- getOrCreateCostModelParamsId majorProtocolVersion costModel
            let
                params =
                    [ ":block_id" := blockId
                    , ":transaction_hash" := transactionHash
                    , ":script_hash" := scriptHash
                    , ":script_name" := scriptName
                    , ":ledger_language" := ledgerLanguage
                    , ":major_protocol_version" := majorProtocolVersion
                    , ":datum" := datum
                    , ":redeemer" := redeemer
                    , ":script_context" := scriptContext
                    , ":cost_model_params_id" := costModelParamsId
                    ]
            rows <-
                queryNamed
                    conn
                    "INSERT INTO execution_context \
                    \ (block_id, transaction_hash, script_hash, script_name, ledger_language, major_protocol_version, datum, redeemer, script_context, cost_model_params_id) \
                    \ values (:block_id, :transaction_hash, :script_hash, :script_name, :ledger_language, :major_protocol_version, :datum, :redeemer, :script_context, :cost_model_params_id) \
                    \ RETURNING context_id"
                    params
            case rows of
                [(Only cei) :: Only ExecutionContextId] -> pure cei
                _ ->
                    -- TODO: handle the error properly
                    error "Failed to return execution context id"

    addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    addCancellationEvent blockHeader scriptHash =
        withTransaction conn $ do
            blockId <- getOrCreateBlockId blockHeader
            execute
                conn
                "INSERT INTO cancellation_event (block_id, script_hash) values (?, ?)"
                (blockId, scriptHash)

    addSelectionEvent :: BlockHeader -> IO ()
    addSelectionEvent blockHeader =
        withTransaction conn $ do
            blockId <- getOrCreateBlockId blockHeader
            execute
                conn
                "INSERT INTO selection_event (block_id) values (?)"
                (Only blockId)

    getEvents :: EventFilterParams -> IO [Event]
    getEvents EventFilterParams{..} =
        withTransaction conn $ do
            let
                -- see `docs/specification.md` for default values
                limitParameter =
                    let limit = fromMaybe 50 _eventFilterParam_limit
                     in min limit 1000
                offsetParameter = fromMaybe 0 _eventFilterParam_offset

                mkNamedParam q n v = (" (" <> q <> ") " :: Text, n := v)
                paramsWithQueries =
                    catMaybes
                        [ _eventFilterParam_type
                            <&> mkNamedParam
                                "(CASE \
                                \ WHEN :event_type = 'execution' THEN ec.block_id \
                                \ WHEN :event_type = 'cancellation' THEN c.block_id \
                                \ WHEN :event_type = 'selection' THEN s.block_id \
                                \ END) IS NOT NULL"
                                ":event_type"
                        , _eventFilterParam_name_or_script_hash
                            <&> mkNamedParam
                                "ec.script_name = :name_or_hash or ec.script_hash = :name_or_hash or c.script_hash = :name_or_hash"
                                ":name_or_hash"
                        , _eventFilterParam_slot_begin
                            <&> mkNamedParam
                                "b.slot_no >= :slot_begin"
                                ":slot_begin"
                        , _eventFilterParam_slot_end
                            <&> mkNamedParam
                                "b.slot_no <= :slot_end"
                                ":slot_end"
                        , _eventFilterParam_time_begin
                            <&> mkNamedParam
                                "ee.created_at >= :time_begin or c.created_at >= :time_begin or s.created_at >= :time_begin"
                                ":time_begin"
                        , _eventFilterParam_time_end
                            <&> mkNamedParam
                                "ee.created_at <= :time_end or c.created_at <= :time_end or s.created_at <= :time_end"
                                ":time_end"
                        ]

                paramsQuery :: Query
                paramsQuery =
                    Query $
                        let q = T.intercalate " and " $ map fst paramsWithQueries
                         in if T.null q then "" else " where " <> q

                eventsQuery :: Query
                eventsQuery =
                    "SELECT b.slot_no, b.hash, b.block_no, \
                    \ CASE \
                    \   WHEN ec.block_id THEN 'execution' \
                    \   WHEN c.block_id THEN 'cancellation' \
                    \   WHEN s.block_id THEN 'selection' \
                    \ END, \
                    \ COALESCE(ee.created_at, c.created_at, s.created_at), \
                    \ c.script_hash, \
                    \ json(ee.trace_logs), \
                    \ ee.eval_error, \
                    \ ee.exec_budget_cpu, \
                    \ ee.exec_budget_mem, \
                    \ ec.transaction_hash, \
                    \ ec.script_name, \
                    \ ec.script_hash, \
                    \ ec.ledger_language, \
                    \ ec.major_protocol_version, \
                    \ ec.datum, \
                    \ ec.redeemer, \
                    \ ec.script_context, \
                    \ cmp.params \
                    \ FROM block b \
                    \ LEFT JOIN execution_context ec ON ec.block_id = b.block_id \
                    \ LEFT JOIN execution_event ee ON ee.context_id = ec.context_id \
                    \ LEFT JOIN cost_model_params cmp ON cmp.params_id = ec.cost_model_params_id \
                    \ LEFT JOIN cancellation_event c ON c.block_id = b.block_id \
                    \ LEFT JOIN selection_event s ON s.block_id = b.block_id "
                        <> paramsQuery
                        <> " ORDER BY b.block_id DESC \
                           \ LIMIT :limit \
                           \ OFFSET :offset"

                parameters = map snd paramsWithQueries <> [":limit" := limitParameter, ":offset" := offsetParameter]

            rows :: [BlockHeader :. (EventType, UTCTime, Maybe ScriptHash, Maybe TraceLogs, Maybe EvalError, Maybe Integer, Maybe Integer) :. Maybe ExecutionContext] <-
                queryNamed conn eventsQuery parameters

            pure $
                rows <&> \case
                    (blockHeader :. (eventType, createdAt, mScriptHash, mTraceLogs, evalError, mExBudgetCpu, mExBudgetMem) :. mExecutionContext) ->
                        let
                            payload = case eventType of
                                Execution ->
                                    case (mTraceLogs, mExBudgetCpu, mExBudgetMem, mExecutionContext) of
                                        (Just traceLogs, Just exBudgetCpu, Just exBudgetMem, Just context) ->
                                            ExecutionPayload $
                                                ExecutionEventPayload
                                                    { traceLogs
                                                    , evalError
                                                    , exUnits = ExUnits (fromInteger exBudgetCpu) (fromInteger exBudgetMem)
                                                    , context
                                                    }
                                        _ ->
                                            -- TODO: handle the error properly
                                            error "Failed to retrieve execution event"
                                Cancellation ->
                                    case mScriptHash of
                                        Just sh -> CancellationPayload sh
                                        _ ->
                                            -- TODO: handle the error properly
                                            error "The cancellation event should have a script hash"
                                Selection -> SelectionPayload
                         in
                            Event{..}

initSchema :: Connection -> IO ()
initSchema conn = do
    let schemaQuery = $(embedStringFile =<< makeRelativeToProject "./schema.sql")
    forM_ (T.split (== ';') schemaQuery) $ \q -> do
        execute_ conn (Query q)

-- instances

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
        case deserialiseFromRawBytes (C.proxyToAsType C.Proxy) bs of
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
            ("execution" :: String) -> pure Execution
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
        scriptName <- maybeField
        scriptHash <- maybeField
        ledgerLanguage <- maybeField
        majorProtocolVersion <- maybeField
        datum <- maybeField
        redeemer <- maybeField
        scriptContext <- maybeField
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
                <*> pure scriptName
                <*> scriptHash
                <*> ledgerLanguage
                <*> majorProtocolVersion
                <*> pure datum
                <*> pure redeemer
                <*> scriptContext
                <*> costModel

maybeField :: (FromField a) => RowParser (Maybe a)
maybeField =
    fieldWith $
        fromField >=> \case
            Just v -> pure v
            _ -> pure Nothing
