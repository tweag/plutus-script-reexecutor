{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.Storage.SQLite (withSqliteStorage) where

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
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple hiding (execute, executeNamed, query, queryNamed)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)
import PSR.Events.Interface
import PSR.Metrics (Summary, observeDuration, regSummary)
import PSR.Storage.Interface
import PlutusLedgerApi.Common (Data (..), MajorProtocolVersion (..), PlutusLedgerLanguage (..))

pragmas :: [Query]
pragmas =
    [ "synchronous = NORMAL"
    , "journal_mode = WAL"
    , "locking_mode = NORMAL"
    ]

openWithPragmas :: FilePath -> IO Connection
openWithPragmas dbPath = do
    conn <- open dbPath
    forM_ pragmas $ \q -> do
        execute_ conn ("PRAGMA " <> q)
    pure conn

withSqliteStorage :: FilePath -> (Storage -> IO ()) -> IO ()
withSqliteStorage dbPath act = do
    pool <- newPool (defaultPoolConfig (openWithPragmas dbPath) close 120 10)
    metrics <- initialiseMetrics
    storage <- mkStorage metrics pool
    act storage

execute :: forall q. (ToRow q) => Summary -> Connection -> Query -> q -> IO ()
execute metric conn q row = observeDuration metric (SQL.execute conn q row)

executeNamed :: Summary -> Connection -> Query -> [NamedParam] -> IO ()
executeNamed metric conn q params = observeDuration metric (SQL.executeNamed conn q params)

query :: forall q r. (ToRow q, FromRow r) => Summary -> Connection -> Query -> q -> IO [r]
query metric conn q params = observeDuration metric (SQL.query conn q params)

queryNamed :: forall r. (FromRow r) => Summary -> Connection -> Query -> [NamedParam] -> IO [r]
queryNamed metric conn q params = observeDuration metric (SQL.queryNamed conn q params)

mkWhereWithParams :: [(Text, NamedParam)] -> (Query, [NamedParam])
mkWhereWithParams queriesWithParams =
    let
        resultQuery =
            Query $
                let q = T.intercalate " AND " $ map fst queriesWithParams
                 in if T.null q then "" else " WHERE " <> q
     in
        (resultQuery, map snd queriesWithParams)

mkStorage :: SqliteMetrics -> Pool Connection -> IO Storage
mkStorage metrics pool = do
    withResource pool initSchema
    pure $ Storage{..}
  where
    getOrCreateBlockId :: Connection -> BlockHeader -> IO Integer
    getOrCreateBlockId conn (BlockHeader slotNo hash blockNo) = do
        execute
            metrics.getOrCreateBlockId_insert
            conn
            "INSERT OR IGNORE INTO block (block_no, slot_no, hash) values (?, ?, ?)"
            (blockNo, slotNo, hash)

        rows <-
            query
                metrics.getOrCreateBlockId_select
                conn
                "SELECT block_id from block where block_no = ? and slot_no = ? and hash = ?"
                (blockNo, slotNo, hash)
        case rows of
            [Only blockId :: Only Integer] -> return blockId
            _ -> error "Can't find the inserted block"

    getOrCreateCostModelParamsId :: Connection -> MajorProtocolVersion -> CostModel -> IO Integer
    getOrCreateCostModelParamsId conn (MajorProtocolVersion v) costModel = do
        version <- mkVersion64 $ fromIntegral v
        let params = L.serialize version $ encodeCostModel costModel
        execute
            metrics.getOrCreateCostModelParamsId_insert
            conn
            "INSERT OR IGNORE INTO cost_model_params (params) values (?)"
            (Only params)

        rows <-
            query
                metrics.getOrCreateCostModelParamsId_select
                conn
                "SELECT params_id from cost_model_params where params = ?"
                (Only params)
        case rows of
            [Only paramsId :: Only Integer] -> return paramsId
            _ -> error "Can't find the inserted block"

    addExecutionEvent :: ExecutionContextId -> TraceLogs -> Maybe EvalError -> ExUnits -> IO ()
    addExecutionEvent eci logs evalError exUnits =
        withResource pool $ \conn -> withTransaction conn $ do
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
                metrics.addExecutionEvent_insert
                conn
                "INSERT INTO execution_event \
                \ (context_id, trace_logs, eval_error, exec_budget_cpu, exec_budget_mem) \
                \ values (:context_id, :trace_logs, :eval_error, :exec_budget_cpu, :exec_budget_mem)"
                params

    addExecutionContext :: BlockHeader -> ExecutionContext -> IO ExecutionContextId
    addExecutionContext blockHeader ExecutionContext{..} =
        withResource pool $ \conn -> withTransaction conn $ do
            blockId <- getOrCreateBlockId conn blockHeader
            costModelParamsId <- getOrCreateCostModelParamsId conn majorProtocolVersion costModel
            let
                ExUnits exBudgetMaxCpu exBudgetMaxMem = exMaxBudget
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
                    , ":exec_budget_max_cpu" := toInteger exBudgetMaxCpu
                    , ":exec_budget_max_mem" := toInteger exBudgetMaxMem
                    , ":cost_model_params_id" := costModelParamsId
                    ]
            rows <-
                queryNamed
                    metrics.addExecutionEvent_insert
                    conn
                    "INSERT INTO execution_context \
                    \ (block_id, transaction_hash, script_hash, script_name, ledger_language, major_protocol_version, datum, redeemer, script_context, exec_budget_max_cpu, exec_budget_max_mem, cost_model_params_id) \
                    \ values (:block_id, :transaction_hash, :script_hash, :script_name, :ledger_language, :major_protocol_version, :datum, :redeemer, :script_context, :exec_budget_max_cpu, :exec_budget_max_mem, :cost_model_params_id) \
                    \ RETURNING context_id"
                    params
            case rows of
                [(Only cei) :: Only ExecutionContextId] -> pure cei
                _ ->
                    -- TODO: handle the error properly
                    error "Failed to return execution context id"

    addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
    addCancellationEvent blockHeader scriptHash =
        withResource pool $ \conn -> withTransaction conn $ do
            blockId <- getOrCreateBlockId conn blockHeader
            execute
                metrics.addCancellationEvent_insert
                conn
                "INSERT INTO cancellation_event (block_id, script_hash) values (?, ?)"
                (blockId, scriptHash)

    addSelectionEvent :: BlockHeader -> IO ()
    addSelectionEvent blockHeader =
        withResource pool $ \conn -> withTransaction conn $ do
            blockId <- getOrCreateBlockId conn blockHeader
            execute
                metrics.addSelectionEvent_insert
                conn
                "INSERT INTO selection_event (block_id) values (?)"
                (Only blockId)

    getEvents :: EventFilterParams -> IO [Event]
    getEvents EventFilterParams{..} =
        withResource pool $ \conn -> withTransaction conn $ do
            let
                -- see `docs/specification.md` for default values
                limitParameter =
                    let limit = fromMaybe 50 _eventFilterParam_limit
                     in min limit 1000
                offsetParameter = fromMaybe 0 _eventFilterParam_offset

                mkNamedParam q n v = (" (" <> q <> ") " :: Text, n := v)
                (paramsQuery, params) =
                    mkWhereWithParams $
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
                                    "ec.script_name = :name_or_hash or HEX(ec.script_hash) = UPPER(:name_or_hash) or HEX(c.script_hash) = UPPER(:name_or_hash)"
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
                    \ ec.exec_budget_max_cpu, \
                    \ ec.exec_budget_max_mem, \
                    \ cmp.params \
                    \ FROM block b \
                    \ LEFT JOIN execution_context ec ON ec.block_id = b.block_id \
                    \ LEFT JOIN execution_event ee ON ee.context_id = ec.context_id \
                    \ LEFT JOIN cost_model_params cmp ON cmp.params_id = ec.cost_model_params_id \
                    \ LEFT JOIN cancellation_event c ON c.block_id = b.block_id \
                    \ LEFT JOIN selection_event s ON s.block_id = b.block_id "
                        <> paramsQuery
                        <> " ORDER BY COALESCE(ee.created_at, c.created_at, s.created_at) ASC \
                           \ LIMIT :limit \
                           \ OFFSET :offset"

                parameters = params <> [":limit" := limitParameter, ":offset" := offsetParameter]

            rows :: [BlockHeader :. (EventType, UTCTime, Maybe ScriptHash, Maybe TraceLogs, Maybe EvalError, Maybe Integer, Maybe Integer) :. Maybe ExecutionContext] <-
                queryNamed metrics.getEvents_select conn eventsQuery parameters

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

    getExecutionContexts :: [FilterBy] -> IO [(BlockHeader, ExecutionContextId, ExecutionContext)]
    getExecutionContexts filters =
        withResource pool $ \conn -> withTransaction conn $ do
            let
                (paramsQuery, params) =
                    mkWhereWithParams $
                        filters <&> \case
                            ByNameOrHash scriptNameOrHash ->
                                ( " (HEX(ec.script_hash) = UPPER(:name_or_hash) OR ec.script_name = :name_or_hash) "
                                , ":name_or_hash" := scriptNameOrHash
                                )
                            ByTxId txId ->
                                ( " (HEX(ec.transaction_hash) = UPPER(:transaction_hash)) "
                                , ":transaction_hash" := txId
                                )
                            ByContextId cid ->
                                ( " (ec.context_id = :context_id) "
                                , ":context_id" := cid
                                )
                sqlQuery =
                    "SELECT b.slot_no, b.hash, b.block_no, \
                    \ ec.context_id, \
                    \ ec.transaction_hash, \
                    \ ec.script_name, \
                    \ ec.script_hash, \
                    \ ec.ledger_language, \
                    \ ec.major_protocol_version, \
                    \ ec.datum, \
                    \ ec.redeemer, \
                    \ ec.script_context, \
                    \ ec.exec_budget_max_cpu, \
                    \ ec.exec_budget_max_mem, \
                    \ cmp.params \
                    \ FROM execution_context ec \
                    \ LEFT JOIN block b ON ec.block_id = b.block_id \
                    \ LEFT JOIN cost_model_params cmp ON cmp.params_id = ec.cost_model_params_id "
                        <> paramsQuery
                        <> " ORDER BY ec.created_at ASC LIMIT 1"

            rows :: [BlockHeader :. Only ExecutionContextId :. Maybe ExecutionContext] <-
                queryNamed metrics.getExecutionContextByNameOrScriptHash_select conn sqlQuery params

            pure $
                mapMaybe
                    ( \case
                        (bh :. Only eci :. Just ec) -> Just (bh, eci, ec)
                        _ -> Nothing
                    )
                    rows

initSchema :: Connection -> IO ()
initSchema conn = withTransaction conn $ do
    let schemaQuery = $(embedStringFile =<< makeRelativeToProject "./schema.sql")
    forM_ (T.split (== ';') schemaQuery) $ \q -> do
        execute_ conn (Query q)

-- Metrics

data SqliteMetrics = SqliteMetrics
    { getOrCreateBlockId_insert :: Summary
    , getOrCreateBlockId_select :: Summary
    , getOrCreateCostModelParamsId_insert :: Summary
    , getOrCreateCostModelParamsId_select :: Summary
    , setOrCreateBlockId_insert :: Summary
    , setOrCreateBlockId_select :: Summary
    , addExecutionEvent_insert :: Summary
    , addCancellationEvent_insert :: Summary
    , addSelectionEvent_insert :: Summary
    , getEvents_select :: Summary
    , getExecutionContextByNameOrScriptHash_select :: Summary
    }

initialiseMetrics :: IO SqliteMetrics
initialiseMetrics = do
    getOrCreateBlockId_insert <-
        regSummary
            "sqlite_getOrCreateBlockId_insert"
            "Execution time of getOrCreateBlockId insert query"
    getOrCreateBlockId_select <-
        regSummary
            "sqlite_getOrCreateBlockId_select"
            "Execution time of getOrCreateBlockId select query"
    getOrCreateCostModelParamsId_select <-
        regSummary
            "sqlite_getOrCreateCostModelParamsId_select"
            "Execution time of getOrCreateCostModelParamsId select query"
    getOrCreateCostModelParamsId_insert <-
        regSummary
            "sqlite_getOrCreateCostModelParamsId_insert"
            "Execution time of getOrCreateCostModelParamsId select query"
    setOrCreateBlockId_insert <-
        regSummary
            "sqlite_setOrCreateBlockId_insert"
            "Execution time of setOrCreateBlockId insert query"
    setOrCreateBlockId_select <-
        regSummary
            "sqlite_setOrCreateBlockId_select"
            "Execution time of setOrCreateBlockId select query"
    addExecutionEvent_insert <-
        regSummary
            "sqlite_addExecutionEvent_insert"
            "Execution time of addExecutionEvent insert query"
    addCancellationEvent_insert <-
        regSummary
            "sqlite_addCancellationEvent_insert"
            "Execution time of addCancellationEvent insert query"
    addSelectionEvent_insert <-
        regSummary
            "sqlite_addSelectionEvent_insert"
            "Execution time of addSelectionEvent insert query"
    getEvents_select <-
        regSummary
            "sqlite_getEvents_select"
            "Execution time of getEvents select query"
    getExecutionContextByNameOrScriptHash_select <-
        regSummary
            "sqlite_getOrCreateCostModelParamsId_select"
            "Execution time of getExecutionContextByNameOrScriptHash select query"
    pure SqliteMetrics{..}

-- instances

deriving instance Generic BlockHeader
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
                <*> pure scriptName
                <*> scriptHash
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
