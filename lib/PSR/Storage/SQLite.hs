module PSR.Storage.SQLite (withSqliteStorage) where

import Cardano.Api (
    BlockHeader (..),
    BlockNo,
    Hash,
    SlotNo,
 )
import Cardano.Ledger.Binary (mkVersion64)
import Cardano.Ledger.Binary qualified as L
import Cardano.Ledger.Plutus (CostModel, ExUnits (..), encodeCostModel)
import Data.Functor (void, (<&>))
import Data.Maybe (mapMaybe)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Database.SQLite.Simple hiding (execute, executeNamed, query, queryNamed)
import PSR.Events.Interface
import PSR.Storage.Interface
import PSR.Storage.SQLite.GetEvents qualified as GetEvents
import PSR.Storage.SQLite.Instances ()
import PSR.Storage.SQLite.Metrics (SqliteMetrics (..), initialiseMetrics)
import PSR.Storage.SQLite.Utils
import PSR.Types (BlockStatus (..))
import PlutusLedgerApi.Common (MajorProtocolVersion (..))

withSqliteStorage :: FilePath -> (Storage -> IO ()) -> IO ()
withSqliteStorage dbPath act = do
    pool <- newPool (defaultPoolConfig (openWithPragmas dbPath) close 120 10)
    metrics <- initialiseMetrics
    storage <- mkStorage metrics pool
    act storage

mkStorage :: SqliteMetrics -> Pool Connection -> IO Storage
mkStorage metrics pool = do
    withResource pool initSchema
    pure $ Storage{..}
  where
    getEvents = GetEvents.getEvents metrics.getEvents_select pool

    -- NOTE: The block may not always exist in our database. And it may not be
    -- possible to get the proper BlockHeader on a rollback event.
    createBlockIfNotExistsUtil ::
        Connection -> SlotNo -> Hash BlockHeader -> Maybe BlockNo -> IO ()
    createBlockIfNotExistsUtil conn slotNo hash mBlockNo = do
        let colsKnown =
                [ col "slot_no" slotNo
                , col "hash" hash
                , col "status" BSUnknown
                ]
            cols =
                case mBlockNo of
                    Nothing -> colsKnown
                    Just blockNo -> col "block_no" blockNo : colsKnown
        sqlInsertLax metrics.createBlockIfNotExists_insert conn "block" cols

    createPartialBlockIfNotExists ::
        Connection -> SlotNo -> Hash BlockHeader -> IO ()
    createPartialBlockIfNotExists conn slotNo hash =
        createBlockIfNotExistsUtil conn slotNo hash Nothing

    createBlockIfNotExists :: Connection -> BlockHeader -> IO ()
    createBlockIfNotExists conn (BlockHeader slotNo hash blockNo) =
        createBlockIfNotExistsUtil conn slotNo hash (Just blockNo)

    -- TODO: Update the metric
    commitBlock :: Connection -> BlockNo -> IO ()
    commitBlock conn blockNo = do
        let q = "UPDATE block SET status = :set_status WHERE block_no = :block_no AND status = :prev_status;"
        executeNamed metrics.createBlockIfNotExists_insert conn q $
            [ ":set_status" := BSCommitted
            , ":prev_status" := BSUnknown
            , ":block_no" := blockNo
            ]

    -- TODO: Update the metric
    cancelBlocksAfterSlot :: Connection -> SlotNo -> IO [Hash BlockHeader]
    cancelBlocksAfterSlot conn slotNo = do
        let q = "UPDATE block SET status = :set_status WHERE slot_no > :slot_no RETURNING hash;"
        fmap (fmap fromOnly) $
            queryNamed metrics.createBlockIfNotExists_insert conn q $
                [ ":set_status" := BSCancelled
                , ":slot_no" := slotNo
                ]

    getOrCreateCostModelParamsId :: Connection -> MajorProtocolVersion -> CostModel -> IO Integer
    getOrCreateCostModelParamsId conn (MajorProtocolVersion v) costModel = do
        version <- mkVersion64 $ fromIntegral v
        let params = L.serialize version $ encodeCostModel costModel
        sqlInsertLax
            metrics.getOrCreateCostModelParamsId_insert
            conn
            "cost_model_params"
            [col "params" params]

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
                    [ col "context_id" eci
                    , col "trace_logs" logs
                    , col "eval_error" evalError
                    , col "exec_budget_cpu" $ toInteger exUnitsSteps
                    , col "exec_budget_mem" $ toInteger exUnitsMem
                    ]
            sqlInsert
                metrics.addExecutionEvent_insert
                conn
                "execution_event"
                params

    addExecutionContext :: BlockHeader -> ExecutionContext -> IO ExecutionContextId
    addExecutionContext blockHeader@(BlockHeader _ hash _) ExecutionContext{..} =
        withResource pool $ \conn -> withTransaction conn $ do
            void $ createBlockIfNotExists conn blockHeader
            costModelParamsId <- getOrCreateCostModelParamsId conn majorProtocolVersion costModel
            let
                ExUnits exBudgetMaxCpu exBudgetMaxMem = exMaxBudget
                params =
                    [ col "block_hash" hash
                    , col "transaction_hash" transactionHash
                    , col "target_script_hash" targetScript.hash
                    , col "target_script_name" targetScript.name
                    , col "shadow_script_hash" shadowScript.hash
                    , col "shadow_script_name" shadowScript.name
                    , col "ledger_language" ledgerLanguage
                    , col "major_protocol_version" majorProtocolVersion
                    , col "datum" datum
                    , col "redeemer" redeemer
                    , col "script_context" scriptContext
                    , col "exec_budget_max_cpu" $ toInteger exBudgetMaxCpu
                    , col "exec_budget_max_mem" $ toInteger exBudgetMaxMem
                    , col "cost_model_params_id" costModelParamsId
                    ]
            rows <-
                sqlInsertReturning
                    metrics.addExecutionEvent_insert
                    conn
                    "execution_context"
                    params
                    ["context_id"]
            case rows of
                [(Only cei) :: Only ExecutionContextId] -> pure cei
                _ ->
                    -- TODO: handle the error properly
                    error "Failed to return execution context id"

    addRollbackEvent :: SlotNo -> Hash BlockHeader -> IO [Hash BlockHeader]
    addRollbackEvent slotNo hash =
        withResource pool $ \conn -> withTransaction conn $ do
            createPartialBlockIfNotExists conn slotNo hash
            blocksCancelled <- cancelBlocksAfterSlot conn slotNo
            let params =
                    [ col "block_hash" hash
                    , col "blocks_cancelled" blocksCancelled
                    ]
            sqlInsert
                metrics.addRollbackEvent_insert
                conn
                "cancellation_event"
                params
            pure blocksCancelled

    addSelectionEvent :: BlockHeader -> IO ()
    addSelectionEvent blockHeader@(BlockHeader _ hash blockNo) =
        withResource pool $ \conn -> withTransaction conn $ do
            void $ createBlockIfNotExists conn blockHeader
            let params = [col "block_hash" hash]
            sqlInsert
                metrics.addSelectionEvent_insert
                conn
                "selection_event"
                params
            -- TODO: Get k from global config
            let k = 2080
            commitBlock conn (blockNo - k)

    getExecutionContexts :: [FilterBy] -> IO [(BlockHeader, ExecutionContextId, ExecutionContext)]
    getExecutionContexts filters =
        withResource pool $ \conn -> withTransaction conn $ do
            let
                (whereQuery, whereParams) =
                    mkWhereWithParams $
                        filters <&> \case
                            ByNameOrHash scriptNameOrHash ->
                                ( " (HEX(ec.targret_script_hash) = UPPER(:name_or_hash) OR ec.target_script_name = :name_or_hash) "
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
                    \ ec.target_script_hash, \
                    \ ec.target_script_name, \
                    \ ec.shadow_script_name, \
                    \ ec.shadow_script_name, \
                    \ ec.ledger_language, \
                    \ ec.major_protocol_version, \
                    \ ec.datum, \
                    \ ec.redeemer, \
                    \ ec.script_context, \
                    \ ec.exec_budget_max_cpu, \
                    \ ec.exec_budget_max_mem, \
                    \ cmp.params \
                    \ FROM execution_context ec \
                    \ LEFT JOIN block b ON ec.block_hash = b.hash \
                    \ LEFT JOIN cost_model_params cmp ON cmp.params_id = ec.cost_model_params_id "
                        <> whereQuery
                        <> " ORDER BY ec.created_at ASC LIMIT 1"

            rows :: [BlockHeader :. Only ExecutionContextId :. Maybe ExecutionContext] <-
                queryNamed metrics.getExecutionContextByNameOrScriptHash_select conn sqlQuery whereParams

            pure $
                mapMaybe
                    ( \case
                        (bh :. Only eci :. Just ec) -> Just (bh, eci, ec)
                        _ -> Nothing
                    )
                    rows
