module PSR.Storage.SQLite.GetEvents (getEvents) where

import Cardano.Api (
    BlockHeader (..),
    BlockNo,
    Hash,
    SlotNo,
 )
import Cardano.Ledger.Plutus (ExUnits (..))
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple hiding (execute, executeNamed, query, queryNamed)
import PSR.Events.Interface (
    EvalError (..),
    Event (..),
    EventFilterParams (..),
    EventPayload (..),
    EventType (..),
    ExecutionContext (..),
    ExecutionEventPayload (..),
    TraceLogs (..),
 )
import PSR.Metrics qualified as Metrics
import PSR.Storage.SQLite.Instances ()
import PSR.Storage.SQLite.Utils
import PSR.Types (BlockStatus)

getEvents :: Metrics.Summary -> Pool Connection -> Int -> EventFilterParams -> IO [Event]
getEvents getEvents_select pool confirmationDepth EventFilterParams{..} =
    withResource pool $ \conn -> withTransaction conn $ do
        let
            -- see `docs/specification.md` for default values
            limitParameter =
                let limit = fromMaybe 50 _eventFilterParam_limit
                 in min limit 1000
            offsetParameter = fromMaybe 0 _eventFilterParam_offset

            mkNamedParam q n v = (" (" <> q <> ") " :: Text, n := v)
            (whereQuery, whereParams) =
                mkWhereWithParams $
                    catMaybes
                        [ _eventFilterParam_type
                            <&> mkNamedParam
                                "(CASE \
                                \ WHEN :event_type = 'execution' THEN ec.block_hash \
                                \ WHEN :event_type = 'rollback' THEN c.block_hash \
                                \ WHEN :event_type = 'selection' THEN s.block_hash \
                                \ END) IS NOT NULL"
                                ":event_type"
                        , _eventFilterParam_target_name_or_script_hash
                            <&> mkNamedParam
                                "ec.target_script_name = :name_or_hash or HEX(ec.target_script_hash) = UPPER(:name_or_hash)"
                                ":name_or_hash"
                        , _eventFilterParam_shadow_name_or_script_hash
                            <&> mkNamedParam
                                "ec.shadow_script_name = :name_or_hash or HEX(ec.shadow_script_hash) = UPPER(:name_or_hash)"
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
                \   WHEN ec.block_hash IS NOT NULL THEN 'execution' \
                \   WHEN re.block_hash IS NOT NULL THEN 'rollback' \
                \   WHEN s.block_hash  IS NOT NULL THEN 'selection' \
                \ END, \
                \ COALESCE(ee.created_at, re.created_at, s.created_at), \
                \ json(ee.trace_logs), \
                \ rb.block_hashes, \
                \ ee.eval_error, \
                \ ee.exec_budget_cpu, \
                \ ee.exec_budget_mem, \
                \ CASE \
                \   WHEN rb.block_hashes IS NOT NULL \
                \       THEN 'cancelled' \
                \   WHEN \
                \       b_max.max_block_no  IS NOT NULL AND \
                \       b.block_no IS NOT NULL AND \
                \       (b_max.max_block_no - :confirmation_depth) > b.block_no \
                \       THEN 'committed' \
                \   ELSE 'unknown' \
                \ END,\
                \ ec.transaction_hash, \
                \ ec.target_script_hash, \
                \ ec.target_script_name, \
                \ ec.shadow_script_hash, \
                \ ec.shadow_script_name, \
                \ ec.ledger_language, \
                \ ec.major_protocol_version, \
                \ ec.datum, \
                \ ec.redeemer, \
                \ ec.script_context, \
                \ ec.exec_budget_max_cpu, \
                \ ec.exec_budget_max_mem, \
                \ cmp.params \
                \ FROM block b \
                \ LEFT JOIN execution_context ec  ON ec.block_hash = b.hash \
                \ LEFT JOIN execution_event   ee  ON ee.context_id = ec.context_id \
                \ LEFT JOIN cost_model_params cmp ON cmp.params_id = ec.cost_model_params_id \
                \ LEFT JOIN selection_event   s   ON s.block_hash  = b.hash  \
                \ LEFT JOIN rollback_event    re  ON re.block_hash = b.hash \
                \ LEFT JOIN \
                \   (SELECT event_id, string_agg(block_hash, ' ') AS block_hashes \
                \    FROM rollback_block)     rb  ON re.event_id   = rb.event_id \
                \ JOIN (SELECT max(block_no) as max_block_no from block) b_max"
                    <> whereQuery
                    <> " ORDER BY COALESCE(ee.created_at, re.created_at, s.created_at) ASC \
                       \ LIMIT :limit \
                       \ OFFSET :offset"

            parameters =
                whereParams
                    <> [ ":limit" := limitParameter
                       , ":offset" := offsetParameter
                       , ":confirmation_depth" := confirmationDepth
                       ]
        fmap rowToEvent
            <$> queryNamed getEvents_select conn eventsQuery parameters
  where
    rowToEvent ::
        ( ( SlotNo
          , Hash BlockHeader
          , Maybe BlockNo
          , EventType
          , UTCTime
          , Maybe TraceLogs
          , Maybe [Hash BlockHeader]
          , Maybe EvalError
          , Maybe Integer
          , Maybe Integer
          , BlockStatus
          )
            :. Maybe ExecutionContext
        ) ->
        Event
    rowToEvent
        ( ( slotNo
                , blockHash
                , mBlockNo
                , eventType
                , createdAt
                , mTraceLogs
                , blocksCancelled
                , evalError
                , mExBudgetCpu
                , mExBudgetMem
                , blockStatus
                )
                :. mExecutionContext
            ) =
            let
                payloadError = "rowToEvent: Unable to parse the payload."
                payload = fromMaybe (error payloadError) $ case eventType of
                    Execution -> do
                        traceLogs <- mTraceLogs
                        exBudgetCpu <- mExBudgetCpu
                        exBudgetMem <- mExBudgetMem
                        let exUnits =
                                ExUnits
                                    (fromInteger exBudgetCpu)
                                    (fromInteger exBudgetMem)
                        context <- mExecutionContext
                        blockNo <- mBlockNo
                        pure $ ExecutionPayload blockNo $ ExecutionEventPayload{..}
                    Rollback -> pure $ RollbackPayload (fromMaybe [] blocksCancelled)
                    Selection -> SelectionPayload <$> mBlockNo
             in
                Event{..}
