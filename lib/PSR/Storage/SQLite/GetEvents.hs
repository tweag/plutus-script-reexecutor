module PSR.Storage.SQLite.GetEvents (getEvents) where

import Cardano.Api (
    BlockHeader (..),
    ScriptHash (..),
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

getEvents :: Metrics.Summary -> Pool Connection -> EventFilterParams -> IO [Event]
getEvents getEvents_select pool EventFilterParams{..} =
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
                                \ WHEN :event_type = 'cancellation' THEN c.block_hash \
                                \ WHEN :event_type = 'selection' THEN s.block_hash \
                                \ END) IS NOT NULL"
                                ":event_type"
                        , _eventFilterParam_target_name_or_script_hash
                            <&> mkNamedParam
                                "ec.target_script_name = :name_or_hash or HEX(ec.target_script_hash) = UPPER(:name_or_hash) or HEX(c.target_script_hash) = UPPER(:name_or_hash)"
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
                \   WHEN c.block_hash IS NOT NULL THEN 'cancellation' \
                \   WHEN s.block_hash IS NOT NULL THEN 'selection' \
                \ END, \
                \ COALESCE(ee.created_at, c.created_at, s.created_at), \
                \ c.script_hash, \
                \ json(ee.trace_logs), \
                \ ee.eval_error, \
                \ ee.exec_budget_cpu, \
                \ ee.exec_budget_mem, \
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
                \ LEFT JOIN execution_context ec ON ec.block_hash = b.hash \
                \ LEFT JOIN execution_event ee ON ee.context_id = ec.context_id \
                \ LEFT JOIN cost_model_params cmp ON cmp.params_id = ec.cost_model_params_id \
                \ LEFT JOIN cancellation_event c ON c.block_hash = b.hash \
                \ LEFT JOIN selection_event s ON s.block_hash = b.hash "
                    <> whereQuery
                    <> " ORDER BY COALESCE(ee.created_at, c.created_at, s.created_at) ASC \
                       \ LIMIT :limit \
                       \ OFFSET :offset"

            parameters = whereParams <> [":limit" := limitParameter, ":offset" := offsetParameter]

        rows :: [BlockHeader :. (EventType, UTCTime, Maybe ScriptHash, Maybe TraceLogs, Maybe EvalError, Maybe Integer, Maybe Integer) :. Maybe ExecutionContext] <-
            queryNamed getEvents_select conn eventsQuery parameters

        pure $
            rows <&> \case
                (blockHeader :. (eventType, createdAt, mCancellationScriptHash, mTraceLogs, evalError, mExBudgetCpu, mExBudgetMem) :. mExecutionContext) ->
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
                                case mCancellationScriptHash of
                                    Just sh -> CancellationPayload sh
                                    _ ->
                                        -- TODO: handle the error properly
                                        error "The cancellation event should have a script hash"
                            Selection -> SelectionPayload
                     in
                        Event{..}
