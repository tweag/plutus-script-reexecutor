{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.Storage.SQLite where

import PSR.Events.Interface (Event(..), EventType(..), ExecutionEventPayload(..), EventFilterParams(..), EventPayload(..))
import PSR.Storage.Interface

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

import Data.Functor ((<&>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

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

  addExecutionEvent :: BlockHeader -> ExecutionEventPayload -> IO ()
  addExecutionEvent blockHeader ExecutionEventPayload{..} = do
    withTransaction conn $ do
      blockId <- getOrCreateBlockId blockHeader
      execute
        conn
        "INSERT INTO execution_event (block_id, transaction_hash, script_hash, name, trace) values (?, ?, ?, ?, ?)"
        (blockId, transactionHash, scriptHash, scriptName, trace)

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

        mkNamedParam q n v = (q, n := v)
        paramsWithQueries =
          catMaybes
            [ fmap
                ( mkNamedParam
                    " (e.name = :name_or_hash or e.script_hash = :name_or_hash or c.script_hash = :name_or_hash) "
                    ":name_or_hash"
                    . toField
                )
                _eventFilterParam_name_or_script_hash
            , fmap
                (mkNamedParam " b.slot_no >= :slot_begin " ":slot_begin" . toField)
                _eventFilterParam_slot_begin
            , fmap
                (mkNamedParam " b.slot_no <= :slot_end " ":slot_end" . toField)
                _eventFilterParam_slot_end
            , fmap
                ( mkNamedParam
                    " (e.created_at >= :time_begin or c.created_at >= :time_begin or s.created_at >= :time_begin) "
                    ":time_begin"
                    . toField
                )
                _eventFilterParam_time_begin
            , fmap
                ( mkNamedParam
                    " (e.created_at <= :time_end or c.created_at <= :time_end or s.created_at <= :time_end) "
                    ":time_end"
                    . toField
                )
                _eventFilterParam_time_end
            ]

        paramsQuery :: Query
        paramsQuery =
          Query $
            let q = T.intercalate " and " $ map fst paramsWithQueries
             in if T.null q then "" else " where " <> q

        eventsQuery :: Query
        eventsQuery =
          "select b.block_no, b.slot_no, b.hash, \
          \ CASE WHEN e.block_id THEN 'execution' WHEN c.block_id THEN 'cancellation' WHEN s.block_id THEN 'selection' END, \
          \ COALESCE(e.created_at, c.created_at, s.created_at), \
          \ COALESCE(e.script_hash, s.script_hash), \
          \ e.transaction_hash, \
          \ e.name, \
          \ e.trace \
          \ from block b \
          \ left join execution_event e on e.block_id = b.block_id \
          \ left join cancellation_event c on c.block_id = b.block_id \
          \ left join selection_event s on s.block_id = b.block_id "
            <> paramsQuery
            <> " order by b.block_id DESC \
               \ limit :limit offset :offset"

        parameters = map snd paramsWithQueries <> [":limit" := limitParameter, ":offset" := offsetParameter]

      rows :: [(BlockNo, SlotNo, Hash BlockHeader, EventType, UTCTime, Maybe ScriptHash, Maybe TxId, Maybe Text, Maybe Text)] <-
        queryNamed conn eventsQuery parameters

      pure $
        rows <&> \case
          (blockNo, slotNo, hash, eventType, createdAt, mScriptHash, mTransactionHash, scriptName, mTrace) ->
            let
              blockHeader = BlockHeader slotNo hash blockNo
              payload = case eventType of
                  Execution ->
                    case (mScriptHash, mTransactionHash, mTrace) of
                      (Just scriptHash, Just transactionHash, Just trace) -> ExecutionPayload $ ExecutionEventPayload
                        { transactionHash
                        , scriptHash
                        , scriptName
                        , trace
                        }
                      _ -> 
                          -- TODO: handle the error properly
                          error "The execution event should have a script hash, a transaction hash and a trace"
                  Cancellation ->
                      case mScriptHash of
                          Just sh -> CancellationPayload sh 
                          _ -> 
                              -- TODO: handle the error properly
                              error "The cancellation event should have a script hash"
                  Selection -> SelectionPayload
            in Event{..}

initSchema :: Connection -> IO ()
initSchema conn = withTransaction conn $ do
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS block( \
    \ block_id INTEGER PRIMARY KEY, \
    \ block_no UNSIGNED BIGINT NOT NULL, \
    \ slot_no UNSIGNED BIGINT NOT NULL, \
    \ hash BLOB NOT NULL, \
    \ UNIQUE(block_no, slot_no, hash) )"

  execute_
    conn
    "CREATE TABLE IF NOT EXISTS execution_event(\
    \ block_id INTEGER NOT NULL REFERENCES block(block_id), \
    \ transaction_hash BLOB NOT NULL, \
    \ script_hash BLOB NOT NULL, \
    \ name TEXT, \
    \ trace TEXT, \
    \ created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP )"

  execute_
    conn
    "CREATE TABLE IF NOT EXISTS cancellation_event(\
    \ block_id INTEGER NOT NULL REFERENCES block(block_id), \
    \ script_hash BLOB NOT NULL, \
    \ created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP )"

  execute_
    conn
    "CREATE TABLE IF NOT EXISTS selection_event(\
    \ block_id INTEGER NOT NULL REFERENCES block(block_id), \
    \ created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP )"

-- instances --

instance ToField BlockNo where
  toField (BlockNo blockNo) = toField blockNo

instance FromField BlockNo where
  fromField f = do
    blockNo <- fromField f
    return $ BlockNo blockNo

instance ToField SlotNo where
  toField (SlotNo slotNo) = toField slotNo

instance FromField SlotNo where
  fromField f = do
    slotNo <- fromField f
    return $ SlotNo slotNo

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

instance FromField EventType where
  fromField f = do
    fromField f >>= \case
      ("execution" :: String) -> pure Execution
      "cancellation" -> pure Cancellation
      "selection" -> pure Selection
      _ -> returnError ConversionFailed f "Failed to parse event type"
