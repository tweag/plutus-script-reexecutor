module PSR.Streaming (
    streamChainSyncEvents,
    unshiftFst,
    streamBlocks,
    streamTransactionContext,
    mainLoop,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Api.Pretty (Pretty (pretty), docToText)
import Cardano.Ledger.Binary (getVersion64)
import Cardano.Ledger.Plutus (
    PlutusArgs,
    PlutusWithContext (..),
    SLanguage (..),
    isLanguage,
 )
import Control.Concurrent (forkIO)
import Control.Exception (throw)
import Control.Monad (join, void, when)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Map qualified as Map
import Ouroboros.Network.Protocol.ChainSync.Client (
    ClientStIdle (..),
    ClientStIntersect (..),
    ClientStNext (..),
 )
import PSR.Chain
import PSR.ConfigMap qualified as CM
import PSR.ContextBuilder
import PSR.Events.Interface (EvalError (..), Events (..), ExecutionContext (..), ExecutionEventPayload (..), ScriptInfo (..), TraceLogs (..))
import PSR.Metrics (Counter, Gauge, Summary, getGauge, incCounter, incCounterBy, observeDuration, regCounter, regGauge, regSummary, setGauge)
import PSR.Types
import PlutusLedgerApi.Common (
    MajorProtocolVersion (MajorProtocolVersion),
    PlutusLedgerLanguage (..),
 )
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Scanl (Scanl)
import Streamly.Data.Scanl.Prelude qualified as Scanl
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as Stream

--------------------------------------------------------------------------------
-- Notes
--------------------------------------------------------------------------------

{- FAQ:

Q: Why does "subscribeToChainSyncEvents" take [C.ChainPoint] and not
  C.ChainPoint?

From my understanding the node walks its chain backwards internally and returns
the first one it finds. The client and the node are not always in sync and the
client might not know the exact point the node is on. So the client may give
something like: [p10, p5, p2, genesis]

NOTE: Stream Nesting

There are 2 types of streams in streamly: Stream and StreamK. StreamK should
ideally be used instead of Stream when there are nested operations. Stream does
not perform well with deep nesting as fusion breaks. Alternatively, we can use
unfoldEach for nesting streams while supporting fusion.

-}

--------------------------------------------------------------------------------
-- Streaming Utils
--------------------------------------------------------------------------------

-- Pairs the current set of transactions with the previous chainpoint
unshiftFst :: (Monad m) => Scanl m (a, b) (a, b)
unshiftFst =
    snd <$> Scanl.mkScanl step initial
  where
    step (prev, _) (new, txs) = (new, (prev, txs))
    initial =
        ( error "unshiftFst: Use postscanl"
        , error "unshiftFst: Use postscanl"
        )

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

{- | "subscribeToChainSyncEvents" uses the chain-sync mini-protocol to
connect to a locally running node and fetch blocks from the given
starting point.
-}
subscribeToChainSyncEvents ::
    -- | Connection Info
    C.LocalNodeConnectInfo ->
    -- | The points on the chain to start streaming from
    [C.ChainPoint] ->
    (ChainSyncEvent -> IO ()) ->
    IO ()
subscribeToChainSyncEvents conn points callback =
    C.connectToLocalNode
        conn
        C.LocalNodeClientProtocols
            { C.localChainSyncClient =
                C.LocalChainSyncClient $ C.ChainSyncClient chainSyncClient
            , C.localStateQueryClient = Nothing
            , C.localTxMonitoringClient = Nothing
            , C.localTxSubmissionClient = Nothing
            }
  where
    -- TODO: When points == [] ensure we start streaming from the latest block.
    -- To start streaming from genesis, use [ChainPointAtGenesis].
    chainSyncClient =
        case points of
            [] -> sendRequestNext
            _ -> sendOnIntersect

    -- This is required if we want PSR to start indexing events from a specific
    -- checkpoint. Essentially set the pointer to somewhere in the past.
    sendOnIntersect =
        pure $
            SendMsgFindIntersect points $
                ClientStIntersect
                    { recvMsgIntersectFound = \chainPoint tip ->
                        C.ChainSyncClient $ do
                            callback (RollBackward chainPoint tip)
                            sendRequestNext
                    , recvMsgIntersectNotFound = throw NoIntersectionFound
                    }

    -- NOTE: We should handle this properly via callback if required.
    actionOnAwait = pure ()

    -- This is required to go to the next block. Essentially forward the
    -- pointer.
    sendRequestNext =
        pure $ SendMsgRequestNext actionOnAwait do
            ClientStNext
                { recvMsgRollForward = \blockInMode tip ->
                    C.ChainSyncClient $ do
                        callback (RollForward blockInMode tip)
                        sendRequestNext
                , recvMsgRollBackward = \chainPoint tip ->
                    C.ChainSyncClient $ do
                        callback (RollBackward chainPoint tip)
                        sendRequestNext
                }

--------------------------------------------------------------------------------
-- Streams
--------------------------------------------------------------------------------

traceChainSyncEvent :: Events -> ChainSyncEvent -> IO ()
traceChainSyncEvent events = \case
    RollForward (C.BlockInMode _ blk) _ -> do
        let header = C.getBlockHeader blk
        events.addSelectionEvent header
    _ -> pure ()

traceTransactionExecutionResult :: CM.ConfigMap -> Events -> TransactionContext era -> IO ()
traceTransactionExecutionResult CM.ConfigMap{cmTargetScriptNames} events tc =
    -- TODO: we need to use the script purpose to tell the difference between executions
    -- TODO: Nesting Eithers made this more difficult to read. We need to clean
    -- this up
    forM_ (Map.elems tc.ctxTransactionExecutionResult) $ \case
        Right elems ->
            forM_ elems $ \case
                (shadowName, shadowHash, val) -> case val of
                    -- This is a script evaluation error.
                    Left (C.ScriptErrorEvaluationFailed (C.DebugPlutusFailure evalErr pwc exUnits logs)) ->
                        addEvent shadowName shadowHash pwc logs exUnits (Just evalErr)
                    Right (pwc, logs, exUnits) ->
                        addEvent shadowName shadowHash pwc logs exUnits Nothing
                    -- TODO: we might need to cover more errors, ex budget
                    _ -> pure ()
        -- NOTE: This is not a script evaluation error but a script
        -- selection error.
        -- TODO: We should also report script missing error.
        Left _ -> pure ()
  where
    addEvent
        shadowName
        shadowHash
        PlutusWithContext
            { pwcArgs = args :: PlutusArgs l
            , pwcCostModel
            , pwcScriptHash
            , pwcProtocolVersion
            , pwcExUnits
            }
        logs
        exUnits
        evalError' = do
            let
                targetScriptHash = C.ScriptHash pwcScriptHash
                ledgerLanguage =
                    case isLanguage @l of
                        SPlutusV1 -> PlutusV1
                        SPlutusV2 -> PlutusV2
                        SPlutusV3 -> PlutusV3
                        SPlutusV4 -> PlutusV3 -- TODO: upd to PlutusV4
                (scriptContext, datum, redeemer) = extractContextDatumRedeemer args
                context =
                    ExecutionContext
                        { transactionHash = C.getTxId $ C.getTxBody tc.ctxTransaction
                        , targetScript =
                            ScriptInfo
                                { name = join $ Map.lookup targetScriptHash cmTargetScriptNames
                                , hash = targetScriptHash
                                }
                        , shadowScript =
                            ScriptInfo
                                { name = shadowName
                                , hash = shadowHash
                                }
                        , ledgerLanguage
                        , majorProtocolVersion = MajorProtocolVersion (fromIntegral (getVersion64 pwcProtocolVersion))
                        , datum
                        , redeemer
                        , scriptContext
                        , costModel = pwcCostModel
                        , exMaxBudget = pwcExUnits
                        }
            void $ do
                eci <- events.addExecutionContext tc.ctxBlockHeader context
                events.addExecutionEvent tc.ctxBlockHeader eci $
                    ExecutionEventPayload
                        { traceLogs = TraceLogs logs
                        , exUnits
                        , evalError = EvalError . docToText . pretty <$> evalError'
                        , context
                        }

streamChainSyncEvents ::
    -- | Connection Info
    C.LocalNodeConnectInfo ->
    -- | The points on the chain to start streaming from
    [C.ChainPoint] ->
    Stream IO ChainSyncEvent
streamChainSyncEvents conn points =
    Stream.fromCallback (void . forkIO . subscribeToChainSyncEvents conn points)

streamBlocks :: StreamingMetrics -> Events -> CM.ConfigMap -> [C.ChainPoint] -> Stream IO (C.ChainPoint, Block)
streamBlocks metrics events CM.ConfigMap{..} points =
    streamChainSyncEvents cmLocalNodeConn points
        & Stream.trace (const (incCounter metrics.blocks_since_start))
        & Stream.trace (traceChainSyncEvent events)
        & fmap getEventBlock
        & Stream.postscanl unshiftFst
        & Stream.mapMaybe (\(a, b) -> (a,) <$> b)

streamTransactionContext ::
    ContextBuilderMetrics -> CM.ConfigMap -> BlockContext era -> Stream IO (TransactionContext era)
streamTransactionContext cbMetrics cm ctx1@BlockContext{..} =
    Stream.fromList ctxTransactions
        & Stream.mapM (mkTransactionContext cbMetrics cm ctx1)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- TODO: Save the auxiliary state to disk when the application is exiting. When
-- restarting, use the saved auxillary state instead of querying the state from
-- the cardano-node.
--
-- NOTE: Think more about how we can reduce the bandwidth of the initial costly
-- query.
--
mainLoop :: Events -> CM.ConfigMap -> [C.ChainPoint] -> IO ()
mainLoop events cm@CM.ConfigMap{..} points = do
    metrics <- initialiseMetrics
    cbMetrics <- initialiseContextBuilderMetrics
    streamBlocks metrics events cm points
        & Stream.fold (Fold.foldlM' (consumeBlock metrics cbMetrics) (pure Nothing))
        & void
  where
    confHashes = Map.keysSet cmShadowScripts
    consumeBlock metrics cbMetrics mUtxoMap (previousChainPt, Block bh sbe txList) = do
        let getUtxoMap =
                case mUtxoMap of
                    Nothing ->
                        getSpendProjectedUtxoMap cmLocalNodeConn cmLeashId previousChainPt sbe confHashes
                    Just utxoMap -> pure utxoMap
            -- NOTE: We only consume a specific set of transactions and not all
            -- the transactions in a block. We use the internal UTxO map to
            -- decide which transaction meet the criteria.
            consumeTransactions era selectedTxs = do
                ctx1 <- mkBlockContext cbMetrics bh cmLocalNodeConn cmLeashId previousChainPt era selectedTxs
                streamTransactionContext cbMetrics cm ctx1
                    & Stream.trace (traceTransactionExecutionResult cm events)
                    & Stream.fold Fold.drain
            withAlonzoEra era = do
                prevUtxoMap <- getUtxoMap
                let ProcessBlockTxList{..} =
                        processBlockTxList confHashes prevUtxoMap txList
                    newUtxoMap = pbtFinalUtxoState
                    selectedTxs = pbtFilteredTransactionList
                incCounterBy metrics.tx_since_start pbtFullTransactionListLength
                -- TODO: It is possible to maintain this count at all times. We
                -- should do that instead of going through our map each and
                -- every time.
                observeUtxoMapValue metrics (Map.size newUtxoMap)
                -- NOTE: In most cases the list of selected transactions is
                -- going to be empty. It is non-empty if and only if,
                -- 1. The block has transactions that involve script executions
                -- 2. These scripts have a non-empty intersection with the
                --    configured scripts
                when (not (null selectedTxs)) $ consumeTransactions era selectedTxs
                pure $ Just newUtxoMap
        observeDuration metrics.mainLoop_consumeBlock_runtime $
            case proveAlonzoEraOnwards sbe of
                Nothing -> pure mUtxoMap
                Just era -> withAlonzoEra era

observeUtxoMapValue :: StreamingMetrics -> Int -> IO ()
observeUtxoMapValue metrics currVal0 = do
    let currVal = fromIntegral currVal0
    setGauge metrics.internal_utxo_map_size_cur currVal
    prevVal <- getGauge metrics.internal_utxo_map_size_max
    when (currVal > prevVal) $
        setGauge metrics.internal_utxo_map_size_max currVal

--------------------------------------------------------------------------------
-- Module metrics
--------------------------------------------------------------------------------

data StreamingMetrics = StreamingMetrics
    { mainLoop_consumeBlock_runtime :: Summary
    , blocks_since_start :: Counter
    , tx_since_start :: Counter
    , -- TODO: Make the name consice.
      internal_utxo_map_size_cur :: Gauge
    , internal_utxo_map_size_max :: Gauge
    }

initialiseMetrics :: IO StreamingMetrics
initialiseMetrics = do
    mainLoop_consumeBlock_runtime <-
        regSummary
            "mainLoop_consumeBlock_runtime"
            "Runtime summary for consumeBlock"
    blocks_since_start <-
        regCounter
            "blocks_since_start"
            "Number of transactions seen since application start"
    tx_since_start <-
        regCounter
            "tx_since_start"
            "The number of transactions seen since application start"
    internal_utxo_map_size_cur <- regGauge "internal_utxo_map_size_cur" ""
    internal_utxo_map_size_max <- regGauge "internal_utxo_map_size_max" ""
    pure StreamingMetrics{..}
