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

import PSR.Events.Interface (EvalError (..), Events (..), ExecutionContext (..), ExecutionEventPayload (..), TraceLogs (..))

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
import Control.Monad (void)
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
import PSR.Metrics (Counter, Summary, incCounter, incCounterBy, observeDuration, regCounter, regSummary)
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

traceTransactionExecutionResult :: Events -> TransactionContext era -> IO ()
traceTransactionExecutionResult events tc =
    -- TODO: we need to use the script purpose to tell the difference between executions
    forM_ (Map.elems tc.ctxTransactionExecutionResult) $ \case
        Right (pwc, logs, exUnits) -> addEvent pwc logs exUnits Nothing
        Left (C.ScriptErrorEvaluationFailed (C.DebugPlutusFailure evalErr pwc exUnits logs)) -> addEvent pwc logs exUnits (Just evalErr)
        -- TODO: we might need to cover more errors, ex budget
        _ -> pure ()
  where
    addEvent
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
                scriptHash = C.ScriptHash pwcScriptHash
                ledgerLanguage =
                    case isLanguage @l of
                        SPlutusV1 -> PlutusV1
                        SPlutusV2 -> PlutusV2
                        SPlutusV3 -> PlutusV3
                        SPlutusV4 -> PlutusV3
                (scriptContext, datum, redeemer) = extractContextDatumRedeemer args
                context =
                    ExecutionContext
                        { transactionHash = C.getTxId $ C.getTxBody tc.ctxTransaction
                        , scriptName = Map.lookup scriptHash tc.ctxRelevantScripts >>= CM.rsName
                        , scriptHash
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

countTransactions :: Maybe Block -> Int
countTransactions Nothing = 0
countTransactions (Just (Block _ _ txs)) = length txs

streamBlocks :: StreamingMetrics -> Events -> CM.ConfigMap -> [C.ChainPoint] -> Stream IO (C.ChainPoint, Block)
streamBlocks metrics events CM.ConfigMap{..} points =
    streamChainSyncEvents cmLocalNodeConn points
        & Stream.trace (const (incCounter metrics.blocks_since_start))
        & Stream.trace (traceChainSyncEvent events)
        & fmap getEventBlock
        & Stream.postscanl unshiftFst
        -- TODO: Can we filter here to remove any block that doesn't reference
        -- any scripts we care about?
        & Stream.trace (\(_, txs) -> incCounterBy metrics.tx_since_start (countTransactions txs))
        & Stream.mapMaybe (\(a, b) -> (a,) <$> b)

streamTransactionContext ::
    ContextBuilderMetrics -> CM.ConfigMap -> BlockContext era -> Stream IO (TransactionContext era)
streamTransactionContext cbMetrics cm ctx1@BlockContext{..} =
    Stream.fromList ctxTransactions
        & Stream.mapMaybeM (mkTransactionContext cbMetrics cm ctx1)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mainLoop :: Events -> CM.ConfigMap -> [C.ChainPoint] -> IO ()
mainLoop events cm@CM.ConfigMap{..} points = do
    metrics <- initialiseMetrics
    cbMetrics <- initialiseContextBuilderMetrics
    streamBlocks metrics events cm points
        & Stream.fold (Fold.drainMapM (uncurry (consumeBlock metrics cbMetrics)))
  where
    consumeBlock metrics cbMetrics previousChainPt (Block bh sbe txList) =
        observeDuration metrics.mainLoop_consumeBlock_runtime $
            case proveAlonzoEraOnwards sbe of
                Nothing -> pure ()
                Just era -> do
                    ctx1 <- mkBlockContext cbMetrics bh cmLocalNodeConn previousChainPt era txList
                    streamTransactionContext cbMetrics cm ctx1
                        & Stream.trace (traceTransactionExecutionResult events)
                        & Stream.fold Fold.drain

--------------------------------------------------------------------------------
-- Module metrics
--------------------------------------------------------------------------------
data StreamingMetrics = StreamingMetrics
    { mainLoop_consumeBlock_runtime :: Summary
    , blocks_since_start :: Counter
    , tx_since_start :: Counter
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
    pure StreamingMetrics{..}
