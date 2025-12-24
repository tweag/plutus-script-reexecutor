module PSR.Streaming (
    LiveEvent (..),
    liveEventStream,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Concurrent (forkIO)
import Control.Exception (throw)
import Control.Monad (void)
import Data.Function ((&))
import Ouroboros.Network.Protocol.ChainSync.Client (
    ClientStIdle (..),
    ClientStIntersect (..),
    ClientStNext (..),
 )
import PSR.Chain
import PSR.ConfigMap qualified as CM
import PSR.ContextBuilder
import PSR.Types
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

    actionOnAwait =
        putStrLn "Waiting..."

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
-- LiveEvent
--------------------------------------------------------------------------------

data LiveEvent
    = LeRollForward C.BlockHeader
    | LeRollBackward C.ChainPoint
    | LeReexecutedTransaction TransactionExecutionResult

--------------------------------------------------------------------------------
-- Streams
--------------------------------------------------------------------------------

traceChainSyncEvent :: (LiveEvent -> IO ()) -> ChainSyncEvent -> IO ()
traceChainSyncEvent cb cse = case cse of
    RollForward (C.BlockInMode _ blk) _ ->
        cb $ LeRollForward $ C.getBlockHeader blk
    RollBackward cp _ ->
        cb $ LeRollBackward cp

streamChainSyncEvents ::
    -- | Connection Info
    C.LocalNodeConnectInfo ->
    -- | The points on the chain to start streaming from
    [C.ChainPoint] ->
    Stream IO ChainSyncEvent
streamChainSyncEvents conn points =
    Stream.fromCallback (void . forkIO . subscribeToChainSyncEvents conn points)

streamBlocks ::
    CM.ConfigMap ->
    [C.ChainPoint] ->
    (LiveEvent -> IO ()) ->
    Stream IO (C.ChainPoint, Block)
streamBlocks CM.ConfigMap{..} points cb =
    streamChainSyncEvents cmLocalNodeConn points
        & Stream.trace (traceChainSyncEvent cb)
        & fmap getEventBlock
        & Stream.postscanl unshiftFst
        -- TODO: Can we filter here to remove any block that doesn't reference
        -- any scripts we care about?
        & Stream.mapMaybe (\(a, b) -> (a,) <$> b)

streamTransactionContext ::
    CM.ConfigMap ->
    BlockContext era ->
    (LiveEvent -> IO ()) ->
    Stream IO (TransactionContext era)
streamTransactionContext cm ctx1@BlockContext{..} cb =
    Stream.fromList ctxTransactions
        & Stream.mapMaybe (mkTransactionContext cm ctx1)
        & Stream.trace (pCompact . ctxTransactionExecutionResult)
        & Stream.trace (cb . LeReexecutedTransaction . ctxTransactionExecutionResult)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mainLoop :: CM.ConfigMap -> [C.ChainPoint] -> (LiveEvent -> IO ()) -> IO ()
mainLoop cm@CM.ConfigMap{..} points cb =
    streamBlocks cm points cb
        & Stream.fold (Fold.drainMapM (uncurry consumeBlock))
  where
    consumeBlock previousChainPt (Block sbe txList) = do
        case proveAlonzoEraOnwards sbe of
            Nothing -> pure ()
            Just era -> do
                ctx1 <- mkBlockContext cmLocalNodeConn previousChainPt era txList
                streamTransactionContext cm ctx1 cb
                    & Stream.fold Fold.drain

liveEventStream :: CM.ConfigMap -> [C.ChainPoint] -> Stream IO LiveEvent
liveEventStream cm points =
    Stream.fromCallback (void . forkIO . mainLoop cm points)
