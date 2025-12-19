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

import PSR.Events.Interface (Events(..))

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
-- Streams
--------------------------------------------------------------------------------

traceChainSyncEvent :: Events -> ChainSyncEvent -> IO ()
traceChainSyncEvent events = \case
    RollForward (C.BlockInMode _ blk) _ -> do
        let header = C.getBlockHeader blk
        events.addSelectionEvent header
    _ -> pure ()

streamChainSyncEvents ::
    -- | Connection Info
    C.LocalNodeConnectInfo ->
    -- | The points on the chain to start streaming from
    [C.ChainPoint] ->
    Stream IO ChainSyncEvent
streamChainSyncEvents conn points =
    Stream.fromCallback (void . forkIO . subscribeToChainSyncEvents conn points)

streamBlocks :: Events -> CM.ConfigMap -> [C.ChainPoint] -> Stream IO (C.ChainPoint, Block)
streamBlocks events CM.ConfigMap{..} points =
    streamChainSyncEvents cmLocalNodeConn points
        & Stream.trace (traceChainSyncEvent events)
        & fmap getEventBlock
        & Stream.postscanl unshiftFst
        -- TODO: Can we filter here to remove any block that doesn't reference
        -- any scripts we care about?
        & Stream.mapMaybe (\(a, b) -> (a,) <$> b)

streamTransactionContext ::
    CM.ConfigMap -> BlockContext era -> Stream IO (TransactionContext era)
streamTransactionContext cm ctx1@BlockContext{..} =
    Stream.fromList ctxTransactions
        & Stream.mapMaybeM (mkTransactionContext cm ctx1)
        & Stream.trace
            ( \TransactionContext{..} -> do
                -- pCompact ctx
                putStrLn "Found scripts:"
                mapM_ pCompact ctxRelevantScripts
            )

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mainLoop :: Events -> CM.ConfigMap -> [C.ChainPoint] -> IO ()
mainLoop events cm@CM.ConfigMap{..} points =
    streamBlocks events cm points
        & Stream.fold (Fold.drainMapM (uncurry consumeBlock))
  where
    consumeBlock previousChainPt (Block sbe txList) = do
        case proveAlonzoEraOnwards sbe of
            Nothing -> pure ()
            Just era -> do
                ctx1 <- mkBlockContext cmLocalNodeConn previousChainPt era txList
                streamTransactionContext cm ctx1
                    & Stream.fold Fold.drain
