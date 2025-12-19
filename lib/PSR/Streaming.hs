module PSR.Streaming (
    streamChainSyncEvents,
    isByron,
    unshiftFst,
    streamBlocks,
    streamTransactionContext,
    mainLoop,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Concurrent (forkIO)
import Control.Exception (throw)
import Control.Monad (void)
import Data.Function ((&))
import Log (object, (.=))
import Ouroboros.Network.Protocol.ChainSync.Client (
    ClientStIdle (..),
    ClientStIntersect (..),
    ClientStNext (..),
 )
import PSR.Chain
import PSR.ConfigMap (ResolvedScript (..))
import PSR.ContextBuilder
import PSR.Logging (HasLogger, LogSeverity (..), logMsgR, logMsgWithR)
import PSR.Types
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Scanl (Scanl)
import Streamly.Data.Scanl.Prelude qualified as Scanl
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as Stream
import Text.Pretty.Simple

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
-- Debugging Utils
--------------------------------------------------------------------------------

compactPrintOpts :: OutputOptions
compactPrintOpts =
    defaultOutputOptionsDarkBg
        { outputOptionsCompact = True
        , outputOptionsCompactParens = True
        , outputOptionsIndentAmount = 2
        , outputOptionsStringStyle = Literal
        }

pCompact :: (Show a, C.MonadIO m) => a -> m ()
pCompact = pPrintOpt CheckColorTty compactPrintOpts

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

streamChainSyncEvents ::
    -- | Connection Info
    C.LocalNodeConnectInfo ->
    -- | The points on the chain to start streaming from
    [C.ChainPoint] ->
    Stream (App context err) ChainSyncEvent
streamChainSyncEvents conn points =
    Stream.morphInner C.liftIO $
        Stream.fromCallback (void . forkIO . subscribeToChainSyncEvents conn points)

streamBlocks ::
    C.LocalNodeConnectInfo ->
    [C.ChainPoint] ->
    Stream (App context err) (C.ChainPoint, Block)
streamBlocks conn points = do
    streamChainSyncEvents conn points
        & Stream.filter (not . isByron)
        & fmap getEventBlock
        & Stream.postscanl unshiftFst
        -- TODO: Can we filter here to remove any block that doesn't reference
        -- any scripts we care about?
        & Stream.mapMaybe (\(a, b) -> (a,) <$> b)

streamTransactionContext ::
    (HasConfigMap context, HasLogger context) =>
    BlockContext era ->
    Stream (App context err) (TransactionContext era)
streamTransactionContext ctx1@BlockContext{..} =
    Stream.fromList ctxTransactions
        & Stream.mapMaybeM (mkTransactionContext ctx1)
        & Stream.trace
            ( \TransactionContext{..} -> do
                -- pCompact ctx
                logMsgR Debug "Found scripts:"
                mapM_ printResolvedScript ctxRelevantScripts
            )

printResolvedScript :: (HasLogger context) => ResolvedScript -> App context err ()
printResolvedScript ResolvedScript{..} =
    logMsgWithR Debug "Script" $
        object
            [ "rsScriptHash" .= rsScriptHash
            , "rsName" .= rsName
            , "rsSource" .= rsSource
            ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

mainLoop :: (HasConfigMap context, HasLogger context) => [C.ChainPoint] -> App context err ()
mainLoop points = do
    conn <- view cmLocalNodeConn
    streamBlocks conn points
        & Stream.fold (Fold.drainMapM (uncurry consumeBlock))
  where
    consumeBlock previousChainPt (Block era txList) = do
        ctx1 <- mkBlockContext previousChainPt era txList
        streamTransactionContext ctx1
            & Stream.fold Fold.drain
