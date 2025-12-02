module PSR.Streaming
  ( ChainSyncEvent (..)
  , streamChainSyncEvents
  ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (SocketPath)
import Ouroboros.Network.Protocol.ChainSync.Client
  (ClientStIdle (..), ClientStIntersect (..), ClientStNext (..))
import Cardano.Api qualified as C
-- import Cardano.Slotting.Block (BlockNo (..))
-- import Cardano.Slotting.Slot (WithOrigin (At, Origin), withOrigin)
import Control.Exception (Exception, throw)
import GHC.Generics (Generic)
-- import GHC.Word (Word64)
import Streamly.Data.Stream.Prelude (Stream)
import Streamly.Data.Stream.Prelude qualified as Stream
import Control.Concurrent (forkIO)
import Control.Monad (void)

--------------------------------------------------------------------------------
-- Notes
--------------------------------------------------------------------------------

{- FAQ:

* Why does "subscribeToChainSyncEvents" take [C.ChainPoint] and not
  C.ChainPoint?

From my understanding the node walks its chain backwards internally and returns
the first one it finds. The client and the node are not always in sync and the
client might not know the exact point the node is on. So the client may give
something like: [p10, p5, p2, genesis]

-}

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data ChainSyncEvent
  = RollForward C.BlockInMode C.ChainTip
  | RollBackward C.ChainPoint C.ChainTip
  deriving stock (Show, Generic)

data ChainSyncEventException = NoIntersectionFound
  deriving stock (Show)
  deriving anyclass (Exception)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

{- | "subscribeToChainSyncEvents" uses the chain-sync mini-protocol to
connect to a locally running node and fetch blocks from the given
starting point.
-}
subscribeToChainSyncEvents
  :: SocketPath
  -- ^ Path to the node socket
  -> C.NetworkId
  -> [C.ChainPoint]
  -- ^ The points on the chain to start streaming from
  -> (ChainSyncEvent -> IO ())
  -> IO ()
subscribeToChainSyncEvents socketPath networkId points callback =
  C.connectToLocalNode
    C.LocalNodeConnectInfo
      { C.localConsensusModeParams =
          -- This a parameter needed only for the Byron era.
          -- Since the Byron era is over and the parameter has never
          -- changed it is ok to hardcode this.
          C.CardanoModeParams (C.EpochSlots 21600)
      , C.localNodeNetworkId = networkId
      , C.localNodeSocketPath = socketPath
      }
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
      pure $ SendMsgFindIntersect points $ ClientStIntersect
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

streamChainSyncEvents
  :: SocketPath
  -- ^ Path to the node socket
  -> C.NetworkId
  -> [C.ChainPoint]
  -- ^ The points on the chain to start streaming from
  -> Stream IO ChainSyncEvent
streamChainSyncEvents s n p =
  Stream.fromCallback (void . forkIO . subscribeToChainSyncEvents s n p)
