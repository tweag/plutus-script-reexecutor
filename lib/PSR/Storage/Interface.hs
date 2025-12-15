module PSR.Storage.Interface
  ( Event(..)
  , ExecutionEventPayload(..)
  , EventType(..)
  , Storage(..)
  , FilterQueryParams(..)
  ) where

import Data.Text (Text)

import PSR.HTTP.API (FilterQueryParams(..), EventType(..))

import Cardano.Api (
  BlockHeader,
  ScriptHash,
  TxId,
 )

data Event = Event
  { eventType :: EventType
  }

data ExecutionEventPayload = ExecutionEventPayload
  { blockHeader :: BlockHeader
  , transactionHash :: TxId 
  , scriptHash :: ScriptHash
  , scriptName :: Maybe Text
  , trace :: Text 
  }

data Storage = Storage
  { addExecutionEvent :: ExecutionEventPayload -> IO ()
  , addCancellationEvent :: BlockHeader -> ScriptHash -> IO ()
  , addSelectionEvent :: BlockHeader -> IO ()
  , getEvents :: FilterQueryParams -> IO [Event] 
  }


