module PSR.Types (
    ChainSyncEvent (..),
    ChainSyncEventException (..),
    Block (..),
    isByron,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Exception (Exception)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data ChainSyncEvent
    = RollForward C.BlockInMode C.ChainTip
    | RollBackward C.ChainPoint C.ChainTip
    deriving stock (Show, Generic)

isByron :: ChainSyncEvent -> Bool
isByron (RollForward (C.BlockInMode C.ByronEra _) _) = True
isByron _ = False

data ChainSyncEventException = NoIntersectionFound
    deriving stock (Show)
    deriving anyclass (Exception)

data Block where
    Block :: C.ShelleyBasedEra era -> [C.Tx era] -> Block

deriving instance Show Block
