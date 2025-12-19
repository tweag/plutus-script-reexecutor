module PSR.Types (
    ChainSyncEvent (..),
    ChainSyncEventException (..),
    Block (..),
    ScriptSubtitutionInfo,
    pCompact,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Ledger.Conway.Scripts qualified as L
import Control.Exception (Exception)
import Data.Map (Map)
import GHC.Generics (Generic)
import Text.Pretty.Simple

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

data Block where
    Block :: C.ShelleyBasedEra era -> [C.Tx era] -> Block

deriving instance Show Block

type ScriptSubtitutionInfo era = Map C.ScriptHash (L.AlonzoScript era)

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

pCompact :: (Show a) => a -> IO ()
pCompact = pPrintOpt CheckColorTty compactPrintOpts
