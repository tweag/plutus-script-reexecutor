{-# OPTIONS_GHC -Wno-orphans #-}

module PSR.Types (
    ChainSyncEvent (..),
    ChainSyncEventException (..),
    Block (..),
    ScriptSubtitutionInfo,
    TransactionExecutionResult,
    RedeemerReportWithLogs,
    PwcExecutionResult,
    PreEvaluationPlutusError (..),
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Conway.Scripts qualified as L
import Cardano.Ledger.Plutus qualified as L
import Control.Exception (Exception)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

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
    Block :: C.BlockHeader -> C.ShelleyBasedEra era -> [C.Tx era] -> Block

deriving instance Show C.BlockHeader
deriving instance Show Block

type ScriptName = Text

type ScriptSubtitutionInfo era =
    Map C.ScriptHash [(Maybe ScriptName, L.AlonzoScript era)]

-- NOTE: We are omiting a lot of information here. We can add it on demand if
-- required.
data PreEvaluationPlutusError
    = PeRedeemerPointsToUnknownScriptHash
    | PeMissingScript

type ExecutionData = (L.PlutusWithContext, [Text], L.ExUnits)

type PwcExecutionResult era =
    Either (L.TransactionScriptFailure era) ExecutionData

type PwcExecutionResult_ =
    Either C.ScriptExecutionError ExecutionData

-- NOTE: The two levels of nesting in Either depict different type of
-- errors. Combining them loses information.
--
-- The first level of error is the error when the redeemer corresponding to the
-- script hash that we want to scrutinize is not found. In the second level the
-- errors in the list correspond to the errors that we get on running
-- independent plutus scripts we substitute.
--
-- NOTE: Should we rename this to "RedeemerReportWithLogs_"?
type TransactionExecutionResult =
    Map
        C.ScriptWitnessIndex
        ( Either
            PreEvaluationPlutusError
            [(Maybe ScriptName, PwcExecutionResult_)]
        )

type RedeemerReportWithLogs era =
    Map
        (L.PlutusPurpose L.AsIx era)
        ( Either
            PreEvaluationPlutusError
            [(Maybe ScriptName, PwcExecutionResult era)]
        )
