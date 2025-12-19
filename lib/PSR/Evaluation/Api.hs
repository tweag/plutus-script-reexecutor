-- NOTE: The contents of this module are originally copied from
-- Cardano.Api.Tx.Internal.Fee

module PSR.Evaluation.Api (evaluateTransactionExecutionUnitsShelley) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api.Plutus
import Cardano.Api.ProtocolParameters

import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Cardano.Api hiding (evaluateTransactionExecutionUnitsShelley)
import Data.Bifunctor (bimap, second)
import Data.ByteString.Short (ShortByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Exts (IsList (..))

import PSR.Evaluation.Ledger (evalTxExUnitsWithLogs)
import PSR.Types (ScriptSubtitutionInfo, TransactionExecutionResult)

--------------------------------------------------------------------------------
-- Evaluators
--------------------------------------------------------------------------------

type EvalTxExecutionUnitsLog = [Text]

type PlutusScriptBytes = ShortByteString

extractPlutusScriptAndLanguage ::
    (Alonzo.AlonzoEraScript (ShelleyLedgerEra era)) =>
    Alonzo.PlutusScript (ShelleyLedgerEra era) ->
    (PlutusScriptBytes, Plutus.Language)
extractPlutusScriptAndLanguage p =
    let bin = Plutus.unPlutusBinary $ Alonzo.plutusScriptBinary p
        l = Alonzo.plutusScriptLanguage p
     in (bin, l)

extractScriptBytesAndLanguage ::
    (Alonzo.AlonzoEraScript (ShelleyLedgerEra era)) =>
    ( L.PlutusPurpose L.AsItem (ShelleyLedgerEra era)
    , Maybe (Alonzo.PlutusScript (ShelleyLedgerEra era))
    , L.ScriptHash
    ) ->
    ( L.PlutusPurpose L.AsItem (ShelleyLedgerEra era)
    , Maybe (PlutusScriptBytes, Plutus.Language)
    , Ledger.ScriptHash
    )
extractScriptBytesAndLanguage (purpose, mbScript, scriptHash) =
    (purpose, fmap extractPlutusScriptAndLanguage mbScript, scriptHash)

evaluateTransactionExecutionUnitsShelley ::
    forall era.
    () =>
    ScriptSubtitutionInfo (ShelleyLedgerEra era) ->
    ShelleyBasedEra era ->
    SystemStart ->
    LedgerEpochInfo ->
    LedgerProtocolParameters era ->
    UTxO era ->
    L.Tx (ShelleyLedgerEra era) ->
    TransactionExecutionResult
evaluateTransactionExecutionUnitsShelley ssi sbe systemstart epochInfo (LedgerProtocolParameters pp) utxo tx =
    caseShelleyToMaryOrAlonzoEraOnwards
        (const Map.empty)
        ( \w ->
            fromLedgerScriptExUnitsMap w $
                alonzoEraOnwardsConstraints w $
                    evalTxExUnitsWithLogs ssi pp tx (toLedgerUTxO sbe utxo) ledgerEpochInfo systemstart
        )
        sbe
  where
    LedgerEpochInfo ledgerEpochInfo = epochInfo

    fromLedgerScriptExUnitsMap ::
        (Alonzo.AlonzoEraScript (ShelleyLedgerEra era)) =>
        AlonzoEraOnwards era ->
        Map
            (L.PlutusPurpose L.AsIx (ShelleyLedgerEra era))
            (Either (L.TransactionScriptFailure (ShelleyLedgerEra era)) (EvalTxExecutionUnitsLog, Alonzo.ExUnits)) ->
        Map ScriptWitnessIndex (Either ScriptExecutionError (EvalTxExecutionUnitsLog, ExecutionUnits))
    fromLedgerScriptExUnitsMap aOnwards exmap =
        fromList
            [ ( toScriptIndex aOnwards rdmrptr
              , bimap (fromAlonzoScriptExecutionError aOnwards) (second fromAlonzoExUnits) exunitsOrFailure
              )
            | (rdmrptr, exunitsOrFailure) <- toList exmap
            ]

    fromAlonzoScriptExecutionError ::
        (Alonzo.AlonzoEraScript (ShelleyLedgerEra era)) =>
        AlonzoEraOnwards era ->
        L.TransactionScriptFailure (ShelleyLedgerEra era) ->
        ScriptExecutionError
    fromAlonzoScriptExecutionError aOnwards =
        shelleyBasedEraConstraints sbe $ \case
            L.UnknownTxIn txin -> ScriptErrorMissingTxIn txin'
              where
                txin' = fromShelleyTxIn txin
            L.InvalidTxIn txin -> ScriptErrorTxInWithoutDatum txin'
              where
                txin' = fromShelleyTxIn txin
            L.MissingDatum dh -> ScriptErrorWrongDatum (ScriptDataHash dh)
            L.ValidationFailure execUnits evalErr logs scriptWithContext ->
                ScriptErrorEvaluationFailed $ DebugPlutusFailure evalErr scriptWithContext execUnits logs
            L.IncompatibleBudget _ -> ScriptErrorExecutionUnitsOverflow
            L.RedeemerPointsToUnknownScriptHash rdmrPtr ->
                ScriptErrorRedeemerPointsToUnknownScriptHash $ toScriptIndex aOnwards rdmrPtr
            -- This should not occur while using cardano-cli because we zip together
            -- the Plutus script and the use site (txin, certificate etc). Therefore
            -- the redeemer pointer will always point to a Plutus script.
            L.MissingScript indexOfScriptWitnessedItem resolveable ->
                let scriptWitnessedItemIndex = toScriptIndex aOnwards indexOfScriptWitnessedItem
                 in ScriptErrorMissingScript scriptWitnessedItemIndex $
                        ResolvablePointers sbe $
                            Map.map extractScriptBytesAndLanguage resolveable
            L.NoCostModelInLedgerState l -> ScriptErrorMissingCostModel l
            L.ContextError e ->
                alonzoEraOnwardsConstraints aOnwards $
                    ScriptErrorTranslationError e
