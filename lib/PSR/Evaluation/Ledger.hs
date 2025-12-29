-- NOTE: The contents of this module are originally copied from
-- Cardano.Ledger.Alonzo.Plutus.Evaluate

module PSR.Evaluation.Ledger (evalTxExUnitsWithLogs) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..), LedgerTxInfo (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (TransactionScriptFailure (..))
import Cardano.Ledger.Alonzo.Scripts (lookupPlutusScript, plutusScriptLanguage, toAsItem, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Plutus.CostModels (costModelsValid)
import Cardano.Ledger.Plutus.Evaluate (
    evaluatePlutusWithContext,
 )
import Cardano.Ledger.Plutus.TxInfo (exBudgetToExUnits)
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Data.MapExtras (fromElems)
import Data.Text (Text)
import Lens.Micro
import PSR.Types (RedeemerReportWithLogs, ScriptSubtitutionInfo)
import PlutusLedgerApi.Common qualified as P

import Cardano.Api qualified as C
import Cardano.Ledger.Conway.Scripts qualified as Conway

--------------------------------------------------------------------------------
-- Evaluators
--------------------------------------------------------------------------------

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

evalTxExUnitsWithLogs ::
    forall era.
    ( AlonzoEraTx era
    , EraUTxO era
    , EraPlutusContext era
    , ScriptsNeeded era ~ AlonzoScriptsNeeded era
    ) =>
    ScriptSubtitutionInfo era ->
    PParams era ->
    -- | The transaction.
    Tx era ->
    -- | The current UTxO set (or the relevant portion for the transaction).
    UTxO era ->
    -- | The epoch info, used to translate slots to POSIX time for plutus.
    EpochInfo (Either Text) ->
    -- | The start time of the given block chain.
    SystemStart ->
    -- | We return a map from redeemer pointers to either a failure or a sufficient
    --     execution budget with logs of the script.  Otherwise, we return a 'TranslationError'
    --     manifesting from failed attempts to construct a valid execution context for the
    --     given transaction.
    --
    --     Unlike `evalTxExUnits`, this function also returns evaluation logs, useful for
    --     debugging.
    RedeemerReportWithLogs era
evalTxExUnitsWithLogs ssi pp tx utxo epochInfo systemStart = Map.mapWithKey findAndCount rdmrs
  where
    keyedByPurpose (plutusPurpose, _) = hoistPlutusPurpose toAsIx plutusPurpose
    purposeToScriptHash = fromElems keyedByPurpose scriptsNeeded
    ledgerTxInfo =
        LedgerTxInfo
            { ltiProtVer = protVer
            , ltiEpochInfo = epochInfo
            , ltiSystemStart = systemStart
            , ltiUTxO = utxo
            , ltiTx = tx
            }
    txInfoResult = mkTxInfoResult ledgerTxInfo
    maxBudget = pp ^. ppMaxTxExUnitsL
    txBody = tx ^. bodyTxL
    wits = tx ^. witsTxL
    rdmrs = wits ^. rdmrsTxWitsL . unRedeemersL
    protVer = pp ^. ppProtocolVersionL
    costModels = costModelsValid $ pp ^. ppCostModelsL
    ScriptsProvided scriptsProvided = getScriptsProvided utxo tx
    AlonzoScriptsNeeded scriptsNeeded = getScriptsNeeded utxo txBody
    findAndCount pointer (redeemerData, exUnits) = do
        (plutusPurpose, plutusScriptHash) <-
            note (RedeemerPointsToUnknownScriptHash pointer) $
                Map.lookup pointer purposeToScriptHash
        let ptrToPlutusScriptNoContext =
                Map.map
                    ( \(sp, sh) ->
                        ( hoistPlutusPurpose toAsItem sp
                        , lookupPlutusScript sh scriptsProvided
                        , sh
                        )
                    )
                    purposeToScriptHash
        let lookupPlutusScriptSubstituting psh =
                case Map.lookup (C.ScriptHash plutusScriptHash) ssi of
                    Nothing ->
                        note (MissingScript pointer ptrToPlutusScriptNoContext) $
                            lookupPlutusScript psh scriptsProvided
                    Just val ->
                        case val of
                            Conway.PlutusScript s -> pure s
                            _ -> error "Conway.NativeScript is not supported."
        plutusScript <- lookupPlutusScriptSubstituting plutusScriptHash
        let lang = plutusScriptLanguage plutusScript
        costModel <-
            note (NoCostModelInLedgerState lang) $ Map.lookup lang costModels
        pwc <-
            first ContextError $
                mkPlutusWithContext
                    plutusScript
                    plutusScriptHash
                    plutusPurpose
                    ledgerTxInfo
                    txInfoResult
                    (redeemerData, maxBudget)
                    costModel
        case evaluatePlutusWithContext P.Verbose pwc of
            (logs, Left err) -> Left $ ValidationFailure exUnits err logs pwc
            (logs, Right exBudget) ->
                note (IncompatibleBudget exBudget) $
                    (pwc,logs,) <$> exBudgetToExUnits exBudget
