{- FOURMOLU_DISABLE -}

{-# LANGUAGE RankNTypes #-}

-- NOTE: The contents of this module are originally copied from
-- Cardano.Ledger.Alonzo.Plutus.Evaluate

module PSR.Evaluation.Ledger (evalTxExUnitsWithLogs, evalPwcExUnitsWithLogs) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- import Cardano.Ledger.Plutus.TxInfo (transExUnits)

import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..), LedgerTxInfo (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (TransactionScriptFailure (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits, lookupPlutusScript, plutusScriptLanguage, toAsIx)
import Cardano.Ledger.Alonzo.TxWits (unRedeemersL)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded (..))
import Cardano.Ledger.Plutus.CostModels (costModelsValid, getEvaluationContext)
import Cardano.Ledger.Plutus.Evaluate (
    PlutusWithContext (..),
 )
import Cardano.Ledger.Plutus.Language (PlutusLanguage (..), PlutusRunnable)
import Cardano.Ledger.Plutus.TxInfo (exBudgetToExUnits)
import Cardano.Ledger.State (EraUTxO (..), ScriptsProvided (..), UTxO (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Data.Bifunctor (Bifunctor (second), first)
import Data.Map.Strict qualified as Map
import Data.MapExtras (fromElems)
import Data.Text (Text)
import Lens.Micro
import PSR.Types (
    PreEvaluationPlutusError (..),
    PwcExecutionResult,
    RedeemerReportWithLogs,
    ScriptSubtitutionInfo,
 )
import PlutusLedgerApi.Common qualified as P

import Cardano.Api qualified as C
import Cardano.Ledger.Conway.Scripts qualified as Conway

--------------------------------------------------------------------------------
-- Evaluators
--------------------------------------------------------------------------------

-- NOTE: There is also debugPlutusUnbounded in
-- cardano-ledger:Cardano.Ledger.Plutus.Evaluate. It may be possible to use it
-- to replace a lot of code from this module.
--
-- TODO: Explore "debugPlutusUnbounded".

note :: e -> Maybe a -> Either e a
note _ (Just x) = Right x
note e Nothing = Left e

withRunnablePlutusWithContext ::
  PlutusWithContext ->
  -- | Handle the decoder failure
  (P.EvaluationError -> a) ->
  (forall l. PlutusLanguage l => PlutusRunnable l -> PlutusArgs l -> a) ->
  a
withRunnablePlutusWithContext PlutusWithContext {pwcProtocolVersion, pwcScript, pwcArgs} onError f =
  case pwcScript of
    Right pr -> f pr pwcArgs
    Left plutus ->
      case decodePlutusRunnable pwcProtocolVersion plutus of
        Right pr -> f pr pwcArgs
        Left err -> onError (P.CodecError err)

evaluatePlutusWithContextBudget ::
  P.VerboseMode ->
  PlutusWithContext ->
  ([Text], Either P.EvaluationError P.ExBudget)
evaluatePlutusWithContextBudget mode pwc@PlutusWithContext {..} =
  withRunnablePlutusWithContext pwc (([],) . Left) $
    -- NOTE: evaluatePlutusRunnableBudget is used for testing primarily and runs
    -- without any budget restrictions.
    evaluatePlutusRunnableBudget
      pwcProtocolVersion
      mode
      (getEvaluationContext pwcCostModel)

evalPwcExUnitsWithLogs ::
    PlutusWithContext ->
    ExUnits ->
    PwcExecutionResult era
evalPwcExUnitsWithLogs pwc exUnits =
    case evaluatePlutusWithContextBudget P.Verbose pwc of
        (logs, Left err) -> Left $ ValidationFailure exUnits err logs pwc
        (logs, Right exBudget) ->
            note (IncompatibleBudget exBudget) $
                (pwc,logs,) <$> exBudgetToExUnits exBudget

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

    findAndCount pointer rdmr = do
        purpHash@(_, plutusScriptHash) <-
            note PeRedeemerPointsToUnknownScriptHash $
                Map.lookup pointer purposeToScriptHash
        {-
        -- NOTE: We don't use this as we don't report this error. If we decide
        -- to update PreEvaluationPlutusError, we need to pass this to
        -- PeMissingScript.
        let ptrToPlutusScriptNoContext =
                Map.map
                    ( \(sp, sh) ->
                        ( hoistPlutusPurpose toAsItem sp
                        , lookupPlutusScript sh scriptsProvided
                        , sh
                        )
                    )
                    purposeToScriptHash
         -}
        scriptsToRun <-
            case Map.lookup (C.ScriptHash plutusScriptHash) ssi of
                Nothing -> do
                    -- NOTE: When there is no shadow script available, we
                    -- can choose to exit. This will simplify the function.
                    providedScript <-
                        note PeMissingScript $
                            lookupPlutusScript plutusScriptHash scriptsProvided
                    pure [(Nothing, C.ScriptHash plutusScriptHash, providedScript)]
                Just vals ->
                    pure . flip map vals $ \case
                        (sname, shadowHash, Conway.PlutusScript s) -> (sname, shadowHash, s)
                        _ -> error "Conway.TimelockScript is not supported."
        pure $ map (second (findAndCountWith purpHash rdmr)) scriptsToRun

    findAndCountWith (plutusPurpose, plutusScriptHash) (redeemerData, exUnits) plutusScript = do
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
        evalPwcExUnitsWithLogs pwc exUnits
