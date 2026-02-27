module PSR.Evaluation where

import Cardano.Api qualified as C
import Cardano.Ledger.Alonzo.Plutus.Evaluate (TransactionScriptFailure (..))
import Cardano.Ledger.Binary (decodeScriptContextFromData, mkVersion64)
import Cardano.Ledger.Plutus (
    ExUnits,
    LegacyPlutusArgs (..),
    PlutusArgs (..),
    PlutusLanguage,
    PlutusRunnable (..),
    PlutusWithContext (..),
    SLanguage (..),
    isLanguage,
    withSLanguage,
 )
import Cardano.Ledger.Plutus.Language qualified as Ledger
import Data.Coerce (coerce)
import Data.Text (Text)
import PSR.ConfigMap
import PSR.Evaluation.Ledger (evalPwcExUnitsWithLogs)
import PSR.Events.Interface
import PlutusLedgerApi.Common qualified as Plutus

tryRunScriptInContext :: (MonadFail m) => ResolvedScript -> ExecutionContext -> m (PlutusWithContext, ExUnits, [Text], Maybe Plutus.EvaluationError)
tryRunScriptInContext ResolvedScript{rsScriptForEvaluation} ExecutionContext{..} = do
    pwcProtocolVersion <- mkVersion64 . fromIntegral . Plutus.getMajorProtocolVersion $ majorProtocolVersion
    let
        withLang = withSLanguage $ case ledgerLanguage of
            Plutus.PlutusV1 -> Ledger.PlutusV1
            Plutus.PlutusV2 -> Ledger.PlutusV2
            Plutus.PlutusV3 -> Ledger.PlutusV3

    withLang $ \(_ :: SLanguage l) -> do
        pwcArgs <- buildPlutusArgs @l (scriptContext, datum, redeemer)
        let
            pwc =
                PlutusWithContext
                    { pwcProtocolVersion
                    , pwcScript = Right $ PlutusRunnable rsScriptForEvaluation
                    , pwcArgs
                    , pwcExUnits = exMaxBudget
                    , pwcCostModel = costModel
                    , pwcScriptHash = coerce targetScript.hash
                    }

        case evalPwcExUnitsWithLogs pwc exMaxBudget of
            Right (pwc', logs, exUnits) -> pure (pwc', exUnits, logs, Nothing)
            Left (ValidationFailure exUnits evalErr logs pwc') -> pure (pwc', exUnits, logs, Just evalErr)
            _err -> fail "The script evaluation has failed"

buildPlutusArgs :: forall l m. (PlutusLanguage l, MonadFail m) => (Plutus.Data, Maybe Plutus.Data, Maybe Plutus.Data) -> m (PlutusArgs l)
buildPlutusArgs (c, mDatum, mRedeemer) =
    case isLanguage @l of
        SPlutusV1 -> case (mDatum, mRedeemer) of
            (Nothing, Just r) -> PlutusV1Args . LegacyPlutusArgs2 r <$> decodeScriptContextFromData c
            (Just d, Just r) -> PlutusV1Args . LegacyPlutusArgs3 d r <$> decodeScriptContextFromData c
            _ -> fail "Failed to build PlutusV1Args"
        SPlutusV2 -> case (mDatum, mRedeemer) of
            (Nothing, Just r) -> PlutusV2Args . LegacyPlutusArgs2 r <$> decodeScriptContextFromData c
            (Just d, Just r) -> PlutusV2Args . LegacyPlutusArgs3 d r <$> decodeScriptContextFromData c
            _ -> fail "Failed to build PlutusV2Args"
        SPlutusV3 -> PlutusV3Args <$> decodeScriptContextFromData c
        SPlutusV4 -> PlutusV4Args <$> decodeScriptContextFromData c
