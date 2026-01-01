#ifdef REMOVE_TRACE
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
#endif

module MODULE_NAME where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Onchain.Simple (CompiledCodeLang (..))
import Onchain.Simple qualified as Simple
import Onchain.Escrow qualified as Escrow
import PlutusTx
import PlutusTx.Prelude
import Cardano.Api
import PlutusCore.Version (plcVersion100)

--------------------------------------------------------------------------------
-- Compiled
--------------------------------------------------------------------------------

type PolicyV2 = BuiltinData -> BuiltinData -> BuiltinUnit
type ValidatorV2 = BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
type CompiledCodePolicyV2 = CompiledCodeLang PlutusScriptV2 PolicyV2
type CompiledCodeValidatorV2 = CompiledCodeLang PlutusScriptV2 ValidatorV2

tracingPolicy :: CompiledCodePolicyV2
tracingPolicy = CompiledCodeLang $$(PlutusTx.compile [|| Simple.policy ||])

tracingValidator :: CompiledCodeValidatorV2
tracingValidator = CompiledCodeLang $$(PlutusTx.compile [|| Simple.validator ||])

escrowValidator :: Escrow.EscrowParams -> CompiledCodeValidatorV2
escrowValidator params =
    CompiledCodeLang
        ($$(PlutusTx.compile [|| Escrow.validatorUntyped ||]) `unsafeApplyCode` liftCode plcVersion100 params)
