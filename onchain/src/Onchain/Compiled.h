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

--------------------------------------------------------------------------------
-- Compiled
--------------------------------------------------------------------------------

alwaysTrue :: CompiledCodeLang PlutusScriptV3 (BuiltinData -> BuiltinUnit)
alwaysTrue = CompiledCodeLang $$(PlutusTx.compile [|| Simple.alwaysTrue ||])

escrowValidator ::
    Escrow.EscrowParams ->
    CompiledCodeLang PlutusScriptV2 (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
escrowValidator params =
    CompiledCodeLang
        ($$(PlutusTx.compile [|| Escrow.validatorUntyped ||]) `unsafeApplyCode` liftCodeDef params)
