#ifdef REMOVE_TRACE
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
#endif
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module MODULE_NAME where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Onchain.V2.Simple (CompiledCodeLang (..))

import Onchain.V3.Simple qualified as Simple
import PlutusTx
import PlutusTx.Prelude
import Cardano.Api

--------------------------------------------------------------------------------
-- Compiled
--------------------------------------------------------------------------------

tracingScript :: CompiledCodeLang PlutusScriptV3 (BuiltinData -> BuiltinUnit)
tracingScript = CompiledCodeLang $$(PlutusTx.compile [|| Simple.script ||])
