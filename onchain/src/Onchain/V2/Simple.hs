{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Onchain.V2.Simple where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import PlutusLedgerApi.V2
import PlutusTx
import PlutusTx.Prelude

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype CompiledCodeLang lang a = CompiledCodeLang (CompiledCode a)

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

{-# INLINEABLE traceScriptPurpose #-}
traceScriptPurpose :: ScriptContext -> Bool
traceScriptPurpose sCtx =
    case scriptContextPurpose sCtx of
        Minting _ -> trace "Minting" True
        Spending _ -> trace "Spending" True
        Rewarding _ -> trace "Rewarding" True
        Certifying _ -> trace "Certifying" True

{-# INLINEABLE policy #-}
policy :: BuiltinData -> BuiltinData -> BuiltinUnit
policy _ sCtx = check $ traceScriptPurpose (unsafeFromBuiltinData sCtx)

{-# INLINEABLE validator #-}
validator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
validator _ _ sCtx = check $ traceScriptPurpose (unsafeFromBuiltinData sCtx)
