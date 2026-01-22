module Onchain.V3.Simple where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Onchain.V3.ShowBS
import PlutusLedgerApi.V3
import PlutusTx.Prelude

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

{-# INLINEABLE traceFullContext #-}
traceFullContext :: ScriptContext -> Bool
traceFullContext sCtx = trace (showBS sCtx) True

{-# INLINEABLE script #-}
script :: BuiltinData -> BuiltinUnit
script sCtx = check $ traceFullContext (unsafeFromBuiltinData sCtx)
