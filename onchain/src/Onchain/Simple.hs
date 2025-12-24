module Onchain.Simple where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import PlutusTx
import PlutusTx.Prelude

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype CompiledCodeLang lang a = CompiledCodeLang (CompiledCode a)

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

{-# INLINEABLE alwaysTrue #-}
alwaysTrue :: BuiltinData -> BuiltinUnit
alwaysTrue _ = trace "Script in debug mode" $ check True
