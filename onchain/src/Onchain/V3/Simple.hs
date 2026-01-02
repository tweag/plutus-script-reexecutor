{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Onchain.V3.Simple where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import PlutusTx.Prelude

import PlutusLedgerApi.V3 qualified as V3
import PlutusTx qualified
import PlutusTx.Builtins (unsafeDataAsI)
import PlutusTx.Builtins.Internal qualified as BI (
    BuiltinList,
    head,
    snd,
    tail,
    unitval,
    unsafeDataAsConstr,
 )

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

{-# INLINEABLE traceScriptPurpose #-}
traceScriptPurpose :: V3.ScriptContext -> Bool
traceScriptPurpose (V3.ScriptContext _ _ scriptInfo) = trace "HERE" True

-- case scriptInfo of
--    V3.MintingScript _ -> trace "Minting" True
--    _ -> trace "Other" True

-- {-# INLINEABLE validator #-}
-- validator :: BuiltinData -> BuiltinUnit
-- validator ctx =
--    -- check $ traceScriptPurpose scriptContext
--    BI.unsafeDataAsConstr ctx

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinUnit
mkValidator arg =
    if red_n < 1000000
        then traceError "redeemer is < 1000000" -- large number ensures same bitsize for all counter values
        else loop red_n
  where
    -- lazily decode script context up to redeemer, which is less expensive and results in much smaller tx size
    constrArgs :: BuiltinData -> BI.BuiltinList BuiltinData
    constrArgs = BI.snd . BI.unsafeDataAsConstr

    redeemerFollowedByScriptInfo :: BI.BuiltinList BuiltinData
    redeemerFollowedByScriptInfo = BI.tail (constrArgs arg)

    redeemer :: BuiltinData
    redeemer = BI.head redeemerFollowedByScriptInfo

    red_n = unsafeDataAsI redeemer

    loop i = if i == 1000000 then BI.unitval else loop (pred i)

--  where
--    scriptContext :: V3.ScriptContext
--    scriptContext = PlutusTx.unsafeFromBuiltinData ctx

-- (unsafeFromBuiltinData sCtx)

compiledValidator :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
compiledValidator = $$(PlutusTx.compile [||mkValidator||])
