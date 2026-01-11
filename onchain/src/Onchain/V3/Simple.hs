module Onchain.V3.Simple where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import PlutusLedgerApi.V3
import PlutusTx.Prelude

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

{-# INLINEABLE traceScriptPurpose #-}
traceScriptPurpose :: ScriptContext -> Bool
traceScriptPurpose sCtx =
    case scriptContextScriptInfo sCtx of
        MintingScript _ -> trace "MintingScript" True
        SpendingScript _ _ -> trace "SpendingScript" True
        RewardingScript _ -> trace "RewardingScript" True
        CertifyingScript _ _ -> trace "CertifyingScript" True
        VotingScript _ -> trace "VotingScript" True
        ProposingScript _ _ -> trace "ProposingScript" True

{-# INLINEABLE script #-}
script :: BuiltinData -> BuiltinUnit
script sCtx = check $ traceScriptPurpose (unsafeFromBuiltinData sCtx)
