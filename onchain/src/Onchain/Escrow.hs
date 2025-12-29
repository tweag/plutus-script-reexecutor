module Onchain.Escrow where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import PlutusLedgerApi.V1 (contains)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.List
import PlutusTx.Prelude

--------------------------------------------------------------------------------
-- Scripts
--------------------------------------------------------------------------------

data EscrowParams = EscrowParams
    { depositor :: PubKeyHash
    , beneficiary :: PubKeyHash
    , deadline :: POSIXTime
    }
PlutusTx.makeLift ''EscrowParams
PlutusTx.unstableMakeIsData ''EscrowParams

data EscrowInput = Claim | Cancel
PlutusTx.unstableMakeIsData ''EscrowInput

{-# INLINEABLE validator #-}
validator :: EscrowParams -> ScriptContext -> Bool
validator params ctx =
    case action of
        Claim -> validateClaim
        Cancel -> validateCancel
  where
    action :: EscrowInput
    action = unsafeFromBuiltinData (getRedeemer $ scriptContextRedeemer ctx)

    elseTrace = flip traceIfFalse

    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBy :: PubKeyHash -> Bool
    signedBy pkh = pkh `elem` txInfoSignatories info

    beforeDeadline =
        to (deadline params) `contains` txInfoValidRange info

    afterDeadline =
        from (deadline params) `contains` txInfoValidRange info

    validateClaim =
        and
            [ signedBy (beneficiary params)
                `elseTrace` "beneficiary signature missing"
            , afterDeadline `elseTrace` "too early"
            ]

    validateCancel =
        and
            [ signedBy (depositor params)
                `elseTrace` "depositor signature missing"
            , beforeDeadline `elseTrace` "deadline passed"
            ]

{-# INLINEABLE validatorUntyped #-}
validatorUntyped :: EscrowParams -> BuiltinData -> BuiltinUnit
validatorUntyped params ctx =
    check $ validator params (unsafeFromBuiltinData ctx)
