{- HLINT ignore "Use &&" -}
module Onchain.V2.Escrow where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import PlutusLedgerApi.V1 (contains)
import PlutusLedgerApi.V2
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
validator :: EscrowParams -> EscrowInput -> ScriptContext -> Bool
validator params action ctx =
    case action of
        Claim -> validateClaim
        Cancel -> validateCancel
  where
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
validatorUntyped :: EscrowParams -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
validatorUntyped params _ r ctx =
    check
        $ validator
            params
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData ctx)
