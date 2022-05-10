{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HydraRPS.Contract (
  winnerValidator,
  winnerValidatorHash,
  winnerValidatorAddress,
) where

import Ledger qualified (Address, ScriptContext, ValidatorHash, scriptHashAddress)
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude

type Gesture = ()
type Redeemer = ()

{-# INLINEABLE mkWinnerValidator #-}
mkWinnerValidator :: Gesture -> Redeemer -> Ledger.ScriptContext -> Bool
mkWinnerValidator () () _ = True

data RPS
instance Scripts.ValidatorTypes RPS where
  type RedeemerType RPS = Redeemer
  type DatumType RPS = Gesture

typedWinnerValidator :: Scripts.TypedValidator RPS
typedWinnerValidator =
  Scripts.mkTypedValidator @RPS
    $$(PlutusTx.compile [||mkWinnerValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Gesture @Redeemer

winnerValidator :: Scripts.Validator
winnerValidator = Scripts.validatorScript typedWinnerValidator

winnerValidatorHash :: Ledger.ValidatorHash
winnerValidatorHash = Scripts.validatorHash typedWinnerValidator

winnerValidatorAddress :: Ledger.Address
winnerValidatorAddress = Ledger.scriptHashAddress winnerValidatorHash
