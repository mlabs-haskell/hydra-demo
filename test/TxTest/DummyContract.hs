{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TxTest.DummyContract (
  dummyValidator,
  dummyValidatorHash,
  dummyValidatorAddress,
) where

import Ledger qualified (Address, ScriptContext, ValidatorHash, scriptHashAddress)
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude

type Gesture = ()
type Redeemer = ()

{-# INLINEABLE mkDummyValidator #-}
mkDummyValidator :: Gesture -> Redeemer -> Ledger.ScriptContext -> Bool
mkDummyValidator () () _ = True

data RPS
instance Scripts.ValidatorTypes RPS where
  type RedeemerType RPS = Redeemer
  type DatumType RPS = Gesture

typedDummyValidator :: Scripts.TypedValidator RPS
typedDummyValidator =
  Scripts.mkTypedValidator @RPS
    $$(PlutusTx.compile [||mkDummyValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Gesture @Redeemer

dummyValidator :: Scripts.Validator
dummyValidator = Scripts.validatorScript typedDummyValidator

dummyValidatorHash :: Ledger.ValidatorHash
dummyValidatorHash = Scripts.validatorHash typedDummyValidator

dummyValidatorAddress :: Ledger.Address
dummyValidatorAddress = Ledger.scriptHashAddress dummyValidatorHash
