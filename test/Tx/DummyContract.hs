{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Tx.DummyContract (
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

data DummyValidator
instance Scripts.ValidatorTypes DummyValidator where
  type RedeemerType DummyValidator = Redeemer
  type DatumType DummyValidator = Gesture

typedDummyValidator :: Scripts.TypedValidator DummyValidator
typedDummyValidator =
  Scripts.mkTypedValidator @DummyValidator
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
