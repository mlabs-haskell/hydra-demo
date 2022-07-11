{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HydraRPS.OnChain (
  rpsValidator,
  rpsValidatorHash,
  rpsValidatorAddress,
  typedRPSValidator,
  Gesture (..),
  GameDatum (..),
  GameRedeemer (..),
  RPS,
  beats,
  encryptGesture,
  toGesture,
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (
  Address,
  PubKeyHash,
  ScriptContext (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  ValidatorHash,
  findDatum,
  findOwnInput,
  scriptHashAddress,
  valuePaidTo,
 )
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (Datum (..), fromBuiltinData, getDatum)
import Plutus.V1.Ledger.Value (Value, geq)

import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified

data Gesture = Rock | Paper | Scissors
  deriving stock (Generic, Prelude.Bounded, Prelude.Enum, Prelude.Eq)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.unstableMakeIsData ''Gesture

{-# INLINEABLE beats #-}
beats :: Gesture -> Gesture -> Bool
beats Paper Rock = True
beats Rock Scissors = True
beats Scissors Paper = True
beats _ _ = False

data GameDatum = GameDatum
  { gdGesture :: BuiltinByteString
  , gdPkh :: PubKeyHash
  }
PlutusTx.unstableMakeIsData ''GameDatum

type RedeemInfo = (PubKeyHash, BuiltinByteString)

data GameRedeemer = GameRedeemer
  { grMyInfo :: RedeemInfo
  , grTheirInfo :: RedeemInfo
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINEABLE mkRPSValidator #-}
mkRPSValidator :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkRPSValidator datum (GameRedeemer (myKey, mySalt) (theirKey, theirSalt)) ctx
  | isClaimingMyToken = case otherPlay theirKey of
    Nothing -> traceError "should contain other input matching their key"
    Just (tOut, theirDatum) ->
      paysCorrectly
        (claimingPlay, toGesture (gdGesture datum) mySalt, myKey)
        (tOut, toGesture (gdGesture theirDatum) theirSalt, theirKey)
        && isDifferentRedeemInfo (myKey, mySalt) (theirKey, theirSalt)
  | isClaimingTheirToken = case otherPlay myKey of
    Nothing -> traceError "should contain other input matching my key"
    Just (tOut, myDatum) ->
      paysCorrectly
        (tOut, toGesture (gdGesture myDatum) mySalt, myKey)
        (claimingPlay, toGesture (gdGesture datum) theirSalt, theirKey)
        && isDifferentRedeemInfo (myKey, mySalt) (theirKey, theirSalt)
  | otherwise = traceError "no good claim"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txIns :: [TxInInfo]
    txIns = txInfoInputs info

    firstJust :: (a -> Maybe b) -> [a] -> Maybe b
    firstJust f (a : as) = case f a of
      Nothing -> firstJust f as
      Just b -> Just b
    firstJust _ [] = Nothing

    otherPlay :: PubKeyHash -> Maybe (TxOut, GameDatum)
    otherPlay pk =
      firstJust
        ( \tIn -> do
            gd <- toGameDatum (txInInfoResolved tIn)
            if matchesInfo gd pk
              then Just (txInInfoResolved tIn, gd)
              else Nothing
        )
        txIns

    claimingPlay :: TxOut
    claimingPlay = case findOwnInput ctx of
      Nothing -> traceError "no input"
      Just t -> txInInfoResolved t

    toGameDatum :: TxOut -> Maybe GameDatum
    toGameDatum tOut = do
      dh <- txOutDatumHash tOut
      dt <- findDatum dh info
      fromBuiltinData (getDatum dt)

    matchesInfo :: GameDatum -> PubKeyHash -> Bool
    matchesInfo (GameDatum _ pkh) pkh' = pkh == pkh'

    isClaimingMyToken :: Bool
    isClaimingMyToken = matchesInfo datum myKey

    isClaimingTheirToken :: Bool
    isClaimingTheirToken = matchesInfo datum theirKey

    isDifferentRedeemInfo :: RedeemInfo -> RedeemInfo -> Bool
    isDifferentRedeemInfo (pkhA, saltA) (pkhB, saltB) =
      (pkhA /= pkhB) && (saltA /= saltB)

    fee :: Value
    fee = txInfoFee info

    paysCorrectly :: (TxOut, Gesture, PubKeyHash) -> (TxOut, Gesture, PubKeyHash) -> Bool
    paysCorrectly (myTOut, myGesture, myPk) (theirTOut, theirGesture, theirPk)
      | beats myGesture theirGesture = traceIfFalse "shouldPay all to myKey" $ valuePaidTo info myPk `geq` ((txOutValue myTOut <> txOutValue theirTOut) - fee)
      | beats theirGesture myGesture = traceIfFalse "shouldPay all to theirKey" $ valuePaidTo info theirPk `geq` ((txOutValue myTOut <> txOutValue theirTOut) - fee)
      | otherwise = traceIfFalse "should pay ada back" $ valuePaidTo info myPk `geq` (txOutValue myTOut - fee) && valuePaidTo info theirPk `geq` (txOutValue theirTOut - fee)

{-# INLINEABLE encryptGesture #-}
encryptGesture :: Gesture -> BuiltinByteString -> BuiltinByteString
encryptGesture Rock salt = sha2_256 ("rock" <> salt)
encryptGesture Paper salt = sha2_256 ("paper" <> salt)
encryptGesture Scissors salt = sha2_256 ("scissors" <> salt)

{-# INLINEABLE toGesture #-}
toGesture :: BuiltinByteString -> BuiltinByteString -> Gesture
toGesture bbs salt
  | encryptGesture Rock salt == bbs = Rock
  | encryptGesture Paper salt == bbs = Paper
  | encryptGesture Scissors salt == bbs = Scissors
  | otherwise = traceError "unable to decode gesture"

data RPS
instance Scripts.ValidatorTypes RPS where
  type RedeemerType RPS = GameRedeemer
  type DatumType RPS = GameDatum

typedRPSValidator :: Scripts.TypedValidator RPS
typedRPSValidator =
  Scripts.mkTypedValidator @RPS
    $$(PlutusTx.compile [||mkRPSValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

rpsValidator :: Scripts.Validator
rpsValidator = Scripts.validatorScript typedRPSValidator

rpsValidatorHash :: ValidatorHash
rpsValidatorHash = Scripts.validatorHash typedRPSValidator

rpsValidatorAddress :: Address
rpsValidatorAddress = Ledger.scriptHashAddress rpsValidatorHash
