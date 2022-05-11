{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HydraRPS.Contract (
  winnerValidator,
  winnerValidatorHash,
  winnerValidatorAddress,
) where

import GHC.Generics (Generic)
import Ledger (Address, PubKeyHash, ScriptContext (..), TxInInfo (..), TxInfo (..), TxOut (..), ValidatorHash, findDatum, findOwnInput, scriptHashAddress, toPubKeyHash)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (fromBuiltinData, getDatum)
import PlutusTx qualified

import PlutusTx.Prelude

data Gesture = Rock | Paper | Scissors
  deriving stock (Generic)
PlutusTx.unstableMakeIsData ''Gesture

{-# INLINEABLE beats #-}
beats :: Gesture -> Gesture -> Bool
beats Rock Paper = True
beats Paper Scissors = True
beats Scissors Rock = True
beats _ _ = False

data GameParams = GameParams
  { gpGesture :: Gesture
  , gpSalt :: BuiltinByteString
  , gpPkh :: PubKeyHash
  }
  deriving stock (Generic)

data GameDatum = GameDatum
  { gdGesture :: BuiltinByteString
  , gdPkh :: PubKeyHash
  }
PlutusTx.unstableMakeIsData ''GameDatum

type RedeemInfo = (PubKeyHash, BuiltinByteString)

data GameRedeemer = GameRedeemer
  { p1 :: RedeemInfo
  , p2 :: RedeemInfo
  }
PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINEABLE mkWinnerValidator #-}
mkWinnerValidator :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkWinnerValidator (GameDatum _ pkh) (GameRedeemer p1rInfo p2rInfo) ctx =
  isClaimingP1Token && paysCorrectly (claimingPlay, p1rInfo) (otherPlay, p2rInfo)
    || isClaimingP2Token && paysCorrectly (claimingPlay, p2rInfo) (otherPlay, p1rInfo)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txIns :: [TxInInfo]
    txIns = txInfoInputs info

    otherPlay :: TxOut
    otherPlay = txInInfoResolved (head txIns)

    claimingPlay :: TxOut
    claimingPlay = case findOwnInput ctx of
      Nothing -> error ()
      Just t -> txInInfoResolved t

    toGameDatum :: TxOut -> GameDatum
    toGameDatum tOut = case txOutDatumHash tOut of
      Nothing -> error ()
      Just dh -> case findDatum dh info of
        Nothing -> error ()
        Just dt -> case fromBuiltinData (getDatum dt) of
          Nothing -> error ()
          Just gd -> gd

    isClaimingP1Token :: Bool
    isClaimingP1Token = fst p1rInfo == pkh

    isClaimingP2Token :: Bool
    isClaimingP2Token = fst p2rInfo == pkh

    toGesture :: BuiltinByteString -> BuiltinByteString -> Gesture
    toGesture bbs salt
      | sha2_256 ("rock" <> salt) == bbs = Rock
      | sha2_256 ("paper" <> salt) == bbs = Paper
      | sha2_256 ("scissors" <> salt) == bbs = Scissors
      | otherwise = error ()

    paysToPubKey :: TxOut -> PubKeyHash -> Bool
    paysToPubKey tOut pk = case toPubKeyHash (txOutAddress tOut) of
      Nothing -> False
      Just pk' -> pk == pk'

    paysCorrectly :: (TxOut, RedeemInfo) -> (TxOut, RedeemInfo) -> Bool
    paysCorrectly (tOut1, (pk1, s1)) (tOut2, (pk2, s2))
      | beats g1 g2 = paysToPubKey tOut1 pk1 && paysToPubKey tOut2 pk1
      | beats g2 g1 = paysToPubKey tOut1 pk2 && paysToPubKey tOut2 pk2
      | otherwise = paysToPubKey tOut1 pk1 && paysToPubKey tOut2 pk2
      where
        gd1 = toGameDatum tOut1
        g1 = toGesture (gdGesture gd1) s1
        gd2 = toGameDatum tOut2
        g2 = toGesture (gdGesture gd2) s2

data RPS
instance Scripts.ValidatorTypes RPS where
  type RedeemerType RPS = GameRedeemer
  type DatumType RPS = GameDatum

typedWinnerValidator :: Scripts.TypedValidator RPS
typedWinnerValidator =
  Scripts.mkTypedValidator @RPS
    $$(PlutusTx.compile [||mkWinnerValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

winnerValidator :: Scripts.Validator
winnerValidator = Scripts.validatorScript typedWinnerValidator

winnerValidatorHash :: ValidatorHash
winnerValidatorHash = Scripts.validatorHash typedWinnerValidator

winnerValidatorAddress :: Address
winnerValidatorAddress = Ledger.scriptHashAddress winnerValidatorHash
