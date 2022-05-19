{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module HydraRPS.OnChain (
  winnerValidator,
  winnerValidatorHash,
  winnerValidatorAddress,
  typedWinnerValidator,
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
  deriving stock (Generic, Prelude.Eq)
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

{-# INLINEABLE mkWinnerValidator #-}
mkWinnerValidator :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkWinnerValidator datum (GameRedeemer myInfo theirInfo) ctx
  | isClaimingMyToken = case otherPlay (fst theirInfo) of
    Nothing -> traceError "should contain other tk matching theirInfo"
    Just tOut -> paysCorrectly (claimingPlay, myInfo) (tOut, theirInfo)
  | isClaimingTheirToken = case otherPlay (fst myInfo) of
    Nothing -> traceError "should contain other tk matching myInfo"
    Just tOut -> paysCorrectly (tOut, myInfo) (claimingPlay, theirInfo)
  | otherwise = traceError "no good claim"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txIns :: [TxInInfo]
    txIns = txInfoInputs info

    otherPlay :: PubKeyHash -> Maybe TxOut
    otherPlay pk =
      txInInfoResolved
        <$> ( find $ \tIn ->
                case toGameDatum (txInInfoResolved tIn) of
                  Nothing -> False
                  Just gd -> matchesInfo gd pk
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
    isClaimingMyToken = matchesInfo datum (fst myInfo)

    isClaimingTheirToken :: Bool
    isClaimingTheirToken = matchesInfo datum (fst theirInfo)

    -- fromMaybe is strict in plutus :(
    fromJust :: Maybe a -> a
    fromJust a = case a of
      Nothing -> traceError "fromJust"
      Just b -> b

    fee :: Value
    fee = txInfoFee info

    paysCorrectly :: (TxOut, RedeemInfo) -> (TxOut, RedeemInfo) -> Bool
    paysCorrectly (myTOut, (myPk, mySalt)) (theirTOut, (theirPk, theirSalt))
      | beats myGesture theirGesture = traceIfFalse "shouldPay all to myKey" $ valuePaidTo info myPk `geq` ((txOutValue myTOut <> txOutValue theirTOut) - fee)
      | beats theirGesture myGesture = traceIfFalse "shouldPay all to theirKey" $ valuePaidTo info theirPk `geq` ((txOutValue myTOut <> txOutValue theirTOut) - fee)
      | otherwise = traceIfFalse "should pay ada back" $ valuePaidTo info myPk `geq` (txOutValue myTOut - fee) && valuePaidTo info theirPk `geq` (txOutValue theirTOut - fee)
      where
        myDatum = fromJust (toGameDatum myTOut)
        myGesture = toGesture (gdGesture myDatum) mySalt
        theirDatum = fromJust (toGameDatum theirTOut)
        theirGesture = toGesture (gdGesture theirDatum) theirSalt

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
