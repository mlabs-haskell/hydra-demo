{-# OPTIONS_GHC -w #-}
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
  encryptGesture
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Ledger (Address, PubKeyHash, ScriptContext (..), TxInInfo (..), TxInfo (..), TxOut (..), ValidatorHash, findDatum, findOwnInput, scriptHashAddress, toPubKeyHash, unPaymentPubKeyHash, valuePaidTo)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (fromBuiltinData, getDatum, Datum (..), Redeemer (..))
import Data.Text (Text)
import Plutus.V1.Ledger.Value (geq, Value)
import Plutus.V1.Ledger.Ada (fromValue, Ada, lovelaceValueOf)

import PlutusTx qualified
import Prelude qualified
import PlutusTx.Prelude

data Gesture = Rock | Paper | Scissors
  deriving stock (Generic, Prelude.Eq)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.unstableMakeIsData ''Gesture

{-# INLINEABLE beats #-}
beats :: Gesture -> Gesture -> Bool
beats Paper    Rock     = True
beats Rock     Scissors = True
beats Scissors Paper    = True
beats _        _        = False

data GameDatum = GameDatum
  { gdGesture :: BuiltinByteString
  , gdPkh :: PubKeyHash
  }
PlutusTx.unstableMakeIsData ''GameDatum

type RedeemInfo = (PubKeyHash, BuiltinByteString)

data GameRedeemer = GameRedeemer
  { myInfo :: RedeemInfo
  , theirInfo :: RedeemInfo
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)
PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINEABLE mkWinnerValidator #-}
mkWinnerValidator :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkWinnerValidator datum (GameRedeemer myInfo theirInfo) ctx =
  -- redeemer: GR { myInfo :: RedeemInfo, theirInfo :: RedeemInfo }
  -- you can only redeem a token if:
    -- matches your Pk && tx contains another token matching their info && the token you are redeeming beats their token
    -- matches their pk && tx contains another token matching your info && this other token beats the token you are redeeming
  if isClaimingMyToken then 
    case otherPlay (fst theirInfo) of
      Nothing -> traceError "should contain other tk matching theirInfo"
      Just tOut -> paysCorrectly (claimingPlay, myInfo) (tOut, theirInfo)
  else
    if isClaimingTheirToken then
      case otherPlay (fst myInfo) of
        Nothing -> traceError "should contain other tk matching myInfo"
        Just tOut -> True --paysCorrectly (tOut, myInfo) (claimingPlay, theirInfo)
    else
      traceError "no good claim"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txIns :: [TxInInfo]
    txIns = txInfoInputs info

    otherPlay :: PubKeyHash -> Maybe TxOut
    otherPlay pk = txInInfoResolved <$> (find $ \tIn -> 
      case toGameDatum (txInInfoResolved tIn) of
        Nothing -> False
        Just gd -> matchesInfo gd pk
      ) txIns

    claimingPlay :: TxOut
    claimingPlay = case findOwnInput ctx of
      Nothing -> traceError "no input"
      Just t -> txInInfoResolved t

    toGameDatum :: TxOut -> Maybe GameDatum
    toGameDatum tOut = do
      dh <- txOutDatumHash tOut
      dt <- findDatum dh info
      fromBuiltinData (getDatum dt)

    otherDatum :: PubKeyHash -> GameDatum
    otherDatum pk = case otherPlay pk of
      Nothing -> traceError "couldn't find other play"
      Just p -> case toGameDatum p of
        Nothing -> traceError "could not decode datum"
        Just gd -> gd

    matchesInfo :: GameDatum -> PubKeyHash -> Bool
    matchesInfo (GameDatum _ pkh) pkh' = pkh == pkh'

    isClaimingMyToken :: Bool
    isClaimingMyToken = matchesInfo datum (fst myInfo)

    isClaimingTheirToken :: Bool
    isClaimingTheirToken = matchesInfo datum (fst theirInfo)

    toGesture :: BuiltinByteString -> BuiltinByteString -> Gesture
    toGesture bbs salt
      | encryptGesture Rock salt == bbs = Rock
      | encryptGesture Paper salt == bbs = Paper
      | encryptGesture Scissors salt == bbs = Scissors
      | otherwise = traceError "gesture"

    paysToPubKey :: TxOut -> PubKeyHash -> Bool
    paysToPubKey tOut pk = case toPubKeyHash (txOutAddress tOut) of
      Nothing -> False
      Just pk' -> pk == pk'

    -- fromMaybe is strict in plutus :(
    fromJust :: Maybe a -> a
    fromJust a = case a of
      Nothing -> traceError "fromJust"
      Just a  -> a

    fee :: Value
    fee = (txInfoFee info)

    paysCorrectly :: (TxOut, RedeemInfo) -> (TxOut, RedeemInfo) -> Bool
    paysCorrectly (myTOut, (myPk, mySalt)) (theirTOut, (theirPk, theirSalt))
      | beats myGesture theirGesture = traceIfFalse "shouldPay all to myKey" $ (valuePaidTo info myPk) `geq` ((txOutValue myTOut <> txOutValue theirTOut) - fee)
      | beats theirGesture myGesture = traceIfFalse "shouldPay all to theirKey" $ (valuePaidTo info myPk) `geq` ((txOutValue myTOut <> txOutValue theirTOut) - fee)
      | otherwise = traceIfFalse "should pay ada back" $ valuePaidTo info myPk `geq` (txOutValue myTOut - fee) && valuePaidTo info theirPk `geq` (txOutValue theirTOut - fee)
      where
        myDatum = fromJust (toGameDatum myTOut)
        myGesture = toGesture (gdGesture myDatum) mySalt
        theirDatum = fromJust (toGameDatum theirTOut)
        theirGesture = toGesture (gdGesture theirDatum) theirSalt

{-# INLINEABLE encryptGesture #-}
encryptGesture :: Gesture -> BuiltinByteString -> BuiltinByteString
encryptGesture Rock salt = ("rock" <> salt)
encryptGesture Paper salt = ("paper" <> salt)
encryptGesture Scissors salt = ("scissors" <> salt)

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
