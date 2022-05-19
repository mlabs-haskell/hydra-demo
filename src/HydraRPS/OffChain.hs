{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -w #-}

module HydraRPS.OffChain
  ( endpoints
  , PlayParams (..)
  , CollectParams (..)) where

import Prelude
import Control.Lens ((^.))
import Control.Monad (forever, void, foldM)
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import HydraRPS.OnChain

import Ledger qualified as Ledger
import Ledger.Constraints as Constraints
import Plutus.Contract (Contract, Endpoint, type (.\/), Promise)
import Plutus.Contract qualified as Contract
import PlutusTx.Prelude (encodeUtf8, toBuiltin, BuiltinByteString, traceError)
import Ledger.Ada qualified as Ada
import PlutusTx qualified

data PlayParams = PlayParams
  { ppGesture :: Gesture
  , ppSalt :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data CollectParams = CollectParams
  { cpMyInfo :: (Ledger.PubKeyHash, Text)
  , cpTheirInfo :: (Ledger.PubKeyHash, Text)
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type RPSSchema = 
  Endpoint "Play" PlayParams
   .\/ Endpoint "Collect" CollectParams

playEndpoint :: Promise () RPSSchema Text ()
playEndpoint = Contract.endpoint @"Play" play

collectEndpoint :: Promise () RPSSchema Text ()
collectEndpoint = Contract.endpoint @"Collect" collectDraw

endpoints :: Contract () RPSSchema Text ()
endpoints = forever $ Contract.selectList
  [ playEndpoint
  , collectEndpoint
  ]

play :: PlayParams -> Contract () RPSSchema Text ()
play (PlayParams gesture salt) = do
  pkh <- Ledger.unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
  let val = Ada.lovelaceValueOf 20000000
      datum = Ledger.Datum $ PlutusTx.toBuiltinData $ GameDatum (encryptGesture gesture ((encodeUtf8 . toBuiltin) salt)) pkh
      tx = Constraints.mustPayToOtherScript winnerValidatorHash datum val
  void $ Contract.submitTxConstraints @RPS typedWinnerValidator tx

collect :: CollectParams -> Contract () RPSSchema Text ()
collect (CollectParams (p1K, p1S) (p2K, p2S)) = do
  utxos <- Contract.utxosAt winnerValidatorAddress

  let lookups = Constraints.otherScript winnerValidator
                <> Constraints.unspentOutputs utxos
      redeemer = Ledger.Redeemer $ PlutusTx.toBuiltinData $ GameRedeemer (p1K, (encodeUtf8 . toBuiltin) p1S) (p2K, (encodeUtf8 . toBuiltin) p2S)
      tx = mconcat $ (`Constraints.mustSpendScriptOutput` redeemer) <$> Map.keys utxos

  void $ Contract.submitTxConstraintsWith @RPS lookups tx

collectDraw :: CollectParams -> Contract () RPSSchema Text ()
collectDraw (CollectParams (myPk, myS) (theirPk, theirS)) = do
  utxos <- Contract.utxosAt winnerValidatorAddress
  filteredUtxos <- filterUtxos utxos

  case filteredUtxos of
    [(t1, d1), (t2, d2)] -> do
      let lookups = Constraints.otherScript winnerValidator
                  <> Constraints.unspentOutputs utxos
          myEncodedSalt = encodeUtf8 . toBuiltin $ myS
          theirEncodedSalt = encodeUtf8 . toBuiltin $ theirS
          redeemer = Ledger.Redeemer $ PlutusTx.toBuiltinData $ GameRedeemer (myPk, myEncodedSalt) (theirPk, theirEncodedSalt)
          extraConstraints = if (toGesture (gdGesture d2) theirEncodedSalt) == (toGesture (gdGesture d1) myEncodedSalt) then [Constraints.mustPayToPubKey (Ledger.PaymentPubKeyHash theirPk) (t2 ^. Ledger.ciTxOutValue)] else []
          tx = mconcat $ ((`Constraints.mustSpendScriptOutput` redeemer) <$> (Map.keys utxos)) <> extraConstraints
      Contract.logError @Text $ pack . show $ (toGesture (gdGesture d2) theirEncodedSalt) `beats` (toGesture (gdGesture d1) myEncodedSalt)
      void $ Contract.submitTxConstraintsWith @RPS lookups tx
    _ -> do
      Contract.logDebug @Text "Lost, not claiming"
      pure ()

  where
    filterUtxos :: Contract.AsContractError e => Map.Map Ledger.TxOutRef Ledger.ChainIndexTxOut -> Contract w s e [(Ledger.ChainIndexTxOut, GameDatum)]
    filterUtxos utxos = foldM (\acc (txOutRef, ciTxOut) -> do
      mbtxDatm <- resolveTxOutDatum $ Ledger._ciTxOutDatum ciTxOut
      case mbtxDatm of
        Nothing -> pure acc
        Just (Ledger.Datum datum) -> case (PlutusTx.fromBuiltinData datum) of
          Nothing -> pure acc
          Just gd@(GameDatum _ pk) -> pure $ if pk == myPk then (ciTxOut, gd):acc else if pk == theirPk then acc ++ [(ciTxOut, gd)] else acc
      ) [] (Map.toList utxos)

    resolveTxOutDatum :: Contract.AsContractError e => Either Ledger.DatumHash Ledger.Datum -> Contract w s e (Maybe Ledger.Datum)
    resolveTxOutDatum = either Contract.datumFromHash (pure . Just)
    
    toGesture :: BuiltinByteString -> BuiltinByteString -> Gesture
    toGesture bbs salt
      | encryptGesture Rock salt == bbs = Rock
      | encryptGesture Paper salt == bbs = Paper
      | encryptGesture Scissors salt == bbs = Scissors
      | otherwise = traceError "gesture"