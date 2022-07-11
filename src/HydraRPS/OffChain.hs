{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

{-# LANGUAGE DeriveAnyClass #-}

module HydraRPS.OffChain (
  endpoints,
  PlayParams (..),
) where

import Control.Lens ((^.))
import Control.Monad (foldM, forever, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude

import HydraRPS.OnChain

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints as Constraints
import Plutus.Contract (Contract, Endpoint, Promise, type (.\/))
import Plutus.Contract qualified as Contract
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinByteString)

data PlayParams = PlayParams
  { ppGesture :: Gesture
  , ppSalt :: BuiltinByteString
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type RPSSchema =
  Endpoint "Play" PlayParams
    .\/ Endpoint "Collect" GameRedeemer

playEndpoint :: Promise () RPSSchema Text ()
playEndpoint = Contract.endpoint @"Play" play

collectEndpoint :: Promise () RPSSchema Text ()
collectEndpoint = Contract.endpoint @"Collect" collect

endpoints :: Contract () RPSSchema Text ()
endpoints =
  forever $
    Contract.selectList
      [ playEndpoint
      , collectEndpoint
      ]

play :: PlayParams -> Contract () RPSSchema Text ()
play (PlayParams gesture salt) = do
  pkh <- Ledger.unPaymentPubKeyHash <$> Contract.ownPaymentPubKeyHash
  let val = Ada.lovelaceValueOf 20000000
      datum = Ledger.Datum $ PlutusTx.toBuiltinData $ GameDatum (encryptGesture gesture salt) pkh
      tx = Constraints.mustPayToOtherScript rpsValidatorHash datum val
  void $ Contract.submitTxConstraints @RPS typedRPSValidator tx

collect :: GameRedeemer -> Contract () RPSSchema Text ()
collect (GameRedeemer (myPk, mySalt) (theirPk, theirSalt)) = do
  utxos <- Contract.utxosAt rpsValidatorAddress
  filteredUtxos <- filterUtxos utxos

  case filteredUtxos of
    [(_, d1), ((_, ciT2), d2)] -> do
      let lookups =
            Constraints.otherScript rpsValidator
              <> Constraints.unspentOutputs (Map.fromList $ fst <$> filteredUtxos)
          redeemer = Ledger.Redeemer $ PlutusTx.toBuiltinData $ GameRedeemer (myPk, mySalt) (theirPk, theirSalt)
          myGesture = toGesture (gdGesture d1) mySalt
          theirGesture = toGesture (gdGesture d2) theirSalt
          extraConstraints = [Constraints.mustPayToPubKey (Ledger.PaymentPubKeyHash theirPk) (ciT2 ^. Ledger.ciTxOutValue) | myGesture == theirGesture]

          tx = mconcat $ ((`Constraints.mustSpendScriptOutput` redeemer) <$> (fst . fst <$> filteredUtxos)) <> extraConstraints

      void $ Contract.submitTxConstraintsWith @RPS lookups tx
    _ -> do
      Contract.logDebug @Text "Expected to find pair of utxos, bailing."
      pure ()
  where
    filterUtxos :: Contract.AsContractError e => Map.Map Ledger.TxOutRef Ledger.ChainIndexTxOut -> Contract w s e [(UTxO, GameDatum)]
    filterUtxos utxos =
      foldM
        ( \acc (tOutRef, ciTxOut) -> do
            mbtxDatm <- resolveTxOutDatum $ Ledger._ciTxOutDatum ciTxOut
            case mbtxDatm of
              Nothing -> pure acc
              Just (Ledger.Datum datum) -> case PlutusTx.fromBuiltinData datum of
                Nothing -> pure acc
                Just gd@(GameDatum _ pk) -> pure $ if pk == myPk then ((tOutRef, ciTxOut), gd) : acc else if pk == theirPk then acc ++ [((tOutRef, ciTxOut), gd)] else acc
        )
        []
        (Map.toList utxos)

    resolveTxOutDatum :: Contract.AsContractError e => Either Ledger.DatumHash Ledger.Datum -> Contract w s e (Maybe Ledger.Datum)
    resolveTxOutDatum = either Contract.datumFromHash (pure . Just)

type UTxO = (Ledger.TxOutRef, Ledger.ChainIndexTxOut)
