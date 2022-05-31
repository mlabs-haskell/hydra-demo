{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module HydraRPS.Node.Command (
    Command (
      Init,
      Abort,
      Commit,
      GetUTxO,
      Close,
      Contest,
      Fanout
    ),
    newTx
) where

import Cardano.Api (AlonzoEra, Tx, UTxO, serialiseToCBOR)
import Data.Aeson (ToJSON, toJSON)
import Data.Int (Int)
import GHC.Generics (Generic)

data Command
  = Init {contestationPeriod :: !Int}
  | Abort
  | Commit {utxo :: !(UTxO AlonzoEra)}
  | NewTx {transaction :: !TxCBOR}
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newtype TxCBOR = TxCBOR (Tx AlonzoEra)

instance ToJSON TxCBOR where
  toJSON (TxCBOR tx) = toJSON (serialiseToCBOR tx)

newTx :: Tx AlonzoEra -> Command
newTx tx = NewTx (TxCBOR tx)
