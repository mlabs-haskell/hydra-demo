{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

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
  newTx,
) where

import Cardano.Api (Tx, UTxO, serialiseToCBOR, BabbageEra)
import Data.Aeson (ToJSON, toJSON)
import Data.Int (Int)
import GHC.Generics (Generic)
import qualified Data.ByteString as Data.ByteString.Internal

data Command
  = Init {contestationPeriod :: !Int}
  | Abort
  | Commit {utxo :: !(UTxO BabbageEra)}
  | NewTx {transaction :: !TxCBOR}
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving stock (Generic)

newtype TxCBOR = TxCBOR (Tx BabbageEra)

instance ToJSON Data.ByteString.Internal.ByteString => ToJSON TxCBOR where
  toJSON (TxCBOR tx) = toJSON (serialiseToCBOR tx)

newTx :: Tx BabbageEra -> Command
newTx tx = NewTx (TxCBOR tx)

instance ToJSON Data.ByteString.Internal.ByteString => ToJSON Command