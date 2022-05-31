{-# LANGUAGE DeriveAnyClass #-}

module HydraRPS.UserInput (
  ClaimInput (..),
  ClaimParams (..),
  PlayParams (..),
) where

import Cardano.Api (TxIn)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Ledger (PubKeyHash)

import HydraRPS.OffChain (PlayParams (..))

data ClaimInput = ClaimInput
  { txIn :: TxIn
  , pkh :: PubKeyHash
  , playParams :: PlayParams
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data ClaimParams = ClaimParams
  { myInput :: ClaimInput
  , theirInput :: ClaimInput
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
