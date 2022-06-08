{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module HydraRPS.UserInput (
  ClaimParams (..),
  PlayParams (..),
) where

import Control.Lens qualified as Lens
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Ledger (PubKeyHash)
import PlutusTx.Prelude (BuiltinByteString)

import HydraRPS.OffChain (PlayParams (..))

data ClaimParams = ClaimParams
  { mySalt :: BuiltinByteString
  , theirPkh :: PubKeyHash
  , theirSalt :: BuiltinByteString
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

Lens.makeLenses ''ClaimParams
