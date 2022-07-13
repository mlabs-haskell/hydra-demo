{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module HydraAuction.UserInput (
  BidParams(..),
  ClaimParams (..),
  CloseParams(..),
  StartParams (..),
) where

import Control.Lens qualified as Lens
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Ledger (PubKeyHash)
import PlutusTx.Prelude (BuiltinByteString)

import HydraAuction.OffChain (BidParams(..), CloseParams(..), StartParams (..))

data ClaimParams = ClaimParams
  { mySalt :: BuiltinByteString
  , theirPkh :: PubKeyHash
  , theirSalt :: BuiltinByteString
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

Lens.makeLenses ''ClaimParams
