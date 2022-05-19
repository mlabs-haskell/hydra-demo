{-# OPTIONS_GHC -w #-}
module OffChain.Test (offChainTests) where

import Test.Tasty qualified as Tasty
import HydraRPS.OffChain
import HydraRPS.OnChain
import Ledger qualified
import Plutus.Trace.Emulator qualified as Emulator
import Plutus.V1.Ledger.Ada qualified as Ada
import Wallet.Emulator.Wallet (Wallet (..), knownWallet, mockWalletPaymentPubKeyHash)
import Data.Text (Text)
import Plutus.Contract.Test (
  assertNoFailedTransactions,
  checkPredicate,
  checkPredicateOptions,
  dataAtAddress,
  defaultCheckOptions,
  emulatorConfig,
  valueAtAddress,
  walletFundsChange,
  (.&&.),
 )
import Data.ByteString qualified as BS
import PlutusTx.Builtins (BuiltinByteString)
import Prelude

offChainTests :: Tasty.TestTree
offChainTests =
  Tasty.testGroup
    "Off-chain tests"
    [ playTests
    , claimTests ]

playTests :: Tasty.TestTree
playTests =
  Tasty.testGroup
    "play"
    [ checkPredicate 
        "Funds are deposited at validator"
        (valueAtAddress winnerValidatorAddress (== (Ada.toValue 40000000)))
        playTrace
    ]
  where
    playTrace :: Emulator.EmulatorTrace ()
    playTrace = do
      h1 <- Emulator.activateContractWallet user1Wallet endpoints
      h2 <- Emulator.activateContractWallet user2Wallet endpoints

      Emulator.callEndpoint @"Play" h1 $
        PlayParams
          { ppGesture = Rock
          , ppSalt = user1Salt
          }
      Emulator.callEndpoint @"Play" h2 $
        PlayParams
          { ppGesture = Paper
          , ppSalt = user2Salt
          }

claimTests :: Tasty.TestTree
claimTests =
  Tasty.testGroup
    "claim"
    [ checkPredicate 
        "P2 can claim victory"
        ((walletFundsChange user2Wallet (Ada.toValue 20000000))
          .&&.
        (walletFundsChange user1Wallet (Ada.toValue (-20000000))))
        p2Win
    , checkPredicate 
        "P1 can claim draw"
        ((walletFundsChange user2Wallet (Ada.toValue 0))
          .&&.
        (walletFundsChange user1Wallet (Ada.toValue 0)))
        draw
    ]
  where
    p2Win :: Emulator.EmulatorTrace ()
    p2Win = do
      h1 <- Emulator.activateContractWallet user1Wallet endpoints
      h2 <- Emulator.activateContractWallet user2Wallet endpoints

      Emulator.callEndpoint @"Play" h1 $
        PlayParams
          { ppGesture = Rock
          , ppSalt = user1Salt
          }
      Emulator.callEndpoint @"Play" h2 $
        PlayParams
          { ppGesture = Paper
          , ppSalt = user2Salt
          }
      Emulator.waitNSlots 10
      Emulator.callEndpoint @"Collect" h2 $
        CollectParams
          { cpMyInfo = (user2PubKeyHash, user2Salt)
          , cpTheirInfo = (user1PubKeyHash, user1Salt)
          }

    draw :: Emulator.EmulatorTrace ()
    draw = do
      h1 <- Emulator.activateContractWallet user1Wallet endpoints
      h2 <- Emulator.activateContractWallet user2Wallet endpoints

      Emulator.callEndpoint @"Play" h1 $
        PlayParams
          { ppGesture = Rock
          , ppSalt = user1Salt
          }
      Emulator.callEndpoint @"Play" h2 $
        PlayParams
          { ppGesture = Rock
          , ppSalt = user2Salt
          }
      Emulator.waitNSlots 10
      Emulator.callEndpoint @"Collect" h1 $
        CollectParams
          { cpMyInfo = (user1PubKeyHash, user1Salt)
          , cpTheirInfo = (user2PubKeyHash, user2Salt)
          }

user1Wallet :: Wallet
user1Wallet = knownWallet 1

user1Salt :: Text
user1Salt = "user1"

user2Wallet :: Wallet
user2Wallet = knownWallet 2

user2Salt :: Text
user2Salt = "user2"

user1PubKeyHash :: Ledger.PubKeyHash
user1PubKeyHash = Ledger.unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash user1Wallet

user2PubKeyHash :: Ledger.PubKeyHash
user2PubKeyHash = Ledger.unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash user2Wallet