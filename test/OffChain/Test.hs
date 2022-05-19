module OffChain.Test (offChainTests) where

import Control.Monad (void)
import HydraRPS.OffChain
import HydraRPS.OnChain
import Ledger qualified
import Plutus.Contract.Test (
  checkPredicate,
  valueAtAddress,
  walletFundsChange,
  (.&&.),
 )
import Plutus.Trace.Emulator qualified as Emulator
import Plutus.V1.Ledger.Ada qualified as Ada
import PlutusTx.Builtins (BuiltinByteString)
import Test.Tasty qualified as Tasty
import Wallet.Emulator.Wallet (Wallet (..), knownWallet, mockWalletPaymentPubKeyHash)
import Prelude

offChainTests :: Tasty.TestTree
offChainTests =
  Tasty.testGroup
    "Off-chain tests"
    [ playTests
    , claimTests
    ]

playTests :: Tasty.TestTree
playTests =
  Tasty.testGroup
    "play"
    [ checkPredicate
        "Funds are deposited at validator"
        (valueAtAddress winnerValidatorAddress (== Ada.toValue 40000000))
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
        ( walletFundsChange user2Wallet (Ada.toValue 20000000)
            .&&. walletFundsChange user1Wallet (Ada.toValue (-20000000))
        )
        p2Win
    , checkPredicate
        "P1 can claim draw"
        ( walletFundsChange user2Wallet (Ada.toValue 0)
            .&&. walletFundsChange user1Wallet (Ada.toValue 0)
        )
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
      void $ Emulator.waitNSlots 10
      Emulator.callEndpoint @"Collect" h2 $
        GameRedeemer
          { grMyInfo = (user2PubKeyHash, user2Salt)
          , grTheirInfo = (user1PubKeyHash, user1Salt)
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
      void $ Emulator.waitNSlots 10
      Emulator.callEndpoint @"Collect" h1 $
        GameRedeemer
          { grMyInfo = (user1PubKeyHash, user1Salt)
          , grTheirInfo = (user2PubKeyHash, user2Salt)
          }

user1Wallet :: Wallet
user1Wallet = knownWallet 1

user1Salt :: BuiltinByteString
user1Salt = "user1"

user2Wallet :: Wallet
user2Wallet = knownWallet 2

user2Salt :: BuiltinByteString
user2Salt = "user2"

user1PubKeyHash :: Ledger.PubKeyHash
user1PubKeyHash = Ledger.unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash user1Wallet

user2PubKeyHash :: Ledger.PubKeyHash
user2PubKeyHash = Ledger.unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash user2Wallet
