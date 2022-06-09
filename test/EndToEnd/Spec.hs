
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module EndToEnd.Spec (spec, headTests) where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import CardanoClient (waitForUTxO)
import CardanoCluster (
  Marked (Fuel, Normal),
  chainConfigFor,
  defaultNetworkId,
  keysFor,
  seedFromFaucet,
  seedFromFaucet_,
 )
import CardanoNode (RunningNode (RunningNode), newNodeConfig, withBFTNode)
import Control.Lens ((^?))
import Data.Aeson (Result (..), Value (Object, String), fromJSON, object, (.=))
import Data.Aeson.Lens (key)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  AddressInEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  TxId,
  TxIn (..),
  VerificationKey,
  lovelaceToValue,
  mkVkAddress,
  serialiseAddress,
  unSlotNo,
 )
import Hydra.Chain.Direct.Handlers (closeGraceTime)
import Hydra.Crypto (deriveVerificationKey, generateSigningKey)
import qualified Hydra.Crypto as Hydra
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (genKeyPair, mkSimpleTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Party (Party, deriveParty)
import HydraNode (
  EndToEndLog (..),
  getMetrics,
  input,
  output,
  proc,
  readCreateProcess,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
  withHydraNode,
 )
import Test.QuickCheck (generate)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import qualified Prelude

allNodeIds :: [Int]
allNodeIds = [1 .. 3]

headTests :: IO TestTree
headTests = testSpec "Head" spec

spec :: Spec
spec = around showLogsOnFailure $ do
  let aliceSk, bobSk, carolSk :: Hydra.SigningKey
      aliceSk = generateSigningKey "alice"
      bobSk = generateSigningKey "bob"
      carolSk = generateSigningKey "carol"

      aliceVk, bobVk, carolVk :: Hydra.VerificationKey
      aliceVk = deriveVerificationKey aliceSk
      bobVk = deriveVerificationKey bobSk
      carolVk = deriveVerificationKey carolSk

      alice, bob, carol :: Party
      alice = deriveParty aliceSk
      bob = deriveParty bobSk
      carol = deriveParty carolSk

  describe "End-to-end test using a single cardano-node" $ do
    describe "three hydra nodes scenario" $
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
            config <- newNodeConfig tmpDir
            withBFTNode (contramap FromCardanoNode tracer) config $ \node -> do
              initAndClose tracer node

  
initAndClose :: Tracer IO EndToEndLog -> RunningNode -> IO ()
initAndClose tracer node@(RunningNode _ nodeSocket) = do
  withTempDir "end-to-end-init-and-close" $ \tmpDir -> do
    aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- generate genKeyPair
    bobKeys@(bobCardanoVk, _) <- generate genKeyPair
    carolKeys@(carolCardanoVk, _) <- generate genKeyPair

    let aliceSk = generateSigningKey ("alice")
    let bobSk = generateSigningKey ("bob")
    let carolSk = generateSigningKey ("carol")

    let alice = deriveParty aliceSk
    let bob = deriveParty bobSk
    let carol = deriveParty carolSk

    let cardanoKeys = [aliceKeys, bobKeys, carolKeys]
        hydraKeys = [aliceSk, bobSk, carolSk]

    let firstNodeId = 0

    withHydraCluster tracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys $ \nodes -> do
      let [n1, n2, n3] = toList nodes
      waitForNodesConnected tracer [n1, n2, n3]
      -- nodes need to be up to observe fuel tx
      -- hydra node port is 5000 + id
      -- Funds to be used as fuel by Hydra protocol transactions
      seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel
      seedFromFaucet_ defaultNetworkId node bobCardanoVk 100_000_000 Fuel
      seedFromFaucet_ defaultNetworkId node carolCardanoVk 100_000_000 Fuel

      let contestationPeriod = 2

      send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
      waitFor tracer 10 [n1, n2, n3] $
        output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob, carol]]

      -- Get some UTXOs to commit to a head
      committedUTxOByAlice <- seedFromFaucet defaultNetworkId node aliceCardanoVk aliceCommittedToHead Normal
      committedUTxOByBob <- seedFromFaucet defaultNetworkId node bobCardanoVk bobCommittedToHead Normal
      send n1 $ input "Commit" ["utxo" .= committedUTxOByAlice]
      send n2 $ input "Commit" ["utxo" .= committedUTxOByBob]
      send n3 $ input "Commit" ["utxo" .= Object mempty]
      waitFor tracer 10 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob)]

      -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
      let firstCommittedUTxO = Prelude.head $ UTxO.pairs committedUTxOByAlice
      let Right tx =
            mkSimpleTx
              firstCommittedUTxO
              (inHeadAddress bobCardanoVk, lovelaceToValue paymentFromAliceToBob)
              aliceCardanoSk
      send n1 $ input "NewTx" ["transaction" .= tx]
      waitFor tracer 10 [n1, n2, n3] $
        output "TxSeen" ["transaction" .= tx]

      -- The expected new utxo set is the created payment to bob,
      -- alice's remaining utxo in head and whatever bot has
      -- committed to the head
      let newUTxO =
            Map.fromList
              [
                ( TxIn (txId tx) (toEnum 0)
                , object
                    [ "address" .= String (serialiseAddress $ inHeadAddress bobCardanoVk)
                    , "value" .= object ["lovelace" .= int paymentFromAliceToBob]
                    ]
                )
              ,
                ( TxIn (txId tx) (toEnum 1)
                , object
                    [ "address" .= String (serialiseAddress $ inHeadAddress aliceCardanoVk)
                    , "value" .= object ["lovelace" .= int (aliceCommittedToHead - paymentFromAliceToBob)]
                    ]
                )
              ]
              <> fmap toJSON (Map.fromList (UTxO.pairs committedUTxOByBob))

      let expectedSnapshot =
            object
              [ "snapshotNumber" .= int expectedSnapshotNumber
              , "utxo" .= newUTxO
              , "confirmedTransactions" .= [tx]
              ]
          expectedSnapshotNumber = 1

      waitMatch 10 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "SnapshotConfirmed"
        snapshot <- v ^? key "snapshot"
        guard $ snapshot == expectedSnapshot

      send n1 $ input "GetUTxO" []
      waitFor tracer 10 [n1] $ output "GetUTxOResponse" ["utxo" .= newUTxO]

      send n1 $ input "Close" []
      waitMatch 3 n1 $ \v -> do
        guard $ v ^? key "tag" == Just "HeadIsClosed"
        snapshotNumber <- v ^? key "snapshotNumber"
        guard $ snapshotNumber == toJSON expectedSnapshotNumber

      -- NOTE: We expect the head to be finalized after the contestation period
      -- and some three secs later, plus the closeGraceTime * slotLength
      waitFor tracer (truncate $ contestationPeriod + (fromIntegral @_ @Double (unSlotNo closeGraceTime) * 0.1) + 3) [n1] $
        output "ReadyToFanout" []

      send n1 $ input "Fanout" []
      waitFor tracer 3 [n1] $
        output "HeadIsFinalized" ["utxo" .= newUTxO]

      case fromJSON $ toJSON newUTxO of
        Error err ->
          failure $ "newUTxO isn't valid JSON?: " <> err
        Success u ->
          failAfter 5 $ waitForUTxO defaultNetworkId nodeSocket u

--
-- Fixtures
--

aliceCommittedToHead :: Num a => a
aliceCommittedToHead = 20_000_000

bobCommittedToHead :: Num a => a
bobCommittedToHead = 5_000_000

paymentFromAliceToBob :: Num a => a
paymentFromAliceToBob = 1_000_000

someTxId :: IsString s => s
someTxId = "9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903"

inHeadAddress :: VerificationKey PaymentKey -> AddressInEra
inHeadAddress =
  mkVkAddress network
 where
  network = Testnet (NetworkMagic 14)

--
-- Helpers
--

int :: Int -> Int
int = id

outputRef :: TxId -> Natural -> Value
outputRef tid tix =
  object
    [ "txId" .= tid
    , "index" .= tix
    ]