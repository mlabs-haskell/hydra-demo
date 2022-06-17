
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -w #-}

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
import Data.Aeson (Result (..), Value (Object, String), fromJSON, object, (.=), eitherDecodeStrict')
import Data.Aeson.Lens (key)
import Data.Text (pack)
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
  ProtocolParameters
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
  connection,
  withNewClient
 )
import Test.QuickCheck (generate)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Hydra.Cluster.Util (readConfigFile)
import HydraRPS.App (UserCommand (..), AppEvent (..), withApiClient, HeadState (..), mkUserCredentials)
import HydraRPS.UserInput (PlayParams (..))
import HydraRPS.OnChain (Gesture (..))
import Control.Concurrent (writeChan)
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

  -- describe "End-to-end test using a single cardano-node" $ do
  --   describe "three hydra nodes scenario" $
  --     it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
  --       failAfter 60 $
  --         withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
  --           config <- newNodeConfig tmpDir
  --           withBFTNode (contramap FromCardanoNode tracer) config $ \node -> do
  --             initAndClose tracer node

  describe "RPS tests" $ do
    describe "place and observe bet" $
      it "inits a Head, places a bet then closes the head" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
            config <- newNodeConfig tmpDir
            withBFTNode (contramap FromCardanoNode tracer) config $ \node -> do
              initBetAndClose tracer node

  
initAndClose :: Tracer IO EndToEndLog -> RunningNode -> IO ()
initAndClose tracer node@(RunningNode _ nodeSocket) = do
  withTempDir "end-to-end-init-and-close" $ \tmpDir -> do
    aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- generate genKeyPair
    bobKeys@(bobCardanoVk, _) <- generate genKeyPair
    carolKeys@(carolCardanoVk, _) <- generate genKeyPair

    let aliceSk = generateSigningKey "alice"
    let bobSk = generateSigningKey "bob"
    let carolSk = generateSigningKey "carol"

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


initBetAndClose :: Tracer IO EndToEndLog -> RunningNode -> IO ()
initBetAndClose tracer node@(RunningNode _ nodeSocket) = do
  withTempDir "end-to-end-init-bet-and-close" $ \tmpDir -> do
    aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- generate genKeyPair
    bobKeys@(bobCardanoVk, bobCardanoSk) <- generate genKeyPair

    let aliceSk = generateSigningKey "alice"
    let bobSk = generateSigningKey "bob"

    let alice = deriveParty aliceSk
    let bob = deriveParty bobSk

    let cardanoKeys = [aliceKeys, bobKeys]
        hydraKeys = [aliceSk, bobSk]

    let firstNodeId = 0

    withHydraCluster tracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys $ \nodes -> do
      let [aliceNode, bobNode] = toList nodes
      waitForNodesConnected tracer [aliceNode, bobNode]
      putStrLn "\nnodes are up"
      -- nodes need to be up to observe fuel tx
      -- hydra node port is 5000 + id
      -- Funds to be used as fuel by Hydra protocol transactions
      seedFromFaucet_ defaultNetworkId node aliceCardanoVk 100_000_000 Fuel
      seedFromFaucet_ defaultNetworkId node bobCardanoVk 100_000_000 Fuel

      let contestationPeriod = 2 :: Int
      pp <- eitherDecodeStrict' <$> readConfigFile "protocol-parameters.json" :: IO (Either String ProtocolParameters)
      let protocolParams = either (error . pack) id pp
          aliceCredentials = mkUserCredentials defaultNetworkId aliceCardanoSk
          bobCredentials = mkUserCredentials defaultNetworkId bobCardanoSk
          aliceState = HeadState aliceCredentials defaultNetworkId protocolParams
          bobState = HeadState bobCredentials defaultNetworkId protocolParams
      putStrLn "starting client"
      withApiClient aliceState "127.0.0.1" 4000 $ \enqueueAliceCommand' -> do
        withApiClient bobState "127.0.0.1" 4001 $ \enqueueBobCommand' -> do
          putStrLn "api client ready \n"
          let enqueueAliceCommand = do
                threadDelay 500
                enqueueAliceCommand'
          let enqueueBobCommand = do
                threadDelay 500
                enqueueBobCommand'
          enqueueAliceCommand $ InitHead contestationPeriod

          -- send aliceNode $ input "Init" ["contestationPeriod" .= contestationPeriod]
          -- newBobNode <- withNewClient bobNode pure
          waitFor tracer 10 [aliceNode, bobNode] $
            output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob]]

          -- Get some UTXOs to commit to a head
          committedUTxOByAlice <- seedFromFaucet defaultNetworkId node aliceCardanoVk aliceCommittedToHead Normal
          committedUTxOByBob <- seedFromFaucet defaultNetworkId node bobCardanoVk bobCommittedToHead Normal
          enqueueAliceCommand $ CommitToHead (UTxO.toApi committedUTxOByAlice)
          enqueueBobCommand $ CommitToHead (UTxO.toApi  committedUTxOByBob)

          waitFor tracer 20 [aliceNode, bobNode] $ output "HeadIsOpen" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob)]

          enqueueAliceCommand $ Bet $ PlayParams { ppGesture = Rock, ppSalt = "1234" }

          waitFor tracer 10 [aliceNode, bobNode] $ output "HeadIsOpena" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob)]
          pure ()


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