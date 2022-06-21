{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module EndToEnd.Spec (spec, headTests) where

import Hydra.Prelude hiding (threadDelay)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (waitForUTxO)
import CardanoCluster (
  Marked (Fuel, Normal),
  defaultNetworkId,
  seedFromFaucet,
  seedFromFaucet_,
 )
import CardanoNode (RunningNode (RunningNode), newNodeConfig, withBFTNode)
import Control.Concurrent (threadDelay)
import Control.Lens (anyOf, (^?))
import Data.Aeson (Result (..), Value (Object, String), eitherDecodeStrict', fromJSON, object, (.=))
import Data.Aeson.Lens (key, values)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (pack)
import Hydra.Cardano.Api (
  AddressInEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  ProtocolParameters,
  TxIn (..),
  VerificationKey,
  lovelaceToValue,
  mkVkAddress,
  serialiseAddress,
  unSlotNo,
 )
import Hydra.Chain.Direct.Handlers (closeGraceTime)
import Hydra.Cluster.Util (readConfigFile)
import Hydra.Crypto (generateSigningKey)
import Hydra.Crypto qualified as Hydra
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (Tx, genKeyPair, mkSimpleTx)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Party (Party, deriveParty)
import HydraNode (
  EndToEndLog (..),
  input,
  output,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraCluster,
 )
import HydraRPS.App (EnqueueCommand, HeadState (..), UserCommand (..), betConstant, mkUserCredentials, withApiClient)
import HydraRPS.OnChain (GameDatum (..), Gesture (..), encryptGesture, rpsValidatorAddress)
import HydraRPS.UserInput (PlayParams (..))
import Ledger qualified
import Ledger.Tx.CardanoAPI (toCardanoAddress)
import PlutusTx qualified
import Test.QuickCheck (generate)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Prelude qualified

headTests :: IO TestTree
headTests = testSpec "Head" spec

spec :: Spec
spec = around showLogsOnFailure $ do
  describe "End-to-end test using a single cardano-node" $ do
    describe "three hydra nodes scenario" $
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
            config <- newNodeConfig tmpDir
            withBFTNode (contramap FromCardanoNode tracer) config $ \node -> do
              initAndClose tracer node

  describe "RPS tests" $ do
    describe "place and observe bet" $
      it "inits a Head, places a bet then closes the head" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-cardano-node" $ \tmpDir -> do
            config <- newNodeConfig tmpDir
            withBFTNode (contramap FromCardanoNode tracer) config $ \node -> do
              initBetAndClose tracer node

withTestApiClients :: Int -> [HeadState] -> ([EnqueueCommand] -> IO ()) -> IO ()
withTestApiClients firstNodeId states action = do
  initClients [] allNodeIds
  where
    allNodeIds = [firstNodeId .. firstNodeId + length states - 1]

    initClients enqueueFns = \case
      [] -> action (reverse enqueueFns)
      (nodeId : rest) -> do
        let headState = states Prelude.!! (nodeId - firstNodeId)
            host = "127.0.0.1"
            port = 4000 + nodeId
        withApiClient headState host port $ \enqueue' -> do
          -- We need to add a delay as we have both the hydraClient socket to read output (coming from withHydraCluster) and our own client to send commands. We don't want our client to issue commands before it has reached the state in which the hydraClient is
          let enqueue cmd = do
                threadDelay 10000
                enqueue' cmd
          initClients (enqueue : enqueueFns) rest

initAndClose :: Tracer IO EndToEndLog -> RunningNode -> IO ()
initAndClose tracer node@(RunningNode _ nodeSocket) = do
  withTempDir "end-to-end-init-and-close" $ \tmpDir -> do
    aliceKeys@(aliceCardanoVk, aliceCardanoSk) <- generate genKeyPair
    bobKeys@(bobCardanoVk, _) <- generate genKeyPair
    carolKeys@(carolCardanoVk, _) <- generate genKeyPair

    let aliceSk, bobSk, carolSk :: Hydra.SigningKey
        aliceSk = generateSigningKey "alice"
        bobSk = generateSigningKey "bob"
        carolSk = generateSigningKey "carol"
    let alice, bob, carol :: Party
        alice = deriveParty aliceSk
        bob = deriveParty bobSk
        carol = deriveParty carolSk

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
              (inHeadVKeyAddress bobCardanoVk, lovelaceToValue paymentFromAliceToBob)
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
                    [ "address" .= String (serialiseAddress $ inHeadVKeyAddress bobCardanoVk)
                    , "value" .= object ["lovelace" .= int paymentFromAliceToBob]
                    ]
                )
              ,
                ( TxIn (txId tx) (toEnum 1)
                , object
                    [ "address" .= String (serialiseAddress $ inHeadVKeyAddress aliceCardanoVk)
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

      waitFor tracer (expectedContestationPeriod contestationPeriod) [n1] $
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

    let aliceSk, bobSk :: Hydra.SigningKey
        aliceSk = generateSigningKey "alice"
        bobSk = generateSigningKey "bob"

        alice, bob :: Party
        alice = deriveParty aliceSk
        bob = deriveParty bobSk

        cardanoKeys = [aliceKeys, bobKeys]
        hydraKeys = [aliceSk, bobSk]

        firstNodeId = 0

    withHydraCluster tracer tmpDir nodeSocket firstNodeId cardanoKeys hydraKeys $ \nodes -> do
      let [aliceNode, bobNode] = toList nodes
      waitForNodesConnected tracer [aliceNode, bobNode]
      putStrLn "\nnodes are up"

      -- Funds to be used as fuel by Hydra protocol transactions
      -- nodes need to be up to observe fuel tx
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
      withTestApiClients firstNodeId [aliceState, bobState] $ \[enqueueAliceCommand, enqueueBobCommand] -> do
        enqueueAliceCommand $ InitHead contestationPeriod

        waitFor tracer 10 [aliceNode, bobNode] $
          output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob]]

        -- Get some UTXOs to commit to a head
        committedUTxOByAlice <- seedFromFaucet defaultNetworkId node aliceCardanoVk aliceCommittedToHead Normal
        committedUTxOByBob <- seedFromFaucet defaultNetworkId node bobCardanoVk bobCommittedToHead Normal
        enqueueAliceCommand $ CommitToHead (UTxO.toApi committedUTxOByAlice)
        enqueueBobCommand $ CommitToHead (UTxO.toApi committedUTxOByBob)

        waitFor tracer 10 [aliceNode, bobNode] $ output "HeadIsOpen" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob)]

        let gesture = Rock
            salt = "1234"

        enqueueAliceCommand $ Bet $ PlayParams gesture salt

        let expectedDatumHash = String $ show $ Ledger.datumHash $ Ledger.Datum $ PlutusTx.toBuiltinData $ GameDatum (encryptGesture gesture salt) aliceState.hsUserCredentials.userPubKeyHash

        txJson <- waitMatch 10 bobNode $ \v -> do
          guard $ v ^? key "tag" == Just "TxSeen"
          transaction <- v ^? key "transaction"
          body <- transaction ^? key "body"
          outputs <- body ^? key "outputs"
          guard $ anyOf values (\tx -> maybe False (== expectedDatumHash) (tx ^? key "datumhash")) outputs
          pure transaction

        tx :: Tx <- do
          case fromJSON txJson of
            Error err -> failure $ "tx isn't valid JSON?: " <> err
            Success a -> pure a

        let expectedSnapshotNumber :: Int
            expectedSnapshotNumber = 1

            newUTxO :: Map.Map TxIn Value
            newUTxO =
              Map.fromList
                [
                  ( TxIn (txId tx) (toEnum 0)
                  , object
                      [ "address" .= String (serialiseAddress $ inHeadScriptAddress rpsValidatorAddress)
                      , "datumhash" .= expectedDatumHash
                      , "value" .= object ["lovelace" .= betConstant]
                      ]
                  )
                ,
                  ( TxIn (txId tx) (toEnum 1)
                  , object
                      [ "address" .= String (serialiseAddress $ inHeadVKeyAddress aliceCardanoVk)
                      , "value" .= object ["lovelace" .= (aliceCommittedToHead - betConstant)]
                      ]
                  )
                ]
                <> fmap toJSON (Map.fromList (UTxO.pairs committedUTxOByBob))

            expectedSnapshot :: Value
            expectedSnapshot =
              object
                [ "snapshotNumber" .= int expectedSnapshotNumber
                , "utxo" .= newUTxO
                , "confirmedTransactions" .= [tx]
                ]

        waitMatch 10 bobNode $ \v -> do
          guard $ v ^? key "tag" == Just "SnapshotConfirmed"
          snapshot <- v ^? key "snapshot"
          guard $ snapshot == expectedSnapshot

        enqueueAliceCommand CloseHead

        waitMatch 10 bobNode $ \v -> do
          guard $ v ^? key "tag" == Just "HeadIsClosed"
          snapshotNumber <- v ^? key "snapshotNumber"
          guard $ snapshotNumber == toJSON expectedSnapshotNumber

        waitFor tracer (expectedContestationPeriod contestationPeriod) [aliceNode, bobNode] $
          output "ReadyToFanout" []

        enqueueAliceCommand IssueFanout

        waitFor tracer 3 [aliceNode, bobNode] $
          output "HeadIsFinalized" ["utxo" .= toJSON newUTxO]

--
-- Fixtures
--

aliceCommittedToHead :: Num a => a
aliceCommittedToHead = 20_000_000

bobCommittedToHead :: Num a => a
bobCommittedToHead = 5_000_000

paymentFromAliceToBob :: Num a => a
paymentFromAliceToBob = 1_000_000

network :: NetworkId
network = Testnet (NetworkMagic 14)

inHeadVKeyAddress :: VerificationKey PaymentKey -> AddressInEra
inHeadVKeyAddress =
  mkVkAddress network

inHeadScriptAddress :: Ledger.Address -> AddressInEra
inHeadScriptAddress addr = either (error . show) id $ toCardanoAddress network addr

--
-- Helpers
--

int :: Int -> Int
int = id

expectedContestationPeriod :: Int -> Natural
expectedContestationPeriod contestationPeriod =
  -- NOTE: We expect the head to be finalized after the contestation period
  -- and some three secs later, plus the closeGraceTime * slotLength
  truncate $ (fromIntegral @_ @Double (unSlotNo closeGraceTime) * 0.1) + fromIntegral contestationPeriod + 3
