{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HydraRPS.App (EnqueueCommand, HeadState (..), UserCommand (..), betConstant, withApiClient, UserCredentials, mkUserCredentials) where

import Cardano.Api (
  AddressInEra,
  AlonzoEra,
  BuildTxWith (BuildTxWith),
  CollateralSupportedInEra (CollateralInAlonzoEra),
  ExecutionUnits (ExecutionUnits, executionMemory, executionSteps),
  Hash,
  Key (getVerificationKey, verificationKeyHash),
  Lovelace (..),
  NetworkId,
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  ScriptData,
  SigningKey,
  StakeAddressReference (NoStakeAddress),
  TxBody,
  TxBodyContent (txIns, txInsCollateral, txOuts, txProtocolParams),
  TxIn,
  TxInsCollateral (TxInsCollateral),
  TxOut (TxOut),
  TxOutDatum (TxOutDatumHash),
  UTxO (UTxO, unUTxO),
  hashScriptData,
  makeShelleyAddressInEra,
  makeTransactionBody,
  txOutValueToValue,
 )
import Cardano.Api qualified (Value)
import Cardano.Api.Shelley (ProtocolParameters (protocolParamMaxTxExUnits), fromPlutusData)
import Control.Concurrent (newChan, readChan, writeChan)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (ToJSON (toJSON), eitherDecode, encode, (.:))
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map qualified as Map (lookupMin, toList)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as Text (putStrLn)
import Ledger qualified (Address (Address), PubKeyHash)
import Ledger.Tx.CardanoAPI (fromCardanoPaymentKeyHash, toCardanoAddressInEra)
import Network.WebSockets (ConnectionException, receiveData, runClient, sendTextData)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential))
import PlutusTx (ToData (toBuiltinData), builtinDataToData)
import PlutusTx.Prelude (BuiltinByteString)

import HydraRPS.Node.Command qualified as NodeCommand
import HydraRPS.OnChain (
  GameDatum (..),
  GameRedeemer (GameRedeemer),
  Gesture,
  beats,
  encryptGesture,
  rpsValidator,
  rpsValidatorAddress,
 )
import HydraRPS.Tx (
  TxDatum (TxDatum),
  TxRedeemer (TxRedeemer),
  baseBodyContent,
  extractLovelace,
  signTx,
  txInForSpending,
  txInForValidator,
  txOutToAddress,
  txOutToScript,
  txOutValueToAddress,
  utxosAt,
 )
import HydraRPS.UserInput qualified as UserInput

import Prelude
import Data.ByteString.Internal qualified as BSI (ByteString)

data HeadState = HeadState
  { hsUserCredentials :: UserCredentials
  , hsNetworkId :: NetworkId
  , hsProtocolParams :: ProtocolParameters
  }

newtype HeadM a = HeadM {unHeadM :: ReaderT HeadState IO a} deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HeadState)

runInHead :: HeadState -> HeadM a -> IO a
runInHead c ba = runReaderT (unHeadM ba) c

data AppEvent
  = ApiEvent (Maybe ServerOutput)
  | UserCommand UserCommand

data ServerOutput
  = Other Text
  | HeadIsOpen (UTxO AlonzoEra)
  | SnapshotConfirmed (UTxO AlonzoEra)
  | HeadIsClosed
  | ReadyToFanout
  | HeadIsFinalized (UTxO AlonzoEra)

data UserCommand
  = InitHead Int
  | CommitToHead (UTxO AlonzoEra)
  | Exit
  | AbortHead
  | CloseHead
  | IssueFanout
  | Bet UserInput.PlayParams
  | Claim UserInput.ClaimParams

data UserCredentials = UserCredentials
  { userSkey :: SigningKey PaymentKey
  , userPubKeyHash :: Ledger.PubKeyHash
  , userAddress :: AddressInEra AlonzoEra
  }

mkUserCredentials :: NetworkId -> SigningKey PaymentKey -> UserCredentials
mkUserCredentials networkId skey = UserCredentials skey pkh addr
  where
    vkeyHash = verificationKeyHash (getVerificationKey skey)
    pkh = fromCardanoPaymentKeyHash vkeyHash
    addr = makeShelleyAddressInEra networkId (PaymentCredentialByKey vkeyHash) NoStakeAddress

type EnqueueCommand = UserCommand -> IO ()

withApiClient :: HeadState -> String -> Int -> (EnqueueCommand -> IO ()) -> IO ()
withApiClient headState host port action = do
  runClient host port "/" $ \ws -> do
    events <- newChan
    let nextServerEvent :: IO ByteString
        nextServerEvent = receiveData ws

        enqueueUserCommand :: UserCommand -> IO ()
        enqueueUserCommand = writeChan events . UserCommand

        enqueueApiEvent :: Maybe ServerOutput -> IO ()
        enqueueApiEvent = writeChan events . ApiEvent

        submitCommand :: (MonadIO m, ToJSON a) => a -> m ()
        submitCommand input = do
          let json = decodeUtf8 $ encode input
          liftIO $ Text.putStrLn $ "client input:\n" <> json <> "\n"
          liftIO $ sendTextData ws json

        nextEvent :: IO AppEvent
        nextEvent = readChan events

    withAsync (apiReader nextServerEvent enqueueApiEvent) $ \_ -> do
      withAsync (runInHead headState (eventProcessor submitCommand nextEvent)) $ \eventLoopAsync -> do
        action enqueueUserCommand
        enqueueUserCommand Exit
        wait eventLoopAsync

apiReader :: IO ByteString -> (Maybe ServerOutput -> IO ()) -> IO ()
apiReader nextServerEvent enqueue = go
  where
    go = do
      result <- try @ConnectionException nextServerEvent
      case result of
        Left e -> do
          putStrLn $ "node connection error: " <> show e
          enqueue Nothing
        Right o -> do
          Text.putStrLn $ "node output:\n" <> decodeUtf8 o
          case decodeServerOutput o of
            Left err -> putStrLn $ "node output decoding error: " <> err
            Right decoded -> enqueue $ Just decoded
          putStrLn ""
          go

decodeServerOutput :: ByteString -> Either String ServerOutput
decodeServerOutput bytes = do
  value <- eitherDecode bytes
  flip parseEither value $ \o -> do
    tag <- o .: "tag"
    case tag of
      "HeadIsOpen" -> HeadIsOpen <$> o .: "utxo"
      "SnapshotConfirmed" -> do
        snapshot <- o .: "snapshot"
        SnapshotConfirmed <$> snapshot .: "utxo"
      "HeadIsClosed" -> pure HeadIsClosed
      "ReadyToFanout" -> pure ReadyToFanout
      "HeadIsFinalized" -> HeadIsFinalized <$> o .: "utxo"
      _ -> pure (Other tag)

eventProcessor :: forall (m :: Type -> Type). (MonadIO m, MonadReader HeadState m) => (NodeCommand.Command -> m ()) -> IO AppEvent -> m ()
eventProcessor submit nextEvent = openTheHead
  where
    openTheHead :: m ()
    openTheHead = do
      event <- liftIO nextEvent
      case event of
        ApiEvent apiEvent ->
          case apiEvent of
            Just serverOutput -> do
              case serverOutput of
                HeadIsOpen utxo -> do
                  liftIO $ putStrLn $ "head is opened\n" <> show utxo <> "\n"
                  playTheGame utxo
                SnapshotConfirmed utxo -> do
                  liftIO $ putStrLn $ "unexpected snapshot confirmation (head is not opened yet)\n" <> show utxo <> "\n"
                  openTheHead
                HeadIsClosed -> do
                  liftIO $ putStrLn "unexpected head closure (head is not opened yet)\n"
                  openTheHead
                ReadyToFanout -> do
                  liftIO $ putStrLn "unexpected ready to fanout (head is not opened yet)\n"
                  openTheHead
                HeadIsFinalized utxo -> do
                  liftIO $ putStrLn $ "unexpected head finalization (head is not opened yet)\n" <> show utxo <> "\n"
                  openTheHead
                Other tag -> do
                  liftIO $ Text.putStrLn $ "decoded node output: " <> tag <> "\n"
                  openTheHead
            Nothing -> return ()
        UserCommand command ->
          case command of
            Exit -> return ()
            InitHead period -> submit (NodeCommand.Init period) >> openTheHead
            AbortHead -> submit NodeCommand.Abort >> openTheHead
            CommitToHead utxoToCommit -> submit (NodeCommand.Commit utxoToCommit) >> openTheHead
            _ -> do
              liftIO $ putStrLn "head is not opened yet"
              openTheHead

    playTheGame :: UTxO AlonzoEra -> m ()
    playTheGame utxo = do
      event <- liftIO nextEvent
      case event of
        ApiEvent apiEvent ->
          case apiEvent of
            Just serverOutput -> do
              case serverOutput of
                Other tag -> do
                  liftIO $ Text.putStrLn $ "decoded node output: " <> tag <> "\n"
                  playTheGame utxo
                HeadIsOpen unexpectedUtxo -> do
                  liftIO $ putStrLn $ "unexpected head opening (head is already opened)\n" <> show unexpectedUtxo <> "\n"
                  playTheGame utxo
                SnapshotConfirmed updatedUtxo -> do
                  liftIO $ putStrLn $ "snapshot confirmed\n" <> show updatedUtxo <> "\n"
                  playTheGame updatedUtxo
                HeadIsClosed -> do
                  liftIO $ putStrLn "closing the head\n"
                  waitForFanout
                ReadyToFanout -> do
                  liftIO $ putStrLn "unexpected ready to fanout (head is not closed yet)\n"
                  playTheGame utxo
                HeadIsFinalized unexpectedUtxo -> do
                  liftIO $ putStrLn $ "unexpected head finalization (head is not closed yet)\n" <> show unexpectedUtxo <> "\n"
                  playTheGame utxo
            Nothing -> return ()
        UserCommand command ->
          case command of
            Exit -> return ()
            CloseHead -> submit NodeCommand.Close >> playTheGame utxo
            IssueFanout -> do
              liftIO $ putStrLn "head is not closed yet\n"
              playTheGame utxo
            Bet pp -> do
              state <- ask
              let inputAllocation =
                    allocateUtxos betConstant $
                      sortOn snd $
                        Map.toList $
                          extractLovelace $
                            utxosAt state.hsUserCredentials.userAddress utxo
              case inputAllocation of
                Nothing -> do
                  liftIO $ putStrLn "not enough funds"
                  playTheGame utxo
                Just (refs, total) -> do
                  let unsignedTx = either error id $ buildBetTx refs total state pp
                      signedTx = signTx state.hsUserCredentials.userSkey unsignedTx
                  submit (NodeCommand.newTx signedTx)
                  playTheGame utxo
            Claim cp -> do
              state <- ask
              let redeemer =
                    GameRedeemer
                      (state.hsUserCredentials.userPubKeyHash, cp.mySalt)
                      (cp.theirPkh, cp.theirSalt)
                  txResult = do
                    collateralTxIn <-
                      maybe (Left "could not find collateral") (Right . fst) $
                        Map.lookupMin $
                          unUTxO $
                            utxosAt state.hsUserCredentials.userAddress utxo
                    scriptAddress <- first (("toCardanoAddress: " <>) . show) $ toCardanoAddressInEra state.hsNetworkId rpsValidatorAddress
                    let betUtxos = utxosAt scriptAddress utxo
                    unsignedTx <- buildClaimTx collateralTxIn state (filterUtxos redeemer betUtxos) redeemer
                    pure $ signTx state.hsUserCredentials.userSkey unsignedTx
              case txResult of
                Left err -> liftIO $ putStrLn $ "claim tx building failed: " <> err
                Right signedTx -> submit (NodeCommand.newTx signedTx)
              playTheGame utxo
            _ -> do
              liftIO $ putStrLn "head is already opened"
              playTheGame utxo

    waitForFanout :: m ()
    waitForFanout = do
      event <- liftIO nextEvent
      case event of
        ApiEvent apiEvent ->
          case apiEvent of
            Just serverOutput -> do
              case serverOutput of
                Other tag -> do
                  liftIO $ Text.putStrLn $ "decoded node output: " <> tag <> "\n"
                  waitForFanout
                ReadyToFanout -> do
                  liftIO $ putStrLn "ready to fanout\n"
                  waitForFinalization
                _ -> do
                  liftIO $ putStrLn "unexpected head state\n"
                  waitForFanout
            Nothing -> return ()
        UserCommand command ->
          case command of
            Exit -> return ()
            _ -> do
              liftIO $ putStrLn "no input is expected in this state\n"
              waitForFanout

    waitForFinalization :: m ()
    waitForFinalization = do
      event <- liftIO nextEvent
      case event of
        ApiEvent apiEvent ->
          case apiEvent of
            Just serverOutput -> do
              case serverOutput of
                Other tag -> do
                  liftIO $ Text.putStrLn $ "decoded node output: " <> tag <> "\n"
                  waitForFinalization
                HeadIsFinalized utxo -> do
                  liftIO $ putStrLn $ "head is finalized\n" <> show utxo <> "\n"
                  openTheHead
                _ -> do
                  liftIO $ putStrLn "unexpected head state\n"
                  waitForFinalization
            Nothing -> return ()
        UserCommand command ->
          case command of
            Exit -> return ()
            IssueFanout -> submit NodeCommand.Fanout >> waitForFinalization
            _ -> do
              liftIO $ putStrLn "only fanout input is expected in this state\n"
              waitForFinalization

allocateUtxos :: Lovelace -> [(TxIn, Lovelace)] -> Maybe ([TxIn], Lovelace)
allocateUtxos _ [] = Nothing
allocateUtxos target ((txIn, lovelace) : rest)
  | target <= lovelace = Just ([txIn], lovelace)
  | Just (refs, available) <- allocateUtxos (target - lovelace) rest = Just (txIn : refs, lovelace + available)
  | otherwise = Nothing

betConstant :: Lovelace
betConstant = 10000000

buildDatum :: UserInput.PlayParams -> Ledger.PubKeyHash -> GameDatum
buildDatum playParams pkh =
  GameDatum
    { gdGesture = encryptGesture playParams.ppGesture playParams.ppSalt
    , gdPkh = pkh
    }

-- precondition: inputTotal >= betConstant
buildBetTx :: [TxIn] -> Lovelace -> HeadState -> UserInput.PlayParams -> Either String (TxBody AlonzoEra)
buildBetTx inputRefs inputTotal state playParams = do
  let changeAddress = state.hsUserCredentials.userAddress
      datum = buildDatum playParams state.hsUserCredentials.userPubKeyHash
  scriptOut <-
    first (("bad address specifier: " <>) . show) $
      txOutToScript state.hsNetworkId rpsValidatorAddress betConstant (TxDatum datum)
  let changeOut
        | inputTotal > betConstant = [txOutToAddress changeAddress (inputTotal - betConstant)]
        | otherwise = []
      bodyContent =
        baseBodyContent
          { txIns = txInForSpending <$> inputRefs
          , txOuts = scriptOut : changeOut
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent

buildClaimTx :: TxIn -> HeadState -> [(TxIn, Gesture, Cardano.Api.Value, GameDatum)] -> GameRedeemer -> Either String (TxBody AlonzoEra)
buildClaimTx collateralTxIn state [(myTxIn, myGesture, myTxValue, myDatum), (theirTxIn, theirGesture, theirTxValue, theirDatum)] redeemer = do
  theirAddress <-
    first (("bad their pub key hash: " <>) . show) $
      toCardanoAddressInEra state.hsNetworkId $
        Ledger.Address (PubKeyCredential theirDatum.gdPkh) Nothing
  let maxTxExUnits =
        fromMaybe ExecutionUnits {executionSteps = 0, executionMemory = 0} $
          protocolParamMaxTxExUnits state.hsProtocolParams
      exUnits =
        ExecutionUnits
          { executionSteps = executionSteps maxTxExUnits `div` 2
          , executionMemory = executionMemory maxTxExUnits `div` 2
          }
      outputs
        | beats myGesture theirGesture = [txOutValueToAddress state.hsUserCredentials.userAddress (myTxValue <> theirTxValue)]
        | beats theirGesture myGesture = [txOutValueToAddress theirAddress (myTxValue <> theirTxValue)]
        | otherwise =
          [ txOutValueToAddress state.hsUserCredentials.userAddress myTxValue
          , txOutValueToAddress theirAddress theirTxValue
          ]

  myValidatorTxIn <-
    first (("bad script: " <>) . show) $
      txInForValidator myTxIn rpsValidator (TxDatum myDatum) (TxRedeemer redeemer) exUnits
  theirValidatorIxIn <-
    first (("bad script: " <>) . show) $
      txInForValidator theirTxIn rpsValidator (TxDatum theirDatum) (TxRedeemer redeemer) exUnits

  let bodyContent =
        baseBodyContent
          { txIns = [myValidatorTxIn, theirValidatorIxIn]
          , txInsCollateral = TxInsCollateral CollateralInAlonzoEra [collateralTxIn]
          , txOuts = outputs
          , txProtocolParams = BuildTxWith (Just state.hsProtocolParams)
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent
buildClaimTx _ _ _ _ = Left "bad input parameters"

filterUtxos :: GameRedeemer -> UTxO AlonzoEra -> [(TxIn, Gesture, Cardano.Api.Value, GameDatum)]
filterUtxos (GameRedeemer (myPk, mySalt) (theirPk, theirSalt)) (UTxO utxos) =
  foldl step [] (Map.toList utxos)
  where
    step acc (txOutRef, TxOut _ txOutValue (TxOutDatumHash _ dh) _)
      | Just (gesture, datum) <- extractGesture myPk mySalt dh = (txOutRef, gesture, txOutValueToValue txOutValue, datum) : acc
      | Just (gesture, datum) <- extractGesture theirPk theirSalt dh = acc ++ [(txOutRef, gesture, txOutValueToValue txOutValue, datum)]
      | otherwise = acc
    step acc _ = acc

    extractGesture :: Ledger.PubKeyHash -> BuiltinByteString -> Hash ScriptData -> Maybe (Gesture, GameDatum)
    extractGesture key salt dh =
      listToMaybe
        [ (gesture, datum)
        | gesture <- [minBound .. maxBound]
        , let datum = buildDatum (UserInput.PlayParams gesture salt) key
        , hashScriptData (fromPlutusData $ builtinDataToData (toBuiltinData datum)) == dh
        ]

-- This is probably not the best solution. Since it is an orphan.
instance ToJSON BSI.ByteString where
  toJSON bs = toJSON $ decodeUtf8 $ fromStrict bs
