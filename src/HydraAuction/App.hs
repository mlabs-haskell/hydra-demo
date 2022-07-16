{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module HydraAuction.App (EnqueueCommand, HeadState (..), UserCommand (..), betConstant, withApiClient, UserCredentials, mkUserCredentials) where

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
  txOutValueToValue, AssetName (AssetName), SerialiseAsRawBytes (deserialiseFromRawBytes), AsType (AsPolicyId), selectLovelace, lovelaceToValue
 )
import Cardano.Api qualified (Value)
import Cardano.Api.Shelley (ProtocolParameters (protocolParamMaxTxExUnits))
import Control.Concurrent (newChan, readChan, writeChan)
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (ToJSON, eitherDecode, encode, (.:))
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map qualified as Map (lookupMin, toList)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as Text (putStrLn)
import Ledger qualified (Address (Address), PubKeyHash)
import Ledger.Tx.CardanoAPI (fromCardanoPaymentKeyHash, toCardanoAddress)
import Network.WebSockets (ConnectionException, receiveData, runClient, sendTextData)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential), CurrencySymbol (unCurrencySymbol), fromBuiltin, TokenName (unTokenName))
import PlutusTx (ToData (toBuiltinData))

import HydraAuction.Node.Command qualified as NodeCommand
import HydraAuction.OnChain qualified as OnChain
import HydraAuction.OnChain (
  Auction(..),
  AuctionDatum (..),
  AuctionAction (MkBid),
  auctionValidator,
  auctionAddress, Bid (bBid, bBidder)
 )
import HydraAuction.Tx (
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
  utxosAt, nftValue
 )
import HydraAuction.UserInput qualified as UserInput

import Prelude
import Ledger (PaymentPubKeyHash(PaymentPubKeyHash, unPaymentPubKeyHash))

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
  | Start UserInput.StartParams
  | Bid UserInput.BidParams
  | Close Auction

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
          let json = encode input
          liftIO $ Text.putStrLn $ "client input:\n" <> decodeUtf8 json <> "\n"
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
                  auction utxo
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

    auction :: UTxO AlonzoEra -> m ()
    auction utxo = do
      event <- liftIO nextEvent
      case event of
        ApiEvent apiEvent ->
          case apiEvent of
            Just serverOutput -> do
              case serverOutput of
                Other tag -> do
                  liftIO $ Text.putStrLn $ "decoded node output: " <> tag <> "\n"
                  auction utxo
                HeadIsOpen unexpectedUtxo -> do
                  liftIO $ putStrLn $ "unexpected head opening (head is already opened)\n" <> show unexpectedUtxo <> "\n"
                  auction utxo
                SnapshotConfirmed updatedUtxo -> do
                  liftIO $ putStrLn $ "snapshot confirmed\n" <> show updatedUtxo <> "\n"
                  auction updatedUtxo
                HeadIsClosed -> do
                  liftIO $ putStrLn "closing the head\n"
                  waitForFanout
                ReadyToFanout -> do
                  liftIO $ putStrLn "unexpected ready to fanout (head is not closed yet)\n"
                  auction utxo
                HeadIsFinalized unexpectedUtxo -> do
                  liftIO $ putStrLn $ "unexpected head finalization (head is not closed yet)\n" <> show unexpectedUtxo <> "\n"
                  auction utxo
            Nothing -> return ()
        UserCommand command ->
          case command of
            Exit -> return ()
            CloseHead -> submit NodeCommand.Close >> auction utxo
            IssueFanout -> do
              liftIO $ putStrLn "head is not closed yet\n"
              auction utxo
            Start sp -> do
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
                  auction utxo
                Just (refs, total) -> do
                  let unsignedTx = either error id $ buildAuctionTx refs total state sp
                      signedTx = signTx state.hsUserCredentials.userSkey unsignedTx
                  submit (NodeCommand.newTx signedTx)
                  auction utxo
            Bid bp -> do
              state <- ask
              let redeemer = OnChain.MkBid (OnChain.Bid (PaymentPubKeyHash state.hsUserCredentials.userPubKeyHash) bp.bpBid)
                  txResult = do
                    collateralTxIn <-
                      maybe (Left "could not find collateral") (Right . fst) $
                        Map.lookupMin $
                          unUTxO $
                            utxosAt state.hsUserCredentials.userAddress utxo
                    scriptAddress <- first (("toCardanoAddress: " <>) . show) $ toCardanoAddress state.hsNetworkId auctionAddress
                    let betUtxos = utxosAt scriptAddress utxo
                    unsignedTx <- buildClaimTx collateralTxIn state (filterUtxos redeemer betUtxos) redeemer
                    pure $ signTx state.hsUserCredentials.userSkey unsignedTx
              case txResult of
                Left err -> liftIO $ putStrLn $ "claim tx building failed: " <> err
                Right signedTx -> submit (NodeCommand.newTx signedTx)
              auction utxo
            Close auc -> do
              state <- ask
              let redeemer = OnChain.Close
                  txResult = do
                    collateralTxIn <-
                      maybe (Left "could not find collateral") (Right . fst) $
                        Map.lookupMin $
                          unUTxO $
                            utxosAt state.hsUserCredentials.userAddress utxo
                    scriptAddress <- first (("toCardanoAddress: " <>) . show) $ toCardanoAddress state.hsNetworkId auctionAddress
                    let betUtxos = utxosAt scriptAddress utxo
                    unsignedTx <- buildClaimTx collateralTxIn state (filterUtxos redeemer betUtxos) redeemer
                    pure $ signTx state.hsUserCredentials.userSkey unsignedTx
              case txResult of
                Left err -> liftIO $ putStrLn $ "claim tx building failed: " <> err
                Right signedTx -> submit (NodeCommand.newTx signedTx)
              auction utxo
            _ -> do
              liftIO $ putStrLn "head is already opened"
              auction utxo

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

buildDatum :: UserInput.StartParams -> Maybe Bid -> Ledger.PubKeyHash -> AuctionDatum
buildDatum startParams bid pkh =
  AuctionDatum
    { adAuction = Auction (PaymentPubKeyHash pkh) startParams.spDeadline startParams.spMinBid startParams.spCurrency startParams.spToken
    , adHighestBid = bid
    }

-- precondition: minBid >= betConstant
buildAuctionTx :: [TxIn] -> Lovelace -> HeadState -> UserInput.StartParams -> Either String (TxBody AlonzoEra)
buildAuctionTx inputRefs inputTotal state startParams = do
  let changeAddress = state.hsUserCredentials.userAddress
      datum = buildDatum startParams Nothing (state.hsUserCredentials.userPubKeyHash)
      assetName = AssetName $ fromBuiltin $ unTokenName startParams.spToken
  policyId <- maybeToRight "Could not parse CurrencySymbol" $ deserialiseFromRawBytes AsPolicyId $ fromBuiltin $ unCurrencySymbol startParams.spCurrency
  let auctionValue = nftValue policyId assetName
      minLove = selectLovelace auctionValue
  scriptOut <-
    first (("bad address specifier: " <>) . show) $
      txOutToScript state.hsNetworkId auctionAddress auctionValue (TxDatum datum)
  let changeOut
        | inputTotal > minLove = [txOutToAddress changeAddress (inputTotal - minLove)]
        | otherwise = []
      bodyContent =
        baseBodyContent
          { txIns = txInForSpending <$> inputRefs
          , txOuts = scriptOut : changeOut
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

-- (OnChain.Bid (PaymentPubKeyHash state.hsUserCredentials.userPubKeyHash) startParams.spMinBid)

buildBidTx :: TxIn -> HeadState -> AuctionDatum -> UserInput.BidParams -> TxIn -> Either String (TxBody AlonzoEra)
buildBidTx collateralTxIn state bidParams auction nftTxIn = do
  let nftPaidTo = case auction.adHighestBid of
        Nothing -> unPaymentPubKeyHash auction.adAuction.aSeller
        Just bid -> unPaymentPubKeyHash bid.bBidder
      assetName = AssetName $ fromBuiltin $ unTokenName auction.adAuction.aToken
  policyId <- maybeToRight "Could not parse CurrencySymbol" $ deserialiseFromRawBytes AsPolicyId $ fromBuiltin $ unCurrencySymbol auction.adAuction.aCurrency
  refundAddress <-
    first (("bad winner pub key hash: " <>) . show) $
      toCardanoAddress state.hsNetworkId $
        Ledger.Address (PubKeyCredential nftPaidTo) Nothing
  scriptAddress <-
    first (("bad winner pub key hash: " <>) . show) $
      toCardanoAddress state.hsNetworkId auctionAddress
  let nft = nftValue policyId assetName <> lovelaceToValue (Lovelace bidParams.bpBid)
      maxTxExUnits =
        fromMaybe ExecutionUnits {executionSteps = 0, executionMemory = 0} $
          protocolParamMaxTxExUnits state.hsProtocolParams
      exUnits =
        ExecutionUnits
          { executionSteps = executionSteps maxTxExUnits `div` 2
          , executionMemory = executionMemory maxTxExUnits `div` 2
          }
      outputs = case auction.adHighestBid of
        Nothing  -> [txOutValueToAddress scriptAddress nft]
        Just bid -> [ txOutValueToAddress scriptAddress nft
                   , txOutValueToAddress refundAddress $ lovelaceToValue $ Lovelace bid.bBid
                   ]

  validatorTxIn <-
    first (("bad script: " <>) . show) $
      txInForValidator nftTxIn auctionValidator (TxDatum AuctionDatum) (TxRedeemer OnChain.Close) exUnits

  let bodyContent =
        baseBodyContent
          { txIns = [validatorTxIn]
          , txInsCollateral = TxInsCollateral CollateralInAlonzoEra [collateralTxIn]
          , txOuts = outputs
          , txProtocolParams = BuildTxWith (Just state.hsProtocolParams)
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent
buildBidTx _ _ _ _ _ = Left "bad input parameters"

buildClaimTx :: TxIn -> HeadState -> AuctionDatum -> TxIn -> Either String (TxBody AlonzoEra)
buildClaimTx collateralTxIn state auction nftTxIn = do
  let nftPaidTo = case auction.adHighestBid of
        Nothing -> unPaymentPubKeyHash auction.adAuction.aSeller
        Just bid -> unPaymentPubKeyHash bid.bBidder
      assetName = AssetName $ fromBuiltin $ unTokenName auction.adAuction.aToken
  policyId <- maybeToRight "Could not parse CurrencySymbol" $ deserialiseFromRawBytes AsPolicyId $ fromBuiltin $ unCurrencySymbol auction.adAuction.aCurrency
  nftToAddress <-
    first (("bad winner pub key hash: " <>) . show) $
      toCardanoAddress state.hsNetworkId $
        Ledger.Address (PubKeyCredential nftPaidTo) Nothing
  sellerAddress <-
    first (("bad seller pub key hash: " <>) . show) $
      toCardanoAddress state.hsNetworkId $
        Ledger.Address (PubKeyCredential $ unPaymentPubKeyHash auction.adAuction.aSeller) Nothing
  let nft = nftValue policyId assetName
      maxTxExUnits =
        fromMaybe ExecutionUnits {executionSteps = 0, executionMemory = 0} $
          protocolParamMaxTxExUnits state.hsProtocolParams
      exUnits =
        ExecutionUnits
          { executionSteps = executionSteps maxTxExUnits `div` 2
          , executionMemory = executionMemory maxTxExUnits `div` 2
          }
      outputs = case auction.adHighestBid of
        Nothing  -> [txOutValueToAddress sellerAddress nft]
        Just bid -> [ txOutValueToAddress nftToAddress nft
                   , txOutValueToAddress sellerAddress $ lovelaceToValue $ Lovelace bid.bBid
                   ]

  validatorTxIn <-
    first (("bad script: " <>) . show) $
      txInForValidator nftTxIn auctionValidator (TxDatum AuctionDatum) (TxRedeemer OnChain.Close) exUnits

  let bodyContent =
        baseBodyContent
          { txIns = [validatorTxIn]
          , txInsCollateral = TxInsCollateral CollateralInAlonzoEra [collateralTxIn]
          , txOuts = outputs
          , txProtocolParams = BuildTxWith (Just state.hsProtocolParams)
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent
buildClaimTx _ _ _ _ = Left "bad input parameters"

-- FIXME: Since there is no easy way to determine the last person  that bid on the nft,
-- the contract should be upgraded to V2 so that we can inline the datum.
filterUtxos :: Auction -> UTxO AlonzoEra -> [(TxIn, Cardano.Api.Value, AuctionDatum)]
filterUtxos auction (UTxO utxos) =
  foldl step [] (Map.toList utxos)
  where
    step acc (txOutRef, TxOut _ txOutValue (TxOutDatumHash _ dh) _)
      | Just datum <- extractAuctionAction myPk mySalt dh = (txOutRef, txOutValueToValue txOutValue, datum) : acc
      | otherwise = acc
    step acc _ = acc