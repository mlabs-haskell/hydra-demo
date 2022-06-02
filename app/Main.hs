module Main (main) where

import Cardano.Api (
  AddressInEra,
  AlonzoEra,
  AsType (AsAddressInEra, AsAlonzoEra, AsPaymentKey, AsSigningKey),
  BuildTxWith (BuildTxWith),
  CollateralSupportedInEra (CollateralInAlonzoEra),
  ExecutionUnits (ExecutionUnits, executionMemory, executionSteps),
  Key (getVerificationKey, verificationKeyHash),
  Lovelace (..),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  SerialiseAddress (deserialiseAddress, serialiseAddress),
  SigningKey,
  StakeAddressReference (NoStakeAddress),
  TxBody,
  TxBodyContent (txIns, txInsCollateral, txOuts, txProtocolParams),
  TxIn,
  TxInsCollateral (TxInsCollateral),
  UTxO (UTxO),
  makeShelleyAddressInEra,
  makeTransactionBody,
  readFileTextEnvelope,
 )
import Cardano.Api.Shelley (ProtocolParameters (protocolParamMaxTxExUnits))
import Control.Concurrent (newChan, readChan, writeChan)
import Control.Concurrent.Async (AsyncCancelled (AsyncCancelled), withAsync)
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson qualified as Aeson (FromJSON, ToJSON, decodeStrict, eitherDecodeFileStrict', encode)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LazyByteString (toStrict)
import Data.Kind (Type)
import Data.Map qualified as Map (singleton)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as Text (putStrLn)
import Ledger qualified (PubKeyHash)
import Ledger.Tx.CardanoAPI (fromCardanoPaymentKeyHash)
import Network.WebSockets (ConnectionException, receiveData, runClient, sendTextData)
import System.Environment (getArgs)
import System.IO.Error (isEOFError)

import HydraRPS.Node.Command qualified as NodeCommand
import HydraRPS.OnChain (
  GameDatum (..),
  GameRedeemer (GameRedeemer),
  encryptGesture,
  rpsValidator,
  rpsValidatorAddress,
 )
import HydraRPS.Tx (
  TxDatum (TxDatum),
  TxRedeemer (TxRedeemer),
  baseBodyContent,
  parseTxIn,
  signTx,
  txInForSpending,
  txInForValidator,
  txOutToAddress,
  txOutToScript,
 )
import HydraRPS.UserInput qualified as UserInput

import Prelude

main :: IO ()
main = do
  nodeHost : nodePort : keyFile : protocolParamsFile : _ <- getArgs
  skey <- either (fail . show) pure =<< readFileTextEnvelope (AsSigningKey AsPaymentKey) keyFile
  pparamsResult <- either (fail . show) pure =<< Aeson.eitherDecodeFileStrict' protocolParamsFile
  let networkId :: NetworkId
      networkId = Testnet (NetworkMagic 42)

      userCreds :: UserCredentials
      userCreds = mkUserCredentials networkId skey

      headState :: HeadState
      headState = HeadState userCreds networkId pparamsResult
  putStrLn $ "user pubKeyHash " <> show userCreds.userPubKeyHash
  Text.putStrLn $ "user address " <> serialiseAddress userCreds.userAddress
  putStrLn ""
  runClient nodeHost (read nodePort) "/" $ \ws -> do
    let nextServerEvent :: IO Text
        nextServerEvent = receiveData ws

        submitCommand :: (MonadIO m, Aeson.ToJSON a) => a -> m ()
        submitCommand input = do
          let json = Aeson.encode input
          liftIO $ Text.putStrLn $ "client input:\n" <> decodeUtf8 (LazyByteString.toStrict json) <> "\n"
          liftIO $ sendTextData ws json

    events <- newChan
    let enqueueApiEvent :: Maybe Text -> IO ()
        enqueueApiEvent = writeChan events . ApiEvent

        enqueueUserCommand :: UserCommand -> IO ()
        enqueueUserCommand = writeChan events . UserCommand

        nextEvent :: IO AppEvent
        nextEvent = readChan events

    withAsync (apiReader nextServerEvent enqueueApiEvent) $ \_ ->
      withAsync (commandReader enqueueUserCommand) $ \_ ->
        runInHead headState (eventProcessor submitCommand nextEvent)

data HeadState = HeadState
  { hsUserCredentials :: UserCredentials
  , hsNetworkId :: NetworkId
  , hsProtocolParams :: ProtocolParameters
  }

newtype HeadM a = HeadM {unHeadM :: ReaderT HeadState IO a} deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader HeadState)

runInHead :: HeadState -> HeadM a -> IO a
runInHead c ba = runReaderT (unHeadM ba) c

data AppEvent
  = ApiEvent (Maybe Text)
  | UserCommand UserCommand

type ApiEvent = Maybe Text
data UserCommand
  = InitHead Int
  | CommitToHead (UTxO AlonzoEra)
  | Exit
  | AbortHead
  | CloseHead
  | IssueFanout
  | Bet TxIn Lovelace UserInput.PlayParams
  | Claim TxIn UserInput.ClaimParams

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

apiReader :: IO Text -> (ApiEvent -> IO ()) -> IO ()
apiReader nextServerEvent enqueue = go
  where
    go = do
      result <- try @ConnectionException nextServerEvent
      case result of
        Left e -> do
          putStrLn $ "node connection error: " <> show e
          enqueue Nothing
        Right o -> do
          enqueue $ Just o
          go

unexpectedInputException :: SomeException -> Bool
unexpectedInputException ex
  | Just io <- fromException ex, isEOFError io = False
  | Just AsyncCancelled <- fromException ex = False
  | otherwise = True

commandReader :: (UserCommand -> IO ()) -> IO ()
commandReader enqueue = go
  where
    go = do
      result <- try @SomeException getLine
      case result of
        Left ex -> do
          when (unexpectedInputException ex) $
            putStrLn $ "input error: " <> displayException ex
          enqueue Exit
        Right command -> case words command of
          [] -> go -- skip empty input for convenience
          ["exit"] -> enqueue Exit
          ["abort"] -> enqueue AbortHead >> go
          ["close"] -> enqueue CloseHead >> go
          ["fanout"] -> enqueue IssueFanout >> go
          ["init", str] | [(period, "")] <- reads str -> do
            enqueue $ InitHead period
            go
          ["commit", txInStr, addrStr, lovelaceStr]
            | Right txIn <- parseTxIn (fromString txInStr)
              , Just addr <- deserialiseAddress (AsAddressInEra AsAlonzoEra) (fromString addrStr)
              , Just lovelace <- parseLovelace lovelaceStr -> do
              enqueue $ CommitToHead $ UTxO $ Map.singleton txIn (txOutToAddress addr lovelace)
              go
          "bet" : txInStr : txInLovelaceStr : ppStr
            | Right txIn <- parseTxIn (fromString txInStr)
              , Just txInLovelace <- parseLovelace txInLovelaceStr
              , Just pp <- parseJsonArgs ppStr -> do
              enqueue (Bet txIn txInLovelace pp)
              go
          "claim" : collateralTxInStr : cpStr
            | Right collateralTxIn <- parseTxIn (fromString collateralTxInStr)
              , Just cp <- parseJsonArgs cpStr ->
              enqueue (Claim collateralTxIn cp) >> go
          cmd -> do
            putStrLn $ "input: unknown command " <> show cmd
            go

    parseJsonArgs :: Aeson.FromJSON a => [String] -> Maybe a
    parseJsonArgs = Aeson.decodeStrict . encodeUtf8 . fromString . unwords

    parseLovelace :: String -> Maybe Lovelace
    parseLovelace str
      | [(lovelace, "")] <- reads str = Just (Lovelace lovelace)
      | otherwise = Nothing

eventProcessor :: forall (m :: Type -> Type). (MonadIO m, MonadReader HeadState m) => (NodeCommand.Command -> m ()) -> IO AppEvent -> m ()
eventProcessor submit nextEvent = go
  where
    go :: m ()
    go = do
      event <- liftIO nextEvent
      case event of
        ApiEvent apiEvent ->
          case apiEvent of
            Just serverOutput -> do
              liftIO $ Text.putStrLn ("node output:\n" <> serverOutput <> "\n")
              go
            Nothing -> return ()
        UserCommand command ->
          case command of
            Exit -> return ()
            AbortHead -> submit NodeCommand.Abort >> go
            CloseHead -> submit NodeCommand.Close >> go
            IssueFanout -> submit NodeCommand.Fanout >> go
            InitHead period -> submit (NodeCommand.Init period) >> go
            CommitToHead utxoToCommit -> submit (NodeCommand.Commit utxoToCommit) >> go
            Bet txIn txInLovelace pp -> do
              state <- ask
              let unsignedTx = either error id $ buildBetTx txIn txInLovelace state pp
                  signedTx = mkSignedTx state unsignedTx
              submit (NodeCommand.newTx signedTx)
              go
            Claim collateralTxIn cp -> do
              state <- ask
              let unsignedTx = either error id $ buildClaimTx collateralTxIn state cp
                  signedTx = mkSignedTx state unsignedTx
              submit (NodeCommand.newTx signedTx) >> go

mkSignedTx :: HeadState -> TxBody AlonzoEra -> Tx AlonzoEra
mkSignedTx state = signTx state.hsUserCredentials.userSkey

betConstant :: Lovelace
betConstant = 10000000

buildDatum :: UserInput.PlayParams -> Ledger.PubKeyHash -> GameDatum
buildDatum playParams pkh =
  GameDatum
    { gdGesture = encryptGesture playParams.ppGesture playParams.ppSalt
    , gdPkh = pkh
    }

-- precondition: txInLovelace >= betConstant
buildBetTx :: TxIn -> Lovelace -> HeadState -> UserInput.PlayParams -> Either String (TxBody AlonzoEra)
buildBetTx txIn txInLovelace state playParams = do
  let changeAddress = state.hsUserCredentials.userAddress
      datum = buildDatum playParams state.hsUserCredentials.userPubKeyHash
  scriptOut <-
    first (("bad address specifier: " <>) . show) $
      txOutToScript state.hsNetworkId rpsValidatorAddress betConstant (TxDatum datum)
  let changeOut
        | txInLovelace > betConstant = [txOutToAddress changeAddress (txInLovelace - betConstant)]
        | otherwise = []
      bodyContent =
        baseBodyContent
          { txIns = [txInForSpending txIn]
          , txOuts = scriptOut : changeOut
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent

buildClaimTx :: TxIn -> HeadState -> UserInput.ClaimParams -> Either String (TxBody AlonzoEra)
buildClaimTx collateralTxIn state cp = do
  let myTxIn = cp.myInput.txIn
      myDatum = buildDatum cp.myInput.playParams cp.myInput.pkh
      theirTxIn = cp.theirInput.txIn
      theirDatum = buildDatum cp.theirInput.playParams cp.theirInput.pkh
      redeemer = GameRedeemer (cp.myInput.pkh, cp.myInput.playParams.ppSalt) (cp.theirInput.pkh, cp.theirInput.playParams.ppSalt)
      maxTxExUnits =
        fromMaybe ExecutionUnits {executionSteps = 0, executionMemory = 0} $
          protocolParamMaxTxExUnits state.hsProtocolParams
      exUnits =
        ExecutionUnits
          { executionSteps = executionSteps maxTxExUnits `div` 2
          , executionMemory = executionMemory maxTxExUnits `div` 2
          }
      outAddress = state.hsUserCredentials.userAddress

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
          , txOuts = [txOutToAddress outAddress (2 * betConstant)]
          , txProtocolParams = BuildTxWith (Just state.hsProtocolParams)
          }
  first (("bad tx-body: " <>) . show) $ makeTransactionBody bodyContent
