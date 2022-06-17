{-# OPTIONS_GHC -w #-}
module Main (main) where

import Cardano.Api (
  AddressInEra,
  AlonzoEra,
  AsType (AsAddressInEra, AsAlonzoEra, AsPaymentKey, AsSigningKey),
  BuildTxWith (BuildTxWith),
  CollateralSupportedInEra (CollateralInAlonzoEra),
  ExecutionUnits (ExecutionUnits, executionMemory, executionSteps),
  Hash,
  Key (getVerificationKey, verificationKeyHash),
  Lovelace (..),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  ScriptData,
  SerialiseAddress (deserialiseAddress, serialiseAddress),
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
  readFileTextEnvelope,
  txOutValueToValue,
  valueToLovelace,
 )
import Cardano.Api qualified (Value)
import Cardano.Api.Shelley (ProtocolParameters (protocolParamMaxTxExUnits))
import Control.Concurrent (newChan, readChan, writeChan , Chan)
import Control.Concurrent.Async (AsyncCancelled (AsyncCancelled), withAsync, async)
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Aeson (FromJSON, Result (Success), ToJSON, Value (String), eitherDecode, eitherDecodeFileStrict', encode, fromJSON, (.:))
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map (filter, lookupMin, mapMaybe, singleton, toList)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (fromString)
import Data.Text.Lazy (Text, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy.IO qualified as Text (putStrLn)
import Ledger qualified (Address (Address), PubKeyHash (PubKeyHash), toCardanoAPIData)
import Ledger.Tx.CardanoAPI (fromCardanoPaymentKeyHash, toCardanoAddress)
import Network.WebSockets (ConnectionException, receiveData, runClient, sendTextData, Connection)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential))
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude (BuiltinByteString)
import System.Environment (getArgs)
import System.IO.Error (isEOFError)

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
  parseTxIn,
  signTx,
  txInForSpending,
  txInForValidator,
  txOutToAddress,
  txOutToScript,
  txOutValueToAddress,
 )
import HydraRPS.UserInput qualified as UserInput
import HydraRPS.App
import Prelude

main :: IO ()
main = do
  nodeHost : nodePort : keyFile : protocolParamsFile : _ <- getArgs
  skey <- either (fail . show) pure =<< readFileTextEnvelope (AsSigningKey AsPaymentKey) keyFile
  pparamsResult <- either fail pure =<< eitherDecodeFileStrict' protocolParamsFile
  let networkId :: NetworkId
      networkId = Testnet (NetworkMagic 42)

      userCreds :: UserCredentials
      userCreds = mkUserCredentials networkId skey

      headState :: HeadState
      headState = HeadState userCreds networkId pparamsResult
  putStrLn $ "user pubKeyHash " <> show userCreds.userPubKeyHash
  Text.putStrLn $ "user address " <> fromStrict (serialiseAddress userCreds.userAddress) <> "\n"

  withApiClient headState nodeHost (read nodePort) $ \enqueueUserCommand -> do
    commandReader processCLICommand enqueueUserCommand

commandReader :: IO (Maybe UserCommand) -> (UserCommand -> IO ()) -> IO ()
commandReader nextUserEvent enqueue = go
  where
    go = do
      event <- nextUserEvent
      case event of
        Nothing -> go
        Just command -> enqueue command >> go

processCLICommand :: IO (Maybe UserCommand)
processCLICommand = do
  result <- try @SomeException getLine
  case result of
    Left ex -> do
      when (unexpectedInputException ex) $
        putStrLn $ "input error: " <> displayException ex
      pure $ Just Exit
    Right command -> case words command of
      [] -> pure Nothing -- skip empty input for convenience
      ["exit"] -> pure $ Just Exit
      ["abort"] -> pure $ Just AbortHead
      ["close"] -> pure $ Just CloseHead
      ["fanout"] -> pure $ Just IssueFanout 
      ["init", str] | [(period, "")] <- reads str -> do
        putStrLn "parsed init"
        pure $ Just $ InitHead period

      ["commit", txInStr, addrStr, lovelaceStr]
        | Right txIn <- parseTxIn (fromString txInStr)
          , Just addr <- deserialiseAddress (AsAddressInEra AsAlonzoEra) (fromString addrStr)
          , Just lovelace <- parseLovelace lovelaceStr -> do
          pure $ Just $ CommitToHead $ UTxO $ Map.singleton txIn (txOutToAddress addr lovelace)

      ["bet", gestureStr, saltStr]
        | Success gesture <- readFromJsonString gestureStr
          , Success salt <- readFromJsonString saltStr -> do
          pure $ Just $ Bet $ UserInput.PlayParams gesture salt

      ["claim", mySaltStr, theirPkhStr, theirSaltStr]
        | Success mySalt <- readFromJsonString mySaltStr
          , Success theirPkh <- readFromJsonString theirPkhStr
          , Success theirSalt <- readFromJsonString theirSaltStr -> do
          pure $ Just $ Claim $ UserInput.ClaimParams mySalt (Ledger.PubKeyHash theirPkh) theirSalt

      cmd -> do
        putStrLn $ "input: unknown command " <> show cmd
        pure Nothing
  where
    readFromJsonString :: FromJSON a => String -> Result a
    readFromJsonString = fromJSON . String . fromString

    parseLovelace :: String -> Maybe Lovelace
    parseLovelace str
      | [(lovelace, "")] <- reads str = Just (Lovelace lovelace)
      | otherwise = Nothing