module Main (main) where

import Cardano.Api (
  AsType (AsAddressInEra, AsAlonzoEra, AsPaymentKey, AsSigningKey),
  Lovelace (..),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  SerialiseAddress (deserialiseAddress, serialiseAddress),
  UTxO (UTxO),
  readFileTextEnvelope,
 )

import Control.Concurrent.Async (AsyncCancelled (AsyncCancelled))
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (when)
import Data.Aeson (FromJSON, Result (Success), Value (String), eitherDecodeFileStrict', fromJSON)
import Data.Map qualified as Map (singleton)
import Data.String (fromString)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.IO qualified as Text (putStrLn)
import HydraRPS.App (HeadState (..), UserCommand (..), UserCredentials, mkUserCredentials, withApiClient)
import HydraRPS.Tx (parseTxIn, txOutToAddress)
import HydraRPS.UserInput qualified as UserInput
import Ledger qualified (PubKeyHash (PubKeyHash))
import System.Environment (getArgs)
import System.IO.Error (isEOFError)
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

unexpectedInputException :: SomeException -> Bool
unexpectedInputException ex
  | Just io <- fromException ex, isEOFError io = False
  | Just AsyncCancelled <- fromException ex = False
  | otherwise = True
