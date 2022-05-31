module Main (main) where

import Cardano.Api (
  AlonzoEra,
  AsType (AsAddressInEra, AsAlonzoEra, AsPaymentKey, AsSigningKey),
  Lovelace (Lovelace),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  TxIn,
  UTxO (UTxO),
  deserialiseAddress,
  readFileTextEnvelope,
 )
import Control.Concurrent (forkIO, newChan, readChan, writeChan)
import Control.Exception (SomeException, displayException, fromException, try)
import Data.Aeson qualified as Aeson (FromJSON, decodeStrict, encode)
import Data.ByteString.Lazy qualified as ByteString (toStrict)
import Data.Functor (void)
import Data.Map qualified as Map (singleton)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as Text (putStrLn)
import Network.WebSockets (ConnectionException, receiveData, runClient, sendTextData)
import System.Environment (getArgs)
import System.IO.Error (isEOFError)

import HydraRPS.Node.Command qualified as NodeCommand
import HydraRPS.Tx (UserCredentials (..), mkUserCredentials, parseTxIn, txOutToAddress)
import HydraRPS.UserInput qualified as UserInput

import Prelude

main :: IO ()
main = do
  nodeHost : nodePort : keyFile : _ <- getArgs
  skey <- either (fail . show) pure =<< readFileTextEnvelope (AsSigningKey AsPaymentKey) keyFile
  let networkId = Testnet (NetworkMagic 42)
      userCreds = mkUserCredentials networkId skey
  runClient nodeHost (read nodePort) "/" $ \ws -> do
    let nextServerEvent = receiveData ws
        submitCommand input = do
          let json = Aeson.encode input
          Text.putStrLn $ "client input: " <> decodeUtf8 (ByteString.toStrict json)
          sendTextData ws json

    events <- newChan
    let enqueueApiEvent = writeChan events . ApiEvent
        enqueuUserCommand = writeChan events . UserCommand
        nextEvent = readChan events

    void $ forkIO $ apiReader nextServerEvent enqueueApiEvent
    void $ forkIO $ commandReader enqueuUserCommand
    eventProcessor userCreds submitCommand nextEvent

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
  | Bet TxIn UserInput.PlayParams
  | Claim UserInput.ClaimParams

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

commandReader :: (UserCommand -> IO ()) -> IO ()
commandReader enqueue = go
  where
    go = do
      result <- try @SomeException getLine
      case result of
        Left ex -> do
          case fromException ex of
            Just io | isEOFError io -> return ()
            _ -> putStrLn $ "input error: " <> displayException ex
          enqueue Exit
        Right command -> case words command of
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
              , [(lovelace, "")] <- reads lovelaceStr -> do
              enqueue $ CommitToHead $ UTxO $ Map.singleton txIn (txOutToAddress addr (Lovelace lovelace))
              go
          "bet" : txInStr : ppStr
            | Right txIn <- parseTxIn (fromString txInStr)
              , Just pp <- parseJsonArgs ppStr -> do
              enqueue (Bet txIn pp)
              go
          "claim" : cpStr | Just cp <- parseJsonArgs cpStr -> enqueue (Claim cp) >> go
          cmd -> do
            putStrLn $ "input: unknown command " <> show cmd
            go

    parseJsonArgs :: Aeson.FromJSON a => [String] -> Maybe a
    parseJsonArgs = Aeson.decodeStrict . encodeUtf8 . fromString . unwords

eventProcessor :: UserCredentials -> (NodeCommand.Command -> IO ()) -> IO AppEvent -> IO ()
eventProcessor _userCreds submit nextEvent = go
  where
    go = do
      event <- nextEvent
      case event of
        ApiEvent apiEvent ->
          case apiEvent of
            Just serverOutput -> do
              Text.putStrLn ("node output: " <> serverOutput)
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
            Bet _txIn _pp -> putStrLn "no betting yet" >> go
            Claim _cp -> putStrLn "no claiming yet" >> go
