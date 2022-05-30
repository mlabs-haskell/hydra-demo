{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main, ClientInput (NewTxCBOR)) where

import Cardano.Api (AlonzoEra, AsType (AsAddressInEra, AsAlonzoEra), Lovelace (Lovelace), UTxO (UTxO), Tx, deserialiseAddress, serialiseToCBOR)
import Control.Concurrent (forkIO, newChan, readChan, writeChan)
import Control.Exception (SomeException, displayException, fromException, try)
import Data.Aeson qualified as Aeson (ToJSON, encode, toJSON)
import Data.Functor (void)
import Data.Map qualified as Map (singleton)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.IO qualified as Text (putStrLn)
import GHC.Generics (Generic)
import Network.WebSockets (ConnectionException, receiveData, sendTextData, runClient)
import System.Environment (getArgs)
import System.IO.Error (isEOFError)

import HydraRPS.Tx (parseTxIn, txOutToAddress)

import Prelude

main :: IO ()
main = do
  nodeHost : nodePort : _ <- getArgs
  runClient nodeHost (read nodePort) "/" $ \ws -> do
    events <- newChan
    void $ forkIO $ apiReader (receiveData ws) (writeChan events . ApiEvent)
    void $ forkIO $ commandReader (writeChan events . UserCommand)
    eventProcessor (sendTextData ws . Aeson.encode) (readChan events)

data ClientInput
  = Init {contestationPeriod :: Int}
  | Abort
  | Commit {utxo :: UTxO AlonzoEra}
  | NewTx {transaction :: TxCBOR}
  | GetUTxO
  | Close
  | Contest
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)

pattern NewTxCBOR :: Tx AlonzoEra -> ClientInput
pattern NewTxCBOR tx = NewTx (TxCBOR tx)

newtype TxCBOR = TxCBOR {getTx :: Tx AlonzoEra}

instance Aeson.ToJSON TxCBOR where
  toJSON = Aeson.toJSON . serialiseToCBOR . getTx

data AppEvent =
    ApiEvent (Maybe Text)
  | UserCommand UserCommand

type ApiEvent = Maybe Text
data UserCommand =
    InitHead Int
  | CommitToHead (UTxO AlonzoEra)
  | Exit
  | AbortHead
  | CloseHead

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
          ["init", str] | [(period, "")] <- reads str -> do
            enqueue $ InitHead period
            go
          ["commit", txInStr, addrStr, lovelaceStr]
            | Right txIn <- parseTxIn (fromString txInStr)
            , Just addr <- deserialiseAddress (AsAddressInEra AsAlonzoEra) (fromString addrStr)
            , [(lovelace, "")] <- reads lovelaceStr -> do
              enqueue $ CommitToHead $ UTxO $ Map.singleton txIn (txOutToAddress addr (Lovelace lovelace))
              go
          cmd -> do
            putStrLn $ "input: unknown command " <> show cmd
            go

eventProcessor :: (ClientInput -> IO ()) -> IO AppEvent -> IO ()
eventProcessor submitCommand nextEvent = go
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
            AbortHead -> submitCommand Abort >> go
            CloseHead -> submitCommand Close >> go
            InitHead period -> do
              submitCommand $ Init period
              go
            CommitToHead utxoToCommit -> do
              submitCommand $ Commit utxoToCommit
              go
