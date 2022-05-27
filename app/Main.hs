{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main, ClientInput (NewTxCBOR)) where

import Cardano.Api (AlonzoEra, UTxO, Tx, serialiseToCBOR)
import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Exception (SomeException, displayException, fromException, try)
import Data.Aeson qualified as Aeson (ToJSON, encode, toJSON)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.IO qualified as Text (putStrLn)
import GHC.Generics (Generic)
import Network.WebSockets (Connection, ConnectionException, receiveData, sendTextData, runClient)
import System.Environment (getArgs)
import System.IO.Error (isEOFError)

import Prelude

main :: IO ()
main = do
  nodeHost : nodePort : _ <- getArgs
  runClient nodeHost (read nodePort) "/" $ \ws -> do
    events <- newChan
    void $ forkIO $ wsReceiver ws events
    void $ forkIO $ commandReader events
    eventProcessor ws events

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
    WsError ConnectionException
  | WsOutput Text
  | UserCommand UserCommand

data UserCommand = SendInit | Exit

wsReceiver :: Connection -> Chan AppEvent -> IO ()
wsReceiver ws chan = go
  where
    go = do
      result <- try $ receiveData ws
      case result of
        Left e -> writeChan chan (WsError e)
        Right o -> do
          writeChan chan (WsOutput o)
          go

commandReader :: Chan AppEvent -> IO ()
commandReader chan = go
  where
    go = do
      result <- try @SomeException $ getLine
      case result of
        Left ex -> do
          case fromException ex of
            Just io | isEOFError io -> return ()
            _ -> putStrLn $ "input error: " <> displayException ex
          submit Exit

        Right command -> case command of
          "exit" -> submit Exit
          "init" -> submit SendInit >> go
          cmd -> do
            putStrLn $ "input: unknown command " <> cmd
            go

    submit = writeChan chan . UserCommand

eventProcessor :: Connection -> Chan AppEvent -> IO ()
eventProcessor ws events = go
  where
    go = do
      event <- readChan events
      case event of
        WsOutput serverOutput -> do
          Text.putStrLn ("node output: " <> serverOutput)
          go
        WsError e -> putStrLn $ "node connection error: " <> show e
        UserCommand command ->
          case command of
            Exit -> return ()
            SendInit -> do
              sendTextData ws $ Aeson.encode $ Init 10
              go
