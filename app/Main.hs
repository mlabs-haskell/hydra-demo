module Main where

import System.Environment (getArgs)

import Network.Wai.Handler.Warp (run)
import Network.WebSockets (runClient)
import Servant

type TopLevelAPI = Raw

topLevelAPI :: Proxy TopLevelAPI
topLevelAPI = Proxy

topLevelServer :: Server TopLevelAPI
topLevelServer = serveDirectoryFileServer "assets"

main :: IO ()
main = do
  appPort : nodeHost : nodePort : _ <- getArgs
  runClient nodeHost (read nodePort) "/" $ \_hydraConnection ->
    run (read appPort) $ serve topLevelAPI topLevelServer
