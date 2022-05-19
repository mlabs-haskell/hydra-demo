module Main (main) where

import Test.Tasty qualified as Tasty
import OffChain.Test (offChainTests)
import Prelude

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Hydra-demo"
    [ offChainTests ]
