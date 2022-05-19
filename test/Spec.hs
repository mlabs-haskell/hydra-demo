module Main (main) where

import OffChain.Test (offChainTests)
import Test.Tasty qualified as Tasty
import Prelude

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Hydra-demo"
    [offChainTests]
