module Main (main) where

import Test.Tasty qualified as Tasty

import Prelude

import EndToEnd.Spec (headTests)
import OffChain.Test (offChainTests)
import Tx.Spec qualified as Tx (tests)

main :: IO ()
main = do
  spec <- headTests
  Tasty.defaultMain $
    Tasty.testGroup
      "Hydra-demo"
      [ offChainTests
      , Tx.tests
      , spec
      ]
