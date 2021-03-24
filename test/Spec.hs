module Main where

import Test.Hspec

import qualified DataTests.DoubleWordSpec
import qualified SimpleClientTests.CliSpec

main :: IO ()
main = hspec $ parallel $ do
  SimpleClientTests.CliSpec.tests
  DataTests.DoubleWordSpec.tests
