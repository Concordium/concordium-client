module Main where

import Test.Hspec

import qualified DataTests.DoubleWordSpec
import qualified SimpleClientTests.CliSpec
import qualified SimpleClientTests.EncryptionSpec

main :: IO ()
main = hspec $ parallel $ do
  SimpleClientTests.CliSpec.tests
  SimpleClientTests.EncryptionSpec.tests
  DataTests.DoubleWordSpec.tests
