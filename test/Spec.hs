module Main where

import Test.Hspec.QuickCheck
import Test.Hspec

import qualified SimpleClientTests.CliSpec

main :: IO ()
main = hspec $ parallel $ do
  SimpleClientTests.CliSpec.tests
