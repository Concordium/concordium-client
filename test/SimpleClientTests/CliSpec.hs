module SimpleClientTests.CliSpec where

import Test.Hspec hiding (pending)

import SimpleClientTests.AccountSpec
import SimpleClientTests.ConfigSpec
import SimpleClientTests.LogSpec
import SimpleClientTests.TransactionSpec

tests :: Spec
tests = do
  accountSpec
  configSpec
  transactionSpec
  logSpec
