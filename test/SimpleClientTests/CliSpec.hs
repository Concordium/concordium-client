module SimpleClientTests.CliSpec where

import Test.Hspec hiding (pending)

import SimpleClientTests.AccountSpec
import SimpleClientTests.TransactionSpec

tests :: Spec
tests = do
  accountSpec
  transactionSpec
