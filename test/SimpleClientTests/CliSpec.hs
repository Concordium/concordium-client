module SimpleClientTests.CliSpec where

import Test.Hspec hiding (pending)

import SimpleClientTests.ConfigSpec
import SimpleClientTests.TransactionSpec
import SimpleClientTests.AccountSpec
import SimpleClientTests.ConsensusSpec
import SimpleClientTests.BlockSpec
import SimpleClientTests.LogSpec

tests :: Spec
tests = do
  configSpec
  transactionSpec
  accountSpec
  consensusSpec
  blockSpec
  logSpec
