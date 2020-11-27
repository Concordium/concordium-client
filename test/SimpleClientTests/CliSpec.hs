module SimpleClientTests.CliSpec where

import Test.Hspec hiding (pending)

import SimpleClientTests.ConfigSpec
import SimpleClientTests.ContractSpec
import SimpleClientTests.TransactionSpec
import SimpleClientTests.AccountSpec
import SimpleClientTests.ConsensusSpec
import SimpleClientTests.BlockSpec
import SimpleClientTests.LogSpec
import SimpleClientTests.ParseSpec

tests :: Spec
tests = do
  configSpec
  contractSpec
  transactionSpec
  accountSpec
  consensusSpec
  blockSpec
  logSpec
  parseSpec
