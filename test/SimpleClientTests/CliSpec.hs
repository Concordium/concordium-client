module SimpleClientTests.CliSpec where

import Test.Hspec hiding (pending)

import SimpleClientTests.AccountSpec
import SimpleClientTests.BackupSpec
import SimpleClientTests.BlockSpec
import SimpleClientTests.ConfigSpec
import SimpleClientTests.ConsensusSpec
import SimpleClientTests.ContractSpec
import SimpleClientTests.LogSpec
import SimpleClientTests.ParseSpec
import SimpleClientTests.SchemaParsingSpec
import SimpleClientTests.TokenAmountSpec
import SimpleClientTests.TransactionSpec

tests :: Spec
tests = do
    backupSpec
    configSpec
    contractSpec
    transactionSpec
    accountSpec
    consensusSpec
    blockSpec
    logSpec
    parseSpec
    schemaParsingSpec
    tokenAmountSpec
