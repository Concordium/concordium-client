module SimpleClientTests.CliSpec where

import Test.Hspec hiding (pending)

import qualified SimpleClientTests.TransactionSpec as TS

tests :: Spec
tests = do
  TS.awaitStateTests
  TS.printTransactionStatusTests

