module SimpleClientTests.TransactionSpec where

import Concordium.Client.Cli
import Concordium.Client.Output
import Concordium.Client.Runner
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types

import SimpleClientTests.QueryTransaction

import Control.Monad.Writer
import Data.HashMap.Strict as M
import Data.Text (Text)

import Test.Hspec hiding (pending)

exampleAddress :: Text
exampleAddress = "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"

exampleAccountAddr :: Types.AccountAddress
Right exampleAccountAddr = IDTypes.addressFromText exampleAddress

exampleTransactionHash :: Types.TransactionHash
exampleTransactionHash = read "c20911f59cda41c116f528531e815a3b561861b96014b379e8a52f1cbbafd2e4"

exampleBlockHash1 :: Types.BlockHash
exampleBlockHash1 = read "0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"

exampleBlockHash2 :: Types.BlockHash
exampleBlockHash2 = read "0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"

exampleBlockHash3 :: Types.BlockHash
exampleBlockHash3 = read "941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"

exampleBlockHash4 :: Types.BlockHash
exampleBlockHash4 = read "be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994"

outcomeSuccess1a :: Types.TransactionSummary
outcomeSuccess1a = Types.TransactionSummary
                   { Types.tsSender = exampleAccountAddr
                   , Types.tsHash = exampleTransactionHash
                   , Types.tsCost = 10
                   , Types.tsEnergyCost = 10
                   , Types.tsType = Just Types.TTTransfer
                   , Types.tsResult = Types.TxSuccess { Types.vrEvents = [] }
                   , Types.tsIndex = Types.TransactionIndex 0 }

-- TODO Identical to outcomeSuccess1b now.
outcomeSuccess1b :: Types.TransactionSummary
outcomeSuccess1b = Types.TransactionSummary
                   { Types.tsSender = exampleAccountAddr
                   , Types.tsHash = exampleTransactionHash
                   , Types.tsCost = 10
                   , Types.tsEnergyCost = 10
                   , Types.tsType = Just Types.TTTransfer
                   , Types.tsResult = Types.TxSuccess { Types.vrEvents = [] }
                   , Types.tsIndex = 0 }

outcomeSuccess2 :: Types.TransactionSummary
outcomeSuccess2 = Types.TransactionSummary
                  { Types.tsSender = exampleAccountAddr
                  , Types.tsHash = exampleTransactionHash
                  , Types.tsCost = 20
                  , Types.tsEnergyCost = 20
                  , Types.tsType = Just Types.TTTransfer
                  , Types.tsResult = Types.TxSuccess { Types.vrEvents = [] }
                  , Types.tsIndex = 0 }

outcomeFailure :: Types.TransactionSummary
outcomeFailure = Types.TransactionSummary
                 { Types.tsSender = exampleAccountAddr
                 , Types.tsHash = exampleTransactionHash
                 , Types.tsCost = 20
                 , Types.tsEnergyCost = 20
                 , Types.tsType = Just Types.TTTransfer
                 , Types.tsResult = Types.TxReject { Types.vrRejectReason = Types.InvalidProof }
                 , Types.tsIndex = 0 }

received :: TransactionStatusResult
received = TransactionStatusResult { tsrState = Received, tsrResults = M.empty }

absent :: TransactionStatusResult
absent = TransactionStatusResult { tsrState = Absent, tsrResults = M.empty }

committedWithOutcomes :: TransactionBlockResults -> TransactionStatusResult
committedWithOutcomes rs = TransactionStatusResult { tsrState = Committed, tsrResults = rs }

finalizedWithOutcome :: TransactionBlockResults -> TransactionStatusResult
finalizedWithOutcome rs = TransactionStatusResult { tsrState = Finalized, tsrResults = rs }

committedOneSuccessfulOutcome :: TransactionStatusResult
committedOneSuccessfulOutcome = committedWithOutcomes $ M.singleton exampleBlockHash1 $ Just outcomeSuccess1a

committedOneFailedOutcome :: TransactionStatusResult
committedOneFailedOutcome = committedWithOutcomes $ M.singleton exampleBlockHash4 $ Just outcomeFailure

committedTwoIdenticalSuccessfulOutcomes :: TransactionStatusResult
committedTwoIdenticalSuccessfulOutcomes = committedWithOutcomes $ M.fromList [(exampleBlockHash1, Just outcomeSuccess1a), (exampleBlockHash2, Just outcomeSuccess1b)]

committedTwoDifferentSuccessfulOutcomes :: TransactionStatusResult
committedTwoDifferentSuccessfulOutcomes = committedWithOutcomes $ M.fromList [(exampleBlockHash1, Just outcomeSuccess1a), (exampleBlockHash3, Just outcomeSuccess2)]

committedSuccessfulAndFailureOutcomes :: TransactionStatusResult
committedSuccessfulAndFailureOutcomes = committedWithOutcomes $ M.fromList [(exampleBlockHash1, Just outcomeSuccess1a), (exampleBlockHash4, Just outcomeFailure)]

finalized :: TransactionStatusResult
finalized = finalizedWithOutcome $ M.singleton exampleBlockHash1 $ Just outcomeSuccess1a

initQuery :: TransactionState -> TestTransactionStatusQuery TransactionStatusResult
initQuery s = awaitState 0 s exampleTransactionHash

initState :: [TransactionStatusResult] -> ([TransactionStatusResult], Int)
initState rs = (rs, 0)

transactionSpec :: Spec
transactionSpec = describe "transaction" $ do
  awaitStateTests
  printTransactionStatusTests

awaitStateTests :: Spec
awaitStateTests = describe "await state" $ do
  describe "committed state" $ do
    let q = initQuery Committed

    describe "doesn't wait if state is committed successful" $ do
      let s = initState [committedOneSuccessfulOutcome]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` committedOneSuccessfulOutcome
      specify "wait not called" $ waitCount `shouldBe` 0

    describe "doesn't wait if state is committed failed" $ do
      let s = initState [committedOneFailedOutcome]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` committedOneFailedOutcome
      specify "wait not called" $ waitCount `shouldBe` 0

    describe "doesn't wait if state is absent" $ do
      let s = initState [absent]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` absent
      specify "wait not called" $ waitCount `shouldBe` 0

    describe "doesn't wait if state is finalized" $ do
      let s = initState [finalized]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` finalized
      specify "wait not called" $ waitCount `shouldBe` 0

    describe "waits if state is received" $ do
      let s = initState [received, committedOneSuccessfulOutcome]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` committedOneSuccessfulOutcome
      specify "wait called once" $ waitCount `shouldBe` 1

    describe "waits if state is received twice" $ do
      let s = initState [received, received, committedOneSuccessfulOutcome]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` committedOneSuccessfulOutcome
      specify "wait called twice" $ waitCount `shouldBe` 2

  describe "finalized state" $ do
    let q = initQuery Finalized

    describe "doesn't wait if state is finalized" $ do
      let s = initState [finalized]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` finalized
      specify "wait not called" $ waitCount `shouldBe` 0

    describe "doesn't wait if state is absent" $ do
      let s = initState [absent]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` absent
      specify "wait not called" $ waitCount `shouldBe` 0

    describe "waits while state is received or committed" $ do
      let s = initState [received, committedOneSuccessfulOutcome, received, finalized]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` finalized
      specify "wait called 3 times" $ waitCount `shouldBe` 3

printTransactionStatusTests :: Spec
printTransactionStatusTests = describe "print transaction status" $ do
  -- Expected cases.
  describe "received" $ do
    specify "correct output" $
      p received `shouldBe`
        [ "Transaction is pending." ]
  describe "absent" $ do
    specify "correct output" $
      p absent `shouldBe`
        [ "Transaction is absent." ]
  describe "committed into one block with status 'success'" $
    specify "correct output" $
      p committedOneSuccessfulOutcome `shouldBe`
        [ "Transaction is committed into block 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 10 GTU (10 NRG)." ]
  describe "committed into one block with status 'failed'" $
    specify "correct output" $
      p committedOneFailedOutcome `shouldBe`
        [ "Transaction is committed into block be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"rejected\" and cost 20 GTU (20 NRG)." ]
  describe "committed into two blocks with same (successful) outcome" $
    specify "correct output" $
      p committedTwoIdenticalSuccessfulOutcomes `shouldBe`
        [ "Transaction is committed into the following 2 blocks with status \"success\" and cost 10 GTU (10 NRG):"
        , "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"
        , "- 0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd" ]
  describe "committed into two blocks with different (successful) outcomes" $
    specify "correct output" $
      p committedTwoDifferentSuccessfulOutcomes `shouldBe`
        [ "Transaction is committed into the following 2 blocks:"
        , "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 10 GTU (10 NRG)."
        , "- 941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795 with status \"success\" and cost 20 GTU (20 NRG)." ]
  describe "committed into two blocks with different outcomes" $
    specify "correct output" $
      p committedSuccessfulAndFailureOutcomes `shouldBe`
        [ "Transaction is committed into the following 2 blocks:"
        , "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 10 GTU (10 NRG)."
        , "- be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"rejected\" and cost 20 GTU (20 NRG)." ]
  describe "committed into two blocks with different outcomes" $
    specify "correct output" $
      p committedSuccessfulAndFailureOutcomes `shouldBe`
        [ "Transaction is committed into the following 2 blocks:"
        , "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 10 GTU (10 NRG)."
        , "- be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"rejected\" and cost 20 GTU (20 NRG)." ]
  describe "finalized with single outcome" $
    specify "correct output" $
      p finalized `shouldBe`
        [ "Transaction is finalized into block 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 10 GTU (10 NRG)." ]
  -- Unexpected cases.
  describe "committed with no outcomes" $
    specify "correct output" $
      p TransactionStatusResult { tsrState = Committed, tsrResults = M.empty } `shouldBe`
        [ "Transaction is committed - no block information received (this should never happen!)." ]
  describe "finalized with no outcomes" $
    specify "correct output" $
      p TransactionStatusResult { tsrState = Finalized, tsrResults = M.empty } `shouldBe`
        [ "Transaction is finalized - no block information received (this should never happen!)." ]
  describe "finalized with multiple outcomes" $
    specify "correct output" $
      p TransactionStatusResult { tsrState = Finalized, tsrResults = M.fromList [(exampleBlockHash1, Just outcomeSuccess1a), (exampleBlockHash2, Just outcomeSuccess1b)]} `shouldBe`
        [ "Transaction is finalized into multiple blocks - this should never happen and may indicate a serious problem with the chain!" ]
  where p = execWriter . printTransactionStatus
