module SimpleClientTests.TransactionSpec where

import Concordium.Client.Cli
import Concordium.Client.Output
import Concordium.Client.Runner
import qualified Concordium.Types as Types
import qualified Concordium.Types.Transactions as Types

import SimpleClientTests.QueryTransaction

import Control.Monad.Writer

import Test.Hspec hiding (pending)

exampleTransactionHash :: Types.TransactionHash
exampleTransactionHash = read "c20911f59cda41c116f528531e815a3b561861b96014b379e8a52f1cbbafd2e4"

outcomeSuccess1a :: TransactionStatusResultItem
outcomeSuccess1a = TransactionStatusResultItem
                   { tsriBlockHash = read "0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"
                   , tsriResult = "success"
                   , tsriEvents = []
                   , tsriExecutionEnergyCost = 10
                   , tsriExecutionCost = 10 }

outcomeSuccess1b :: TransactionStatusResultItem
outcomeSuccess1b = TransactionStatusResultItem
                   { tsriBlockHash = read "0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
                   , tsriResult = "success"
                   , tsriEvents = []
                   , tsriExecutionEnergyCost = 10
                   , tsriExecutionCost = 10 }

outcomeSuccess2 :: TransactionStatusResultItem
outcomeSuccess2 = TransactionStatusResultItem
                  { tsriBlockHash = read "941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"
                  , tsriResult = "success"
                  , tsriEvents = []
                  , tsriExecutionEnergyCost = 20
                  , tsriExecutionCost = 20 }

outcomeFailure :: TransactionStatusResultItem
outcomeFailure = TransactionStatusResultItem
                 { tsriBlockHash = read "be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994"
                 , tsriResult = "failure"
                 , tsriEvents = []
                 , tsriExecutionEnergyCost = 20
                 , tsriExecutionCost = 20 }

pending :: TransactionStatusResult
pending = TransactionStatusResult { tsrState = Pending, tsrHash = exampleTransactionHash, tsrResults = [] }

absent :: TransactionStatusResult
absent = TransactionStatusResult { tsrState = Absent, tsrHash = exampleTransactionHash, tsrResults = [] }

committedWithOutcomes :: [TransactionStatusResultItem] -> TransactionStatusResult
committedWithOutcomes rs = TransactionStatusResult { tsrState = Committed, tsrHash = exampleTransactionHash, tsrResults = rs }

finalizedWithOutcome :: TransactionStatusResultItem -> TransactionStatusResult
finalizedWithOutcome r = TransactionStatusResult { tsrState = Finalized, tsrHash = exampleTransactionHash, tsrResults = [r] }

committedOneSuccessfulOutcome :: TransactionStatusResult
committedOneSuccessfulOutcome = committedWithOutcomes [outcomeSuccess1a]

committedOneFailedOutcome :: TransactionStatusResult
committedOneFailedOutcome = committedWithOutcomes [outcomeFailure]

committedTwoIdenticalSuccessfulOutcomes :: TransactionStatusResult
committedTwoIdenticalSuccessfulOutcomes = committedWithOutcomes [outcomeSuccess1a, outcomeSuccess1b]

committedTwoDifferentSuccessfulOutcomes :: TransactionStatusResult
committedTwoDifferentSuccessfulOutcomes = committedWithOutcomes [outcomeSuccess1a, outcomeSuccess2]

committedSuccessfulAndFailureOutcomes :: TransactionStatusResult
committedSuccessfulAndFailureOutcomes = committedWithOutcomes [outcomeSuccess1a, outcomeFailure]

finalized :: TransactionStatusResult
finalized = finalizedWithOutcome outcomeSuccess1a

initQuery :: TransactionState -> TestTransactionStatusQuery TransactionStatusResult
initQuery s = awaitState 0 s exampleTransactionHash

initState :: [TransactionStatusResult] -> ([TransactionStatusResult], Int)
initState rs = (rs, 0)

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

    describe "waits if state is pending" $ do
      let s = initState [pending, committedOneSuccessfulOutcome]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` committedOneSuccessfulOutcome
      specify "wait called once" $ waitCount `shouldBe` 1

    describe "waits if state is pending twice" $ do
      let s = initState [pending, pending, committedOneSuccessfulOutcome]

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

    describe "waits while state is pending or committed" $ do
      let s = initState [pending, committedOneSuccessfulOutcome, pending, finalized]

      let (finalState, (_, waitCount)) = runQuery q s
      specify "correct final state" $ finalState `shouldBe` finalized
      specify "wait called 3 times" $ waitCount `shouldBe` 3

printTransactionStatusTests :: Spec
printTransactionStatusTests = describe "print transaction status" $ do
  -- Expected cases.
  describe "pending" $ do
    specify "correct output" $
      p pending `shouldBe`
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
        [ "Transaction is committed into block be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"failure\" and cost 20 GTU (20 NRG)." ]
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
        , "- be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"failure\" and cost 20 GTU (20 NRG)." ]
  describe "committed into two blocks with different outcomes" $
    specify "correct output" $
      p committedSuccessfulAndFailureOutcomes `shouldBe`
        [ "Transaction is committed into the following 2 blocks:"
        , "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 10 GTU (10 NRG)."
        , "- be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"failure\" and cost 20 GTU (20 NRG)." ]
  describe "finalized with single outcome" $
    specify "correct output" $
      p finalized `shouldBe`
        [ "Transaction is finalized into block 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 10 GTU (10 NRG)." ]
  -- Unexpected cases.
  describe "committed with no outcomes" $
    specify "correct output" $
      p TransactionStatusResult { tsrHash = exampleTransactionHash, tsrState = Committed, tsrResults = [] } `shouldBe`
        [ "Transaction is committed (no block information received)." ]
  describe "finalized with no outcomes" $
    specify "correct output" $
      p TransactionStatusResult { tsrHash = exampleTransactionHash, tsrState = Finalized, tsrResults = [] } `shouldBe`
        [ "Transaction is finalized (no block information received)." ]
  describe "finalized with multiple outcomes" $
    specify "correct output" $
      p TransactionStatusResult { tsrHash = exampleTransactionHash, tsrState = Finalized, tsrResults = [outcomeSuccess1a, outcomeSuccess1b]} `shouldBe`
        [ "Transaction is finalized into multiple blocks - this is very unexpected!" ]
  where p = execWriter . printTransactionStatus
