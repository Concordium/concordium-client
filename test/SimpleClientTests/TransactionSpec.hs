module SimpleClientTests.TransactionSpec where

import Concordium.Client.Output
import Concordium.Client.Runner
import Concordium.Client.Types.Transaction
import Concordium.Client.Types.TransactionStatus

import qualified Concordium.Crypto.SignatureScheme as ID
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.Types.Queries as Types

import SimpleClientTests.QueryTransaction

import Control.Monad.Writer
import qualified Data.Aeson as AE
import Data.ByteString.Short as SBS
import Data.Map.Strict as Map
import Data.Text as Text
import Data.Word (Word8)

import Concordium.Types.Transactions
import Test.Hspec hiding (pending)

exampleAddress1 :: Text
exampleAddress1 = "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"

exampleAccountAddr1 :: Types.AccountAddress
exampleAccountAddr1 = case IDTypes.addressFromText exampleAddress1 of
    Right addr -> addr
    -- This does not happen since the format
    -- of the text is that of a valid address.
    Left err -> error err

exampleAddress2 :: Text
exampleAddress2 = "4MkK65HrYvMauNTHTuL23wRDKp4VXkCiTpmoWYFtsrZHV3WwSa"

exampleAccountAddr2 :: Types.AccountAddress
exampleAccountAddr2 = case IDTypes.addressFromText exampleAddress2 of
    Right addr -> addr
    -- This does not happen since the format
    -- of the text is that of a valid address.
    Left err -> error err

exampleRegisterDataPayload :: Types.Payload
exampleRegisterDataPayload = Types.RegisterData{rdData = Types.RegisteredData exampleShortByteString}

exampleShortByteString :: ShortByteString
exampleShortByteString = SBS.pack ([1, 2] :: [Word8])

exampleSignatureMap :: Map.Map IDTypes.KeyIndex ID.Signature
exampleSignatureMap = Map.singleton 1 (ID.Signature exampleShortByteString)

exampleCredentialSignatureMapEmpty :: Map.Map IDTypes.CredentialIndex (Map.Map IDTypes.KeyIndex ID.Signature)
exampleCredentialSignatureMapEmpty = Map.empty

exampleCredentialSignatureMap :: Map.Map IDTypes.CredentialIndex (Map.Map IDTypes.KeyIndex ID.Signature)
exampleCredentialSignatureMap = Map.insert (1 :: IDTypes.CredentialIndex) exampleSignatureMap exampleCredentialSignatureMapEmpty

exampleSignableTransaction :: SignableTransaction
exampleSignableTransaction =
    SignableTransaction
        { stEnergy = Types.Energy 1,
          stExpiryTime = Types.TransactionTime 2,
          stNonce = Types.Nonce 3,
          stSigner = exampleAccountAddr1,
          stPayload = exampleRegisterDataPayload,
          stSignature = TransactionSignature exampleCredentialSignatureMap,
          stExtended = False,
          stSponsor = Nothing,
          stSponsorSignature = Nothing
        }

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

exampleEvent :: Types.SupplementedEvent
exampleEvent = Types.Transferred (Types.AddressAccount exampleAccountAddr1) 10 (Types.AddressAccount exampleAccountAddr2)

exampleRejectReason :: Types.RejectReason
exampleRejectReason = Types.AmountTooLarge (Types.AddressAccount exampleAccountAddr1) 11

outcomeSuccess1a :: Types.SupplementedTransactionSummary
outcomeSuccess1a =
    Types.SupplementedTransactionSummary
        { Types.stsSender = Just exampleAccountAddr1,
          Types.stsHash = exampleTransactionHash,
          Types.stsCost = 10,
          Types.stsEnergyCost = 10,
          Types.stsType = Types.TSTAccountTransaction $ Just Types.TTTransfer,
          Types.stsResult = Types.TxSuccess{Types.vrEvents = [exampleEvent]},
          Types.stsIndex = Types.TransactionIndex 0,
          Types.stsSponsorDetails = Nothing
        }

outcomeSuccess1b :: Types.SupplementedTransactionSummary
outcomeSuccess1b =
    Types.SupplementedTransactionSummary
        { Types.stsSender = Just exampleAccountAddr1,
          Types.stsHash = exampleTransactionHash,
          Types.stsCost = 10,
          Types.stsEnergyCost = 10,
          Types.stsType = Types.TSTAccountTransaction $ Just Types.TTTransfer,
          Types.stsResult = Types.TxSuccess{Types.vrEvents = [exampleEvent]},
          Types.stsIndex = 0,
          Types.stsSponsorDetails = Nothing
        }

outcomeSuccess2 :: Types.SupplementedTransactionSummary
outcomeSuccess2 =
    Types.SupplementedTransactionSummary
        { Types.stsSender = Just exampleAccountAddr1,
          Types.stsHash = exampleTransactionHash,
          Types.stsCost = 20,
          Types.stsEnergyCost = 20,
          Types.stsType = Types.TSTAccountTransaction $ Just Types.TTTransfer,
          Types.stsResult = Types.TxSuccess{Types.vrEvents = [exampleEvent]},
          Types.stsIndex = 0,
          Types.stsSponsorDetails = Nothing
        }

outcomeFailure :: Types.SupplementedTransactionSummary
outcomeFailure =
    Types.SupplementedTransactionSummary
        { Types.stsSender = Just exampleAccountAddr1,
          Types.stsHash = exampleTransactionHash,
          Types.stsCost = 20,
          Types.stsEnergyCost = 20,
          Types.stsType = Types.TSTAccountTransaction $ Just Types.TTTransfer,
          Types.stsResult = Types.TxReject{Types.vrRejectReason = exampleRejectReason},
          Types.stsIndex = 0,
          Types.stsSponsorDetails = Nothing
        }

received :: TransactionStatusResult
received = TransactionStatusResult{tsrState = Received, tsrResults = Map.empty}

absent :: TransactionStatusResult
absent = TransactionStatusResult{tsrState = Absent, tsrResults = Map.empty}

committedWithOutcomes :: TransactionBlockResults -> TransactionStatusResult
committedWithOutcomes rs = TransactionStatusResult{tsrState = Committed, tsrResults = rs}

finalizedWithOutcome :: TransactionBlockResults -> TransactionStatusResult
finalizedWithOutcome rs = TransactionStatusResult{tsrState = Finalized, tsrResults = rs}

committedOneSuccessfulOutcome :: TransactionStatusResult
committedOneSuccessfulOutcome = committedWithOutcomes $ Map.singleton exampleBlockHash1 outcomeSuccess1a

committedOneFailedOutcome :: TransactionStatusResult
committedOneFailedOutcome = committedWithOutcomes $ Map.singleton exampleBlockHash4 outcomeFailure

committedTwoIdenticalSuccessfulOutcomes :: TransactionStatusResult
committedTwoIdenticalSuccessfulOutcomes = committedWithOutcomes $ Map.fromList [(exampleBlockHash1, outcomeSuccess1a), (exampleBlockHash2, outcomeSuccess1b)]

committedTwoDifferentSuccessfulOutcomes :: TransactionStatusResult
committedTwoDifferentSuccessfulOutcomes = committedWithOutcomes $ Map.fromList [(exampleBlockHash1, outcomeSuccess1a), (exampleBlockHash3, outcomeSuccess2)]

committedSuccessfulAndFailureOutcomes :: TransactionStatusResult
committedSuccessfulAndFailureOutcomes = committedWithOutcomes $ Map.fromList [(exampleBlockHash1, outcomeSuccess1a), (exampleBlockHash4, outcomeFailure)]

finalized :: TransactionStatusResult
finalized = finalizedWithOutcome $ Map.singleton exampleBlockHash1 outcomeSuccess1a

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

        describe "does not wait if state is committed successful" $ do
            let s = initState [committedOneSuccessfulOutcome]

            let (finalState, (_, waitCount)) = runQuery q s
            specify "correct final state" $ finalState `shouldBe` committedOneSuccessfulOutcome
            specify "wait not called" $ waitCount `shouldBe` 0

        describe "does not wait if state is committed failed" $ do
            let s = initState [committedOneFailedOutcome]

            let (finalState, (_, waitCount)) = runQuery q s
            specify "correct final state" $ finalState `shouldBe` committedOneFailedOutcome
            specify "wait not called" $ waitCount `shouldBe` 0

        describe "does not wait if state is absent" $ do
            let s = initState [absent]

            let (finalState, (_, waitCount)) = runQuery q s
            specify "correct final state" $ finalState `shouldBe` absent
            specify "wait not called" $ waitCount `shouldBe` 0

        describe "does not wait if state is finalized" $ do
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

        describe "does not wait if state is finalized" $ do
            let s = initState [finalized]

            let (finalState, (_, waitCount)) = runQuery q s
            specify "correct final state" $ finalState `shouldBe` finalized
            specify "wait not called" $ waitCount `shouldBe` 0

        describe "does not wait if state is absent" $ do
            let s = initState [absent]

            let (finalState, (_, waitCount)) = runQuery q s
            specify "correct final state" $ finalState `shouldBe` absent
            specify "wait not called" $ waitCount `shouldBe` 0

        describe "waits while state is received or committed" $ do
            let s = initState [received, committedOneSuccessfulOutcome, received, finalized]

            let (finalState, (_, waitCount)) = runQuery q s
            specify "correct final state" $ finalState `shouldBe` finalized
            specify "wait called 3 times" $ waitCount `shouldBe` 3

    describe "JSON encoding and decoding" $ do
        specify "for 'SignableTransaction'" $ (AE.eitherDecode . AE.encode $ exampleSignableTransaction) `shouldBe` Right exampleSignableTransaction

printTransactionStatusTests :: Spec
printTransactionStatusTests = describe "print transaction status" $ do
    -- Expected cases.
    describe "received" $ do
        specify "correct output" $
            p received
                `shouldBe` ["Transaction is pending."]
    describe "absent" $ do
        specify "correct output" $
            p absent
                `shouldBe` ["Transaction is absent."]
    describe "committed into one block with status 'success'" $
        specify "correct output" $
            p committedOneSuccessfulOutcome
                `shouldBe` ["Transaction is committed into block 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 0.000010 CCD (10 NRG)."]
    describe "committed into one block with status 'rejected'" $
        specify "correct output" $
            p committedOneFailedOutcome
                `shouldBe` [ "Transaction is committed into block be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"rejected\" and cost 0.000020 CCD (20 NRG).",
                             "Transaction rejected: insufficient funds."
                           ]
    describe "committed into two blocks with same (successful) outcome" $
        specify "correct output" $
            p committedTwoIdenticalSuccessfulOutcomes
                `shouldBe` [ "Transaction is committed into 2 blocks with status \"success\" and cost 0.000010 CCD (10 NRG):",
                             "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389",
                             "- 0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
                           ]
    describe "committed into two blocks with different (successful) outcomes" $
        specify "correct output" $
            p committedTwoDifferentSuccessfulOutcomes
                `shouldBe` [ "Transaction is committed into 2 blocks:",
                             "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 0.000010 CCD (10 NRG):",
                             "  * transferred 0.000010 CCD from account '2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6' to account '4MkK65HrYvMauNTHTuL23wRDKp4VXkCiTpmoWYFtsrZHV3WwSa'",
                             "- 941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795 with status \"success\" and cost 0.000020 CCD (20 NRG):",
                             "  * transferred 0.000010 CCD from account '2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6' to account '4MkK65HrYvMauNTHTuL23wRDKp4VXkCiTpmoWYFtsrZHV3WwSa'"
                           ]
    describe "committed into two blocks" $
        specify "correct output" $
            p committedSuccessfulAndFailureOutcomes
                `shouldBe` [ "Transaction is committed into 2 blocks:",
                             "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 0.000010 CCD (10 NRG):",
                             "  * transferred 0.000010 CCD from account '2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6' to account '4MkK65HrYvMauNTHTuL23wRDKp4VXkCiTpmoWYFtsrZHV3WwSa'",
                             "- be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"rejected\" and cost 0.000020 CCD (20 NRG):",
                             "  * account or contract '2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6' does not have enough funds to transfer 0.000011 CCD"
                           ]
    describe "committed into two blocks" $
        specify "correct output" $
            p committedSuccessfulAndFailureOutcomes
                `shouldBe` [ "Transaction is committed into 2 blocks:",
                             "- 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 0.000010 CCD (10 NRG):",
                             "  * transferred 0.000010 CCD from account '2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6' to account '4MkK65HrYvMauNTHTuL23wRDKp4VXkCiTpmoWYFtsrZHV3WwSa'",
                             "- be880f81dfbcc0a049c3defe483327d0a2a3002a186a06d34bcd93a9be7f9994 with status \"rejected\" and cost 0.000020 CCD (20 NRG):",
                             "  * account or contract '2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6' does not have enough funds to transfer 0.000011 CCD"
                           ]
    describe "finalized with single outcome" $
        specify "correct output" $
            p finalized
                `shouldBe` ["Transaction is finalized into block 0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389 with status \"success\" and cost 0.000010 CCD (10 NRG)."]
    -- Unexpected cases.
    describe "committed with no outcomes" $
        specify "correct output" $
            p TransactionStatusResult{tsrState = Committed, tsrResults = Map.empty}
                `shouldBe` ["Transaction is committed, but no block information was received - this should never happen!"]
    describe "finalized with no outcomes" $
        specify "correct output" $
            p TransactionStatusResult{tsrState = Finalized, tsrResults = Map.empty}
                `shouldBe` ["Transaction is finalized, but no block information was received - this should never happen!"]
    describe "finalized with multiple outcomes" $
        specify "correct output" $
            p TransactionStatusResult{tsrState = Finalized, tsrResults = Map.fromList [(exampleBlockHash1, outcomeSuccess1a), (exampleBlockHash2, outcomeSuccess1b)]}
                `shouldBe` ["Transaction is finalized into multiple blocks - this should never happen and may indicate a serious problem with the chain!"]
  where
    p = \x -> execWriter $ printTransactionStatus x False Nothing
