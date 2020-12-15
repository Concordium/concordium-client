{-# OPTIONS_GHC -Wno-deprecations #-}
module SimpleClientTests.AccountSpec where

import System.Random

import Concordium.Client.Cli
import Concordium.Client.Output
import Concordium.Client.Types.Account
import Concordium.Common.Version
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.Crypto.SignatureScheme as SS
import Concordium.Crypto.EncryptedTransfers
import Concordium.ID.DummyData(dummyCommitment)
import qualified Data.Map.Strict as OrdMap

import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as AE
import Test.Hspec
import Data.Maybe
import qualified Data.Sequence as Seq
import System.IO.Unsafe (unsafePerformIO)

import Data.Time.Clock
import Data.Word (Word64)
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Crypto.BlsSignature (derivePublicKey, generateSecretKey)
import Concordium.Crypto.Ed25519Signature (newKeyPair)

exampleAddress :: NamedAddress
exampleAddress = NamedAddress (Just "example") a
  where Right a = IDTypes.addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"

accountSpec :: Spec
accountSpec = describe "account" $ do
  printAccountListSpec
  printAccountInfoSpec
  printCredSpec

dummyEncryptionSecretKey :: ElgamalSecretKey
dummyEncryptionSecretKey = fromJust $ AE.decode "\"a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd521271365fdd517916086b19e5cdc337659274b1149ab59fa780aa76f3bad2f74\""
dummyEncryptionPublicKey :: IDTypes.AccountEncryptionKey
dummyEncryptionPublicKey = fromJust $ AE.decode "\"a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4\""
encAmount1 :: EncryptedAmount
encAmount1 = fromJust $ AE.decode "\"b15c03d419e05b657257c6016b92788d3cc1cb48ad2ef442aae1ba0db0c42e566b214a4ecb2031f6e5a3d5261d639186b832e7e3d91a86a428f01748143618a9ba8c214654b9bcd8969a49b69f69042124d2170e0986a3cec80e5db45d1e920d84c640baf5646f7d6dc537015b1aac25d041dcfecd48b251b5867b2a74f0f92e6d8df5a2e4ae2f54b970ae472098c21881d07da93a6067dd1a9e0537b2ac15a594e2a1fc31a757ce430c1ad79fd69b4eea81d20f9c953e9439d2970ec4674b76\""
encAmount2 :: EncryptedAmount
encAmount2 = fromJust $ AE.decode "\"9450b8ace9ad5a22e8eea743244bf929e69de3d2c8445d34278d23c6c72dfbf2c1a6fc7fabd4eb3bd7752a0765255ea0963748ddc6bc87040627533b1a3ce76318734cf3cc9dd9b05fd8dfe5c31f51addc68f41b43f764a36f03097c1d1dda12926b233d1f2efdd8f1c143c7a63c5575e1a9f5fac7e265d33ba769f6396db6c91da16e9ddf85b1ec7fc0cbcb4afbd9e491a755540bdf8a42cb46e32f9de7c8986e77a6d111e9fac32524183415cce14ddff3ca5795b5abdc1ad0a1397853a4a3\""

dummyTransactionHash1 :: Types.TransactionHash
dummyTransactionHash1 = fromJust . AE.decode $ "\"f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868\""
dummyTransactionHash2 :: Types.TransactionHash
dummyTransactionHash2 = fromJust . AE.decode $ "\"b041315fe35a8bdf836647037c24c8e87402547c82aea568c66ee18aa3091326\""

exampleBakerInfoResult :: AccountInfoBakerPendingChange -> AccountInfoBakerResult
exampleBakerInfoResult pc = AccountInfoBakerResult
  { abirStakedAmount = 100
  , abirStakeEarnings = True
  , abirAccountBakerInfo = AccountInfoBakerInfo {
      aibiIdentity = 1,
      aibiElectionVerifyKey = VRF.publicKey . fst $ VRF.randomKeyPair (mkStdGen 0),
      aibiAggregationVerifyKey = derivePublicKey $ unsafePerformIO generateSecretKey,
      aibiSignatureVerifyKey = snd $ unsafePerformIO newKeyPair
      }
  , abirBakerPendingChange = pc
  }

exampleAccountInfoResult :: Maybe AccountInfoBakerPendingChange -> [IDTypes.AccountCredential] -> AccountInfoResult
exampleAccountInfoResult d cs = AccountInfoResult
                                { airAmount = Types.Amount 1
                                , airNonce = Types.Nonce 2
                                , airBaker = exampleBakerInfoResult <$> d
                                , airCredentials = map (Versioned 0) cs
                                , airInstances = []
                                , airReleaseSchedule = AccountInfoReleaseSchedule 0 []
                                , airEncryptedAmount = Types.AccountEncryptedAmount {
                                    _startIndex = 3,
                                    _incomingEncryptedAmounts = Seq.fromList [encAmount1, encAmount2],
                                    _selfAmount = encAmount2,
                                    _aggregatedAmount = Nothing
                                    }
                                , airEncryptionKey = dummyEncryptionPublicKey }

exampleCredentials :: IDTypes.Policy -> IDTypes.AccountCredential
exampleCredentials p = IDTypes.NormalAC (IDTypes.CredentialDeploymentValues
                       { IDTypes.cdvAccount = acc
                       , IDTypes.cdvRegId = regId
                       , IDTypes.cdvIpId = IDTypes.IP_ID 21
                       , IDTypes.cdvThreshold = IDTypes.Threshold 1
                       , IDTypes.cdvArData = Map.singleton (IDTypes.ArIdentity 0) IDTypes.ChainArData
                                              { IDTypes.ardIdCredPubShare = share }
                       , IDTypes.cdvPolicy = p }) $ IDTypes.CredentialDeploymentCommitments
                        {
                          cmmPrf = dummyCommitment,
                          cmmCredCounter = dummyCommitment,
                          cmmMaxAccounts = dummyCommitment,
                          cmmAttributes = OrdMap.empty,
                          cmmIdCredSecSharingCoeff = []
                        }
  where acc = let
          keys = unsafePerformIO $ replicateM 2 (SS.correspondingVerifyKey <$> SS.newKeyPair SS.Ed25519)
          threshold = 1
          in IDTypes.NewAccount keys (IDTypes.SignatureThreshold threshold)
        (Just regId) = BSH.deserializeBase16 "a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f"
        (Just share) = BSH.deserializeBase16 "a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46fa1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f"

examplePolicyWithoutItems :: IDTypes.Policy
examplePolicyWithoutItems = IDTypes.Policy { IDTypes.pCreatedAt = IDTypes.YearMonth 2020 4
                                           , IDTypes.pValidTo = IDTypes.YearMonth 2021 4
                                           , IDTypes.pItems = Map.empty }

examplePolicyWithOneItem :: IDTypes.Policy
examplePolicyWithOneItem = IDTypes.Policy
                           { IDTypes.pCreatedAt = IDTypes.YearMonth 2020 4
                           , IDTypes.pValidTo = IDTypes.YearMonth 2021 4
                           , IDTypes.pItems = Map.fromList [(IDTypes.AttributeTag 1,
                                                             IDTypes.AttributeValue "Value-1")] }

examplePolicyWithTwoItems :: IDTypes.Policy
examplePolicyWithTwoItems = IDTypes.Policy
                            { IDTypes.pCreatedAt = IDTypes.YearMonth 2020 4
                            , IDTypes.pValidTo = IDTypes.YearMonth 2021 4
                            , IDTypes.pItems = Map.fromList [(IDTypes.AttributeTag 1, IDTypes.AttributeValue "Value-1"),
                                                             (IDTypes.AttributeTag 3, IDTypes.AttributeValue "Value-2")] }

examplePolicyWithItemOutOfRange :: IDTypes.Policy
examplePolicyWithItemOutOfRange = IDTypes.Policy
                                  { IDTypes.pCreatedAt = IDTypes.YearMonth 2020 4
                                  , IDTypes.pValidTo = IDTypes.YearMonth 2021 4
                                  , IDTypes.pItems = Map.fromList [(IDTypes.AttributeTag 255, IDTypes.AttributeValue "Value-1")] }

printAccountListSpec :: Spec
printAccountListSpec = describe "printAccountList" $ do
  specify "empty" $ p [] `shouldBe` ["Accounts: none"]
  specify "single" $ p [exampleAddress] `shouldBe`
    ["Accounts:"
      , "                 Account Address                     Account Name"
      , "-------------------------------------------------------------------"
      , "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6   example"]
  where p = execWriter . printAccountList

exampleTime :: Word64 -> UTCTime
exampleTime _ = unsafePerformIO getCurrentTime

printAccountInfoSpec :: Spec
printAccountInfoSpec = describe "printAccountInfo" $ do
  specify "without baker nor credentials" $ p exampleAddress (exampleAccountInfoResult Nothing []) `shouldBe`
    [ "Local name:            example"
    , "Address:               2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "Balance:               0.000001 GTU"
    , "Nonce:                 2"
    , "Encryption public key: a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4"
    , ""
    , "Baker: none"
    , ""
    , "Credentials: none" ]
  specify "with baker" $ p exampleAddress (exampleAccountInfoResult (Just NoChange) []) `shouldBe`
    [ "Local name:            example"
    , "Address:               2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "Balance:               0.000001 GTU"
    , "Nonce:                 2"
    , "Encryption public key: a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4"
    , ""
    , "Baker: #1"
    ," - Staked amount: 0.000100 GTU"
    ," - Restake earnings: yes"
    , ""
    , "Credentials: none" ]
  specify "with release schedule" $ p exampleAddress ((exampleAccountInfoResult Nothing []) { airReleaseSchedule = AccountInfoReleaseSchedule {
                                                                                                 totalRelease = 100,
                                                                                                 releaseSchedule = [ReleaseScheduleItem 1604417302000 33 [dummyTransactionHash1, dummyTransactionHash2],
                                                                                                                    ReleaseScheduleItem 1604417342000 33 [dummyTransactionHash1],
                                                                                                                    ReleaseScheduleItem 1604417382000 34 [dummyTransactionHash2]]
                                                                                                 } }) `shouldBe`
    [ "Local name:            example"
    , "Address:               2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "Balance:               0.000001 GTU"
    , "Release schedule:      total 0.000100 GTU"
    , "   Tue,  3 Nov 2020 15:28:22 UTC:               0.000033 GTU scheduled by the transactions: f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868, b041315fe35a8bdf836647037c24c8e87402547c82aea568c66ee18aa3091326."
    , "   Tue,  3 Nov 2020 15:29:02 UTC:               0.000033 GTU scheduled by the transactions: f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868."
    , "   Tue,  3 Nov 2020 15:29:42 UTC:               0.000034 GTU scheduled by the transactions: b041315fe35a8bdf836647037c24c8e87402547c82aea568c66ee18aa3091326."
    , "Nonce:                 2"
    , "Encryption public key: a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4"
    , ""
    , "Baker: none"
    , ""
    , "Credentials: none" ]
  specify "with one credential" $ p exampleAddress (exampleAccountInfoResult Nothing [exampleCredentials examplePolicyWithoutItems]) `shouldBe`
    [ "Local name:            example"
    , "Address:               2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "Balance:               0.000001 GTU"
    , "Nonce:                 2"
    , "Encryption public key: a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4"
    , ""
    , "Baker: none"
    , ""
    , "Credentials:"
    , "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:"
    , "  - Expiration: Apr 2021"
    , "  - Type: normal"
    , "  - Revealed attributes: none" ]
  specify "with two credentials" $ p exampleAddress (exampleAccountInfoResult Nothing [ exampleCredentials examplePolicyWithoutItems
                                                                                       , exampleCredentials examplePolicyWithTwoItems ]) `shouldBe`
    [ "Local name:            example"
    , "Address:               2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "Balance:               0.000001 GTU"
    , "Nonce:                 2"
    , "Encryption public key: a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4"
    , ""
    , "Baker: none"
    , ""
    , "Credentials:"
    , "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:"
    , "  - Expiration: Apr 2021"
    , "  - Type: normal"
    , "  - Revealed attributes: none"
    , "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:"
    , "  - Expiration: Apr 2021"
    , "  - Type: normal"
    , "  - Revealed attributes: lastName=\"Value-1\", dob=\"Value-2\"" ]
  xspecify "with one credential - verbose" $
    (execWriter $ printAccountInfo exampleTime exampleAddress (exampleAccountInfoResult Nothing [exampleCredentials examplePolicyWithoutItems]) True False Nothing) `shouldBe`
      [ "Local name:            example"
      , "Address:               2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
      , "Balance:               0.000001 GTU"
      , "Nonce:                 2"
      , "Encryption public key: a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4"
      , ""
      , "Baker: none"
      , ""
      , "Credentials:"
      , "{\n\
        \    \"value\": {\n\
        \        \"ipIdentity\": 21,\n\
        \        \"regId\": \"a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f\",\n\
        \        \"arData\": {\n\
        \            \"0\": {\n\
        \                \"encIdCredPubShare\": \"a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46fa1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f\"\n\
        \            }\n\
        \        },\n\
        \        \"account\": \"2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6\",\n\
        \        \"revocationThreshold\": 1,\n\
        \        \"policy\": {\n\
        \            \"revealedAttributes\": {},\n\
        \            \"createdAt\": \"202004\",\n\
        \            \"validTo\": \"202104\"\n\
        \        }\n\
        \    },\n\
        \    \"v\": 0\n\
        \}" ]
  specify "show encrypted balance" $ penc exampleAddress (exampleAccountInfoResult Nothing [exampleCredentials examplePolicyWithoutItems]) `shouldBe`
    [ "Local name:            example"
    , "Address:               2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "Balance:               0.000001 GTU"
    , "Nonce:                 2"
    , "Encryption public key: a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4"
    , ""
    , "Encrypted balance:"
    , "  Incoming amounts:"
    , "    3: b15c03d419e05b657257..."
    , "    4: 9450b8ace9ad5a22e8ee..."
    , "  Self balance: 9450b8ace9ad5a22e8ee..."
    , ""
    , "Baker: none"
    , ""
    , "Credentials:"
    , "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:"
    , "  - Expiration: Apr 2021"
    , "  - Type: normal"
    , "  - Revealed attributes: none" ]
  xspecify "show encrypted balance - verbose" $
    (execWriter $ printAccountInfo exampleTime exampleAddress (exampleAccountInfoResult Nothing [exampleCredentials examplePolicyWithoutItems]) True True Nothing) `shouldBe`
      [ "Local name:            example"
      , "Address:               2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
      , "Balance:               0.000001 GTU"
      , "Nonce:                 2"
      , "Encryption public key: a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4"
      , ""
      , "Encrypted balance:"
      , "  Incoming amounts:"
      , "    3: b15c03d419e05b657257c6016b92788d3cc1cb48ad2ef442aae1ba0db0c42e566b214a4ecb2031f6e5a3d5261d639186b832e7e3d91a86a428f01748143618a9ba8c214654b9bcd8969a49b69f69042124d2170e0986a3cec80e5db45d1e920d84c640baf5646f7d6dc537015b1aac25d041dcfecd48b251b5867b2a74f0f92e6d8df5a2e4ae2f54b970ae472098c21881d07da93a6067dd1a9e0537b2ac15a594e2a1fc31a757ce430c1ad79fd69b4eea81d20f9c953e9439d2970ec4674b76"
      , "    4: 9450b8ace9ad5a22e8eea743244bf929e69de3d2c8445d34278d23c6c72dfbf2c1a6fc7fabd4eb3bd7752a0765255ea0963748ddc6bc87040627533b1a3ce76318734cf3cc9dd9b05fd8dfe5c31f51addc68f41b43f764a36f03097c1d1dda12926b233d1f2efdd8f1c143c7a63c5575e1a9f5fac7e265d33ba769f6396db6c91da16e9ddf85b1ec7fc0cbcb4afbd9e491a755540bdf8a42cb46e32f9de7c8986e77a6d111e9fac32524183415cce14ddff3ca5795b5abdc1ad0a1397853a4a3"
      , "  Self balance: 9450b8ace9ad5a22e8eea743244bf929e69de3d2c8445d34278d23c6c72dfbf2c1a6fc7fabd4eb3bd7752a0765255ea0963748ddc6bc87040627533b1a3ce76318734cf3cc9dd9b05fd8dfe5c31f51addc68f41b43f764a36f03097c1d1dda12926b233d1f2efdd8f1c143c7a63c5575e1a9f5fac7e265d33ba769f6396db6c91da16e9ddf85b1ec7fc0cbcb4afbd9e491a755540bdf8a42cb46e32f9de7c8986e77a6d111e9fac32524183415cce14ddff3ca5795b5abdc1ad0a1397853a4a3"
      , ""
      , "Baker: none"
      , ""
      , "Credentials:"
      , "{\n\
        \    \"value\": {\n\
        \        \"ipIdentity\": 21,\n\
        \        \"regId\": \"a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f\",\n\
        \        \"arData\": {\n\
        \            \"0\": {\n\
        \                \"encIdCredPubShare\": \"a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46fa1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f\"\n\
        \            }\n\
        \        },\n\
        \        \"account\": \"2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6\",\n\
        \        \"revocationThreshold\": 1,\n\
        \        \"policy\": {\n\
        \            \"revealedAttributes\": {},\n\
        \            \"createdAt\": \"202004\",\n\
        \            \"validTo\": \"202104\"\n\
        \        }\n\
        \    },\n\
        \    \"v\": 0\n\
        \}" ]
  where p addr res = execWriter $ printAccountInfo exampleTime addr res False False Nothing
        penc addr res = execWriter $ printAccountInfo exampleTime addr res False True Nothing

printCredSpec :: Spec
printCredSpec = describe "printCred" $ do
  specify "without attributes" $ (p $ exampleCredentials examplePolicyWithoutItems) `shouldBe`
    [ "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:"
    , "  - Expiration: Apr 2021"
    , "  - Type: normal"
    , "  - Revealed attributes: none" ]
  specify "with single attribute" $ (p $ exampleCredentials examplePolicyWithOneItem) `shouldBe`
    [ "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:"
    , "  - Expiration: Apr 2021"
    , "  - Type: normal"
    , "  - Revealed attributes: lastName=\"Value-1\"" ]
  specify "with two attributes" $ (p $ exampleCredentials examplePolicyWithTwoItems) `shouldBe`
    [ "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:"
    , "  - Expiration: Apr 2021"
    , "  - Type: normal"
    , "  - Revealed attributes: lastName=\"Value-1\", dob=\"Value-2\"" ]
  specify "with attribute having key out of range" $ (p $ exampleCredentials examplePolicyWithItemOutOfRange) `shouldBe`
    [ "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:"
    , "  - Expiration: Apr 2021"
    , "  - Type: normal"
    , "  - Revealed attributes: <255>=\"Value-1\"" ]
  where p = execWriter . printCred
