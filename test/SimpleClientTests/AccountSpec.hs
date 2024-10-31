{-# OPTIONS_GHC -Wno-deprecations #-}

module SimpleClientTests.AccountSpec where

import Concordium.Client.Config (AccountNameMap)
import Concordium.Client.Output
import Concordium.Client.Types.Account
import Concordium.Common.Time
import Concordium.Common.Version
import Concordium.Crypto.BlsSignature (derivePublicKey, generateSecretKey)
import qualified Concordium.Crypto.ByteStringHelpers as BSH
import Concordium.Crypto.Ed25519Signature (newKeyPair)
import Concordium.Crypto.EncryptedTransfers
import qualified Concordium.Crypto.SignatureScheme as SS
import qualified Concordium.Crypto.VRF as VRF
import Concordium.ID.DummyData (dummyCommitment)
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import Concordium.Types.Accounts
import Concordium.Types.Accounts.Releases
import Concordium.Types.Execution (DelegationTarget (..), OpenStatus (..))

import Control.Monad
import Control.Monad.Writer
import qualified Data.Aeson as AE
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Time.Clock
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Test.Hspec

exampleNamedAddress :: NamedAddress
exampleNamedAddress = NamedAddress ["example"] exampleAddress1

exampleAddress1 :: IDTypes.AccountAddress
exampleAddress1 = case IDTypes.addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6" of
    Right addr -> addr
    -- This does not happen since the format
    -- of the text is that of a valid address.
    Left str -> error str

exampleAddress2 :: IDTypes.AccountAddress
exampleAddress2 = case IDTypes.addressFromText "4P6vppapjvwAxGf5o1dXUhgwpW3Tvpc6vHj75MJHD6Z3RUmMpJ" of
    Right addr -> addr
    -- This does not happen since the format of
    -- the text is that of a valid address.
    Left str -> error str

exampleAccountNameMap :: AccountNameMap
exampleAccountNameMap = Map.fromList [("example", exampleAddress1), ("exampleExtraName", exampleAddress1), ("example2", exampleAddress2)]

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

exampleBakerPoolInfo :: BakerPoolInfo
exampleBakerPoolInfo =
    BakerPoolInfo
        OpenForAll
        (Types.UrlText "https://example.com")
        (Types.CommissionRates rate rate rate)
  where
    rate = Types.makeAmountFraction 1000

exampleBakerInfoResult :: StakePendingChange' UTCTime -> AccountStakingInfo
exampleBakerInfoResult pc =
    AccountStakingBaker
        { asiStakedAmount = 100,
          asiStakeEarnings = True,
          asiBakerInfo =
            BakerInfo
                { _bakerIdentity = 1,
                  _bakerElectionVerifyKey = VRF.publicKey . fst $ VRF.randomKeyPair (mkStdGen 0),
                  _bakerAggregationVerifyKey = derivePublicKey $ unsafePerformIO generateSecretKey,
                  _bakerSignatureVerifyKey = snd $ unsafePerformIO newKeyPair
                },
          asiPendingChange = pc,
          asiPoolInfo = Just exampleBakerPoolInfo,
          asiIsSuspended = False
        }

exampleDelegatorStakingInfo :: StakePendingChange' UTCTime -> AccountStakingInfo
exampleDelegatorStakingInfo pc =
    AccountStakingDelegated
        { asiStakedAmount = 100,
          asiStakeEarnings = False,
          asiDelegationTarget = DelegatePassive,
          asiDelegationPendingChange = pc
        }

-- The credentials will be given indices 0, 1, ..
exampleAccountInfoResult :: AccountStakingInfo -> [IDTypes.RawAccountCredential] -> AccountInfo
exampleAccountInfoResult = exampleAccountInfoResultWithCooldowns []

exampleCooldowns :: [Cooldown]
exampleCooldowns = [Cooldown 3600000 200 StatusCooldown, Cooldown 7200000 400 StatusPreCooldown, Cooldown 9000000 500 StatusPrePreCooldown]

exampleAccountInfoResultWithCooldowns :: [Cooldown] -> AccountStakingInfo -> [IDTypes.RawAccountCredential] -> AccountInfo
exampleAccountInfoResultWithCooldowns cooldowns staking cs =
    AccountInfo
        { aiAccountAmount = Types.Amount 1,
          aiAccountNonce = Types.Nonce 2,
          aiStakingInfo = staking,
          aiAccountCredentials = Map.fromList . zip [0 ..] . map (Versioned 0) $ cs,
          aiAccountThreshold = 1,
          aiAccountReleaseSchedule = AccountReleaseSummary 0 [],
          aiAccountEncryptedAmount =
            Types.AccountEncryptedAmount
                { _startIndex = 3,
                  _incomingEncryptedAmounts = Seq.fromList [encAmount1, encAmount2],
                  _selfAmount = encAmount2,
                  _aggregatedAmount = Nothing
                },
          aiAccountEncryptionKey = dummyEncryptionPublicKey,
          aiAccountIndex = 27,
          aiAccountAddress = exampleAddress1,
          aiAccountCooldowns = cooldowns,
          aiAccountAvailableAmount = Types.Amount 1
        }

exampleCredentials :: IDTypes.Policy -> IDTypes.RawAccountCredential
exampleCredentials p =
    IDTypes.NormalAC
        ( IDTypes.CredentialDeploymentValues
            { IDTypes.cdvPublicKeys = acc,
              IDTypes.cdvCredId = regId,
              IDTypes.cdvIpId = IDTypes.IP_ID 21,
              IDTypes.cdvThreshold = IDTypes.Threshold 1,
              IDTypes.cdvArData =
                Map.singleton
                    (IDTypes.ArIdentity 0)
                    IDTypes.ChainArData
                        { IDTypes.ardIdCredPubShare = share
                        },
              IDTypes.cdvPolicy = p
            }
        )
        $ IDTypes.CredentialDeploymentCommitments
            { cmmPrf = dummyCommitment,
              cmmCredCounter = dummyCommitment,
              cmmMaxAccounts = dummyCommitment,
              cmmAttributes = Map.empty,
              cmmIdCredSecSharingCoeff = []
            }
  where
    acc =
        let
            keys = Map.fromList . zip [0 ..] $ unsafePerformIO $ replicateM 2 (SS.correspondingVerifyKey <$> SS.newKeyPair SS.Ed25519)
            threshold = 1
        in
            IDTypes.CredentialPublicKeys keys (IDTypes.SignatureThreshold threshold)
    regIdS = "a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f"
    shareS = "a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46fa1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f"
    (regId, share) = case (BSH.deserializeBase16 regIdS, BSH.deserializeBase16 shareS) of
        (Just regId', Just shareS') -> (regId', shareS')
        -- This does not happen since regIds and shareS are
        -- validly generated base 16 strings and hence they
        -- are deserialized.
        _ -> error "unable to deserialize"

examplePolicyWithoutItems :: IDTypes.Policy
examplePolicyWithoutItems =
    IDTypes.Policy
        { IDTypes.pCreatedAt = YearMonth 2020 4,
          IDTypes.pValidTo = YearMonth 2021 4,
          IDTypes.pItems = Map.empty
        }

examplePolicyWithOneItem :: IDTypes.Policy
examplePolicyWithOneItem =
    IDTypes.Policy
        { IDTypes.pCreatedAt = YearMonth 2020 4,
          IDTypes.pValidTo = YearMonth 2021 4,
          IDTypes.pItems =
            Map.fromList
                [   ( IDTypes.AttributeTag 1,
                      IDTypes.AttributeValue "Value-1"
                    )
                ]
        }

examplePolicyWithTwoItems :: IDTypes.Policy
examplePolicyWithTwoItems =
    IDTypes.Policy
        { IDTypes.pCreatedAt = YearMonth 2020 4,
          IDTypes.pValidTo = YearMonth 2021 4,
          IDTypes.pItems =
            Map.fromList
                [ (IDTypes.AttributeTag 1, IDTypes.AttributeValue "Value-1"),
                  (IDTypes.AttributeTag 3, IDTypes.AttributeValue "Value-2")
                ]
        }

examplePolicyWithItemOutOfRange :: IDTypes.Policy
examplePolicyWithItemOutOfRange =
    IDTypes.Policy
        { IDTypes.pCreatedAt = YearMonth 2020 4,
          IDTypes.pValidTo = YearMonth 2021 4,
          IDTypes.pItems = Map.fromList [(IDTypes.AttributeTag 255, IDTypes.AttributeValue "Value-1")]
        }

printAccountListSpec :: Spec
printAccountListSpec = describe "printAccountList" $ do
    specify "empty" $ p [] `shouldBe` ["Accounts: none"]
    specify "multiple" $
        p [exampleAddress1, exampleAddress2]
            `shouldBe` [ "Accounts:",
                         "                 Account Address                     Account Names",
                         "--------------------------------------------------------------------",
                         "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6   'example' 'exampleExtraName'",
                         "4P6vppapjvwAxGf5o1dXUhgwpW3Tvpc6vHj75MJHD6Z3RUmMpJ   'example2'"
                       ]
  where
    p = execWriter . (printAccountList exampleAccountNameMap)

printAccountInfoSpec :: Spec
printAccountInfoSpec = describe "printAccountInfo" $ do
    specify "without baker nor credentials" $
        p exampleNamedAddress (exampleAccountInfoResult AccountStakingNone [])
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Validator or delegator: no",
                         "",
                         "Credentials: none"
                       ]
    specify "with baker" $
        p exampleNamedAddress (exampleAccountInfoResult (exampleBakerInfoResult NoChange) [])
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Validator: #1",
                         " - Staked amount: 0.000100 CCD",
                         " - Restake earnings: yes",
                         "",
                         "Credentials: none"
                       ]
    specify "with delegator" $
        p exampleNamedAddress (exampleAccountInfoResult (exampleDelegatorStakingInfo NoChange) [])
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Delegating stake: yes",
                         "Delegation target: Passive delegation",
                         " - Staked amount: 0.000100 CCD",
                         " - Restake earnings: no",
                         "",
                         "Credentials: none"
                       ]
    specify "with delegator & cooldown" $
        p exampleNamedAddress (exampleAccountInfoResultWithCooldowns exampleCooldowns (exampleDelegatorStakingInfo NoChange) [])
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Delegating stake: yes",
                         "Delegation target: Passive delegation",
                         " - Staked amount: 0.000100 CCD",
                         " - Restake earnings: no",
                         "Inactive stake in cooldown:",
                         "   0.000200 CCD available after Thu,  1 Jan 1970 01:00:00 UTC",
                         "   0.000400 CCD available after Thu,  1 Jan 1970 02:00:00 UTC",
                         "   0.000500 CCD available after Thu,  1 Jan 1970 02:30:00 UTC",
                         "",
                         "Credentials: none"
                       ]
    specify "with release schedule" $
        p
            exampleNamedAddress
            ( (exampleAccountInfoResult AccountStakingNone [])
                { aiAccountReleaseSchedule =
                    AccountReleaseSummary
                        { releaseTotal = 100,
                          releaseSchedule =
                            [ ScheduledRelease 1604417302000 33 [dummyTransactionHash1, dummyTransactionHash2],
                              ScheduledRelease 1604417342000 33 [dummyTransactionHash1],
                              ScheduledRelease 1604417382000 34 [dummyTransactionHash2]
                            ]
                        }
                }
            )
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Release schedule:       total 0.000100 CCD",
                         "   Tue,  3 Nov 2020 15:28:22 UTC:               0.000033 CCD scheduled by the transactions: f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868, b041315fe35a8bdf836647037c24c8e87402547c82aea568c66ee18aa3091326.",
                         "   Tue,  3 Nov 2020 15:29:02 UTC:               0.000033 CCD scheduled by the transactions: f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868.",
                         "   Tue,  3 Nov 2020 15:29:42 UTC:               0.000034 CCD scheduled by the transactions: b041315fe35a8bdf836647037c24c8e87402547c82aea568c66ee18aa3091326.",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Validator or delegator: no",
                         "",
                         "Credentials: none"
                       ]
    specify "with one credential" $
        p exampleNamedAddress (exampleAccountInfoResult AccountStakingNone [exampleCredentials examplePolicyWithoutItems])
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Validator or delegator: no",
                         "",
                         "Credentials:",
                         "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:",
                         "  - Index: 0",
                         "  - Expiration: Apr 2021",
                         "  - Type: normal",
                         "  - Revealed attributes: none"
                       ]
    specify "with two credentials" $
        p
            exampleNamedAddress
            ( exampleAccountInfoResult
                AccountStakingNone
                [ exampleCredentials examplePolicyWithoutItems,
                  exampleCredentials examplePolicyWithTwoItems
                ]
            )
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Validator or delegator: no",
                         "",
                         "Credentials:",
                         "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:",
                         "  - Index: 0",
                         "  - Expiration: Apr 2021",
                         "  - Type: normal",
                         "  - Revealed attributes: none",
                         "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:",
                         "  - Index: 1",
                         "  - Expiration: Apr 2021",
                         "  - Type: normal",
                         "  - Revealed attributes: lastName=\"Value-1\", dob=\"Value-2\""
                       ]
    xspecify "with one credential - verbose" $
        (execWriter $ printAccountInfo exampleNamedAddress (exampleAccountInfoResult AccountStakingNone [exampleCredentials examplePolicyWithoutItems]) True False Nothing)
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Validator or delegator: no",
                         "",
                         "Credentials:",
                         "{\n\
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
                         \}"
                       ]
    specify "show encrypted balance" $
        penc exampleNamedAddress (exampleAccountInfoResult AccountStakingNone [exampleCredentials examplePolicyWithoutItems])
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Shielded balance:",
                         "  Incoming amounts:",
                         "    3: b15c03d419e05b657257...",
                         "    4: 9450b8ace9ad5a22e8ee...",
                         "  Self balance: 9450b8ace9ad5a22e8ee...",
                         "",
                         "Validator or delegator: no",
                         "",
                         "Credentials:",
                         "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:",
                         "  - Index: 0",
                         "  - Expiration: Apr 2021",
                         "  - Type: normal",
                         "  - Revealed attributes: none"
                       ]
    xspecify "show encrypted balance - verbose" $
        (execWriter $ printAccountInfo exampleNamedAddress (exampleAccountInfoResult AccountStakingNone [exampleCredentials examplePolicyWithoutItems]) True True Nothing)
            `shouldBe` [ "Local names:            'example'",
                         "Address:                2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6",
                         "Balance:                0.000001 CCD",
                         " - At disposal:         0.000001 CCD",
                         "Nonce:                  2",
                         "Encryption public key:  a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4",
                         "",
                         "Shielded balance:",
                         "  Incoming amounts:",
                         "    3: b15c03d419e05b657257c6016b92788d3cc1cb48ad2ef442aae1ba0db0c42e566b214a4ecb2031f6e5a3d5261d639186b832e7e3d91a86a428f01748143618a9ba8c214654b9bcd8969a49b69f69042124d2170e0986a3cec80e5db45d1e920d84c640baf5646f7d6dc537015b1aac25d041dcfecd48b251b5867b2a74f0f92e6d8df5a2e4ae2f54b970ae472098c21881d07da93a6067dd1a9e0537b2ac15a594e2a1fc31a757ce430c1ad79fd69b4eea81d20f9c953e9439d2970ec4674b76",
                         "    4: 9450b8ace9ad5a22e8eea743244bf929e69de3d2c8445d34278d23c6c72dfbf2c1a6fc7fabd4eb3bd7752a0765255ea0963748ddc6bc87040627533b1a3ce76318734cf3cc9dd9b05fd8dfe5c31f51addc68f41b43f764a36f03097c1d1dda12926b233d1f2efdd8f1c143c7a63c5575e1a9f5fac7e265d33ba769f6396db6c91da16e9ddf85b1ec7fc0cbcb4afbd9e491a755540bdf8a42cb46e32f9de7c8986e77a6d111e9fac32524183415cce14ddff3ca5795b5abdc1ad0a1397853a4a3",
                         "  Self balance: 9450b8ace9ad5a22e8eea743244bf929e69de3d2c8445d34278d23c6c72dfbf2c1a6fc7fabd4eb3bd7752a0765255ea0963748ddc6bc87040627533b1a3ce76318734cf3cc9dd9b05fd8dfe5c31f51addc68f41b43f764a36f03097c1d1dda12926b233d1f2efdd8f1c143c7a63c5575e1a9f5fac7e265d33ba769f6396db6c91da16e9ddf85b1ec7fc0cbcb4afbd9e491a755540bdf8a42cb46e32f9de7c8986e77a6d111e9fac32524183415cce14ddff3ca5795b5abdc1ad0a1397853a4a3",
                         "",
                         "Validator or delegator: no",
                         "",
                         "Credentials:",
                         "{\n\
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
                         \}"
                       ]
  where
    p addr res = execWriter $ printAccountInfo addr res False False Nothing
    penc addr res = execWriter $ printAccountInfo addr res False True Nothing

printCredSpec :: Spec
printCredSpec = describe "printCred" $ do
    specify "without attributes" $
        (p $ exampleCredentials examplePolicyWithoutItems)
            `shouldBe` [ "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:",
                         "  - Index: 0",
                         "  - Expiration: Apr 2021",
                         "  - Type: normal",
                         "  - Revealed attributes: none"
                       ]
    specify "with single attribute" $
        (p $ exampleCredentials examplePolicyWithOneItem)
            `shouldBe` [ "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:",
                         "  - Index: 0",
                         "  - Expiration: Apr 2021",
                         "  - Type: normal",
                         "  - Revealed attributes: lastName=\"Value-1\""
                       ]
    specify "with two attributes" $
        (p $ exampleCredentials examplePolicyWithTwoItems)
            `shouldBe` [ "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:",
                         "  - Index: 0",
                         "  - Expiration: Apr 2021",
                         "  - Type: normal",
                         "  - Revealed attributes: lastName=\"Value-1\", dob=\"Value-2\""
                       ]
    specify "with attribute having key out of range" $
        (p $ exampleCredentials examplePolicyWithItemOutOfRange)
            `shouldBe` [ "* a1355cd1e5e2f4b712c4302f09f045f194c708e5d0cae3b980f53ae3244fc7357d688d97be251a86735179871f03a46f:",
                         "  - Index: 0",
                         "  - Expiration: Apr 2021",
                         "  - Type: normal",
                         "  - Revealed attributes: <255>=\"Value-1\""
                       ]
  where
    p = execWriter . printCred 0
