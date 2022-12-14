module SimpleClientTests.ConsensusSpec where

import Concordium.Client.Cli
import Concordium.ID.Types
import Concordium.Client.Output
import Concordium.Types
import Concordium.Types.Queries

import qualified Data.Map.Strict as Map

import Control.Monad.Writer
import Data.Time.Clock
import Data.Time.Format

import Test.Hspec

consensusSpec :: Spec
consensusSpec = describe "consensus" $ do
  consensusStatusSpec
  consensusShowParametersSpec

consensusStatusSpec :: Spec
consensusStatusSpec = describe "status" $ do
  specify "optional fields present" $ p exampleStatusWithOptionalFields `shouldBe`
    [ "Best block:                  0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"
    , "Genesis block:               0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
    , "Genesis time:                1970-01-12 13:46:40 UTC"
    , "Slot duration:               1m 40s"
    , "Epoch duration:              1m 40s"
    , "Last finalized block:        941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"
    , "Best block height:           1"
    , "Last finalized block height: 2"
    , "Blocks received count:       3"
    , "Block last received time:    Mon, 12 Jan 1970 13:46:40 UTC"
    , "Block receive latency:          10 ms (EMA),    20 ms (EMSD)"
    , "Block receive period:          340 ms (EMA),  5600 ms (EMSD)"
    , "Blocks verified count:       5"
    , "Block last arrived time:     Thu, 15 Jan 1970 06:56:07 UTC"
    , "Block arrive latency:            0 ms (EMA),     1 ms (EMSD)"
    , "Block arrive period:           123 ms (EMA),   235 ms (EMSD)"
    , "Transactions per block:         0.110 (EMA),   12.135 (EMSD)"
    , "Finalization count:          14"
    , "Last finalized time:         Sat, 24 Jan 1970 03:33:20 UTC"
    , "Finalization period:         100000 ms (EMA), 200000 ms (EMSD)"
    , "Protocol version:            P1"
    , "Genesis index:               0"
    , "Current era genesis block:   0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
    , "Current era genesis time:    1970-01-12 13:46:40 UTC" ]

  specify "optional fields absent" $ p exampleStatusWithoutOptionalFields `shouldBe`
    [ "Best block:                  0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"
    , "Genesis block:               0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
    , "Genesis time:                1970-01-12 13:46:40 UTC"
    , "Slot duration:               1m 40s"
    , "Epoch duration:              1m 40s"
    , "Last finalized block:        941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"
    , "Best block height:           1"
    , "Last finalized block height: 2"
    , "Blocks received count:       3"
    , "Block last received time:    none"
    , "Block receive latency:          10 ms (EMA),    20 ms (EMSD)"
    , "Block receive period:        none"
    , "Blocks verified count:       5"
    , "Block last arrived time:     none"
    , "Block arrive latency:            0 ms (EMA),     1 ms (EMSD)"
    , "Block arrive period:         none"
    , "Transactions per block:         0.110 (EMA),   12.130 (EMSD)"
    , "Finalization count:          14"
    , "Last finalized time:         none"
    , "Finalization period:         none" 
    , "Protocol version:            P1"
    , "Genesis index:               0"
    , "Current era genesis block:   0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
    , "Current era genesis time:    1970-01-12 13:46:40 UTC" ]
  where p = execWriter . printConsensusStatus

consensusShowParametersSpec :: Spec
consensusShowParametersSpec = describe "show parameters" $ do
  specify "not including bakers" $ p False exampleBirkParameters addrmap `shouldBe`
    [ "Election nonce:      50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"
    ] -- , "Election difficulty: 0.12" ]
  specify "including bakers" $ p True exampleBirkParameters addrmap `shouldBe`
    [ "Election nonce:      50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"
    -- , "Election difficulty: 0.12"
    , "Bakers:"
    , "                             Account                       Lottery power  Account Name"
    , "        ------------------------------------------------------------------------------"
    , "     1: 2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6   10.0000 %  account1"
    , "    12: 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y    2.0000 %   "
    , "    13: 4p2n8QQn5akq3XqAAJt2a5CsnGhDvUon6HExd2szrfkZCTD4FX   <0.0001 %  account2" ]
  specify "including bakers (empty)" $ p True exampleBirkParametersNoBakers addrmap `shouldBe`
    [ "Election nonce:      50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"
    -- , "Election difficulty: 0.12"
    , "Bakers:              none" ]
  specify "including bakers (single one with 100% stake)" $ p True exampleBirkParametersSingleBakerWithAllStake addrmap `shouldBe`
    [ "Election nonce:      50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"
    -- , "Election difficulty: 0.12"
    , "Bakers:"
    , "                             Account                       Lottery power  Account Name"
    , "        ------------------------------------------------------------------------------"
    , "     1: 2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6  100.0000 %  account1" ]
  where 
    p includeBakers bparams = execWriter . printBirkParameters includeBakers bparams
    addrmap = Map.fromList [(acc1, "account1"), (acc2, "account2")]
    acc1 = case addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6" of
             Right addr -> addr
             Left err -> error err
    acc2 = case addressFromText "4p2n8QQn5akq3XqAAJt2a5CsnGhDvUon6HExd2szrfkZCTD4FX" of
             Right addr -> addr
             Left err -> error err

exampleStatusWithOptionalFields :: ConsensusStatus
exampleStatusWithOptionalFields =
  ConsensusStatus
  { csBestBlock = exampleBlockHash1
  , csGenesisBlock = exampleBlockHash2
  , csLastFinalizedBlock = exampleBlockHash3
  , csBestBlockHeight = 1
  , csLastFinalizedBlockHeight = 2
  , csBlocksReceivedCount = 3
  , csBlockLastReceivedTime = Just exampleTime1
  , csBlockReceiveLatencyEMA = 0.01
  , csBlockReceiveLatencyEMSD = 0.02
  , csBlockReceivePeriodEMA = Just 0.34
  , csBlockReceivePeriodEMSD = Just 5.6
  , csBlocksVerifiedCount = 5
  , csBlockLastArrivedTime = Just exampleTime2
  , csBlockArriveLatencyEMA = 0.0004
  , csBlockArriveLatencyEMSD = 0.00051
  , csBlockArrivePeriodEMA = Just 0.1234
  , csBlockArrivePeriodEMSD = Just 0.23451
  , csTransactionsPerBlockEMA = 0.11
  , csTransactionsPerBlockEMSD = 12.13456
  , csFinalizationCount = 14
  , csLastFinalizedTime = Just exampleTime3
  , csFinalizationPeriodEMA = Just 100
  , csFinalizationPeriodEMSD = Just 200
  , csGenesisTime = exampleTime1
  , csSlotDuration = 100000
  , csEpochDuration = 100000
  , csProtocolVersion = P1
  , csGenesisIndex = 0
  , csCurrentEraGenesisBlock = exampleBlockHash2
  , csCurrentEraGenesisTime = exampleTime1 }

exampleStatusWithoutOptionalFields :: ConsensusStatus
exampleStatusWithoutOptionalFields =
  ConsensusStatus
  { csBestBlock = exampleBlockHash1
  , csGenesisBlock = exampleBlockHash2
  , csLastFinalizedBlock = exampleBlockHash3
  , csBestBlockHeight = 1
  , csLastFinalizedBlockHeight = 2
  , csBlocksReceivedCount = 3
  , csBlockLastReceivedTime = Nothing
  , csBlockReceiveLatencyEMA = 0.01
  , csBlockReceiveLatencyEMSD = 0.02
  , csBlockReceivePeriodEMA = Nothing
  , csBlockReceivePeriodEMSD = Nothing
  , csBlocksVerifiedCount = 5
  , csBlockLastArrivedTime = Nothing
  , csBlockArriveLatencyEMA = 0.0004
  , csBlockArriveLatencyEMSD = 0.00051
  , csBlockArrivePeriodEMA = Nothing
  , csBlockArrivePeriodEMSD = Nothing
  , csTransactionsPerBlockEMA = 0.11
  , csTransactionsPerBlockEMSD = 12.13
  , csFinalizationCount = 14
  , csLastFinalizedTime = Nothing
  , csFinalizationPeriodEMA = Nothing
  , csFinalizationPeriodEMSD = Nothing
  , csGenesisTime = exampleTime1
  , csSlotDuration = 100000
  , csEpochDuration = 100000
  , csProtocolVersion = P1
  , csGenesisIndex = 0
  , csCurrentEraGenesisBlock = exampleBlockHash2
  , csCurrentEraGenesisTime = exampleTime1 }

exampleBirkParameters :: BirkParametersResult
exampleBirkParameters =
  BirkParametersResult
  { bprElectionNonce = exampleNonce
  -- , bprElectionDifficulty = 0.12
  , bprBakers = [ BirkParametersBakerResult
                  { bpbrId = 1
                  , bpbrLotteryPower = 0.1
                  , bpbrAccount = exampleAccountAddress1 }
                , BirkParametersBakerResult
                  { bpbrId = 12
                  , bpbrLotteryPower = 0.02
                  , bpbrAccount = exampleAccountAddress2 }
                , BirkParametersBakerResult
                  { bpbrId = 13
                  , bpbrLotteryPower = 1e-9
                  , bpbrAccount = exampleAccountAddress3 } ] }

exampleBirkParametersSingleBakerWithAllStake :: BirkParametersResult
exampleBirkParametersSingleBakerWithAllStake =
  BirkParametersResult
  { bprElectionNonce = exampleNonce
  -- , bprElectionDifficulty = 0.12
  , bprBakers = [ BirkParametersBakerResult
                  { bpbrId = 1
                  , bpbrLotteryPower = 1
                  , bpbrAccount = exampleAccountAddress1 } ] }

exampleBirkParametersNoBakers :: BirkParametersResult
exampleBirkParametersNoBakers =
  BirkParametersResult
  { bprElectionNonce = exampleNonce
  -- , bprElectionDifficulty = 0.12
  , bprBakers = [] }

exampleNonce :: LeadershipElectionNonce
exampleNonce = read "50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"

exampleAccountAddress1 :: AccountAddress
exampleAccountAddress1 = case addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6" of
                           Right addr -> addr
                           Left err -> error err

exampleAccountAddress2 :: AccountAddress
exampleAccountAddress2 = case addressFromText "4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y" of
                           Right addr -> addr
                           Left err -> error err

exampleAccountAddress3 :: AccountAddress
exampleAccountAddress3 = case addressFromText "4p2n8QQn5akq3XqAAJt2a5CsnGhDvUon6HExd2szrfkZCTD4FX" of
                           Right addr -> addr
                           Left err -> error err

exampleBlockHash1 :: BlockHash
exampleBlockHash1 = read "0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"

exampleBlockHash2 :: BlockHash
exampleBlockHash2 = read "0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"

exampleBlockHash3 :: BlockHash
exampleBlockHash3 = read "941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"

exampleTime1 :: UTCTime
exampleTime1 = parseTimeOrError False defaultTimeLocale "%s" "1000000"

exampleTime2 :: UTCTime
exampleTime2 = parseTimeOrError False defaultTimeLocale "%s" "1234567"

exampleTime3 :: UTCTime
exampleTime3 = parseTimeOrError False defaultTimeLocale "%s" "2000000"
