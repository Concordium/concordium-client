module SimpleClientTests.ConsensusSpec where

import Concordium.Client.Cli
import Concordium.ID.Types
import Concordium.Client.Output
import Concordium.Types

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
    , "Finalization period:         100000 ms (EMA), 200000 ms (EMSD)" ]

  specify "optional fields absent" $ p exampleStatusWithoutOptionalFields `shouldBe`
    [ "Best block:                  0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"
    , "Genesis block:               0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
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
    , "Finalization period:         none" ]
  where p = execWriter . printConsensusStatus

consensusShowParametersSpec :: Spec
consensusShowParametersSpec = describe "show parameters" $ do
  specify "not including bakers" $ p False exampleBirkParameters `shouldBe`
    [ "Election nonce:      50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"
    , "Election difficulty: 0.12" ]
  specify "including bakers" $ p True exampleBirkParameters `shouldBe`
    [ "Election nonce:      50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"
    , "Election difficulty: 0.12"
    , "Bakers:"
    , "                             Account                       Lottery power"
    , "        ----------------------------------------------------------------"
    , "    11: 2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6    0.1000"
    , "    12: 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y    0.0200" ]
  specify "including bakers (empty)" $ p True exampleBirkParametersNoBakers `shouldBe`
    [ "Election nonce:      50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"
    , "Election difficulty: 0.12"
    , "Bakers:              none" ]
  where p includeBakers = execWriter . printBirkParameters includeBakers

exampleStatusWithOptionalFields :: ConsensusStatusResult
exampleStatusWithOptionalFields =
  ConsensusStatusResult
  { csrBestBlock = exampleBlockHash1
  , csrGenesisBlock = exampleBlockHash2
  , csrLastFinalizedBlock = exampleBlockHash3
  , csrBestBlockHeight = 1
  , csrLastFinalizedBlockHeight = 2
  , csrBlocksReceivedCount = 3
  , csrBlockLastReceivedTime = Just exampleTime1
  , csrBlockReceiveLatencyEMA = 0.01
  , csrBlockReceiveLatencyEMSD = 0.02
  , csrBlockReceivePeriodEMA = Just 0.34
  , csrBlockReceivePeriodEMSD = Just 5.6
  , csrBlocksVerifiedCount = 5
  , csrBlockLastArrivedTime = Just exampleTime2
  , csrBlockArriveLatencyEMA = 0.0004
  , csrBlockArriveLatencyEMSD = 0.00051
  , csrBlockArrivePeriodEMA = Just 0.1234
  , csrBlockArrivePeriodEMSD = Just 0.23451
  , csrTransactionsPerBlockEMA = 0.11
  , csrTransactionsPerBlockEMSD = 12.13456
  , csrFinalizationCount = 14
  , csrLastFinalizedTime = Just exampleTime3
  , csrFinalizationPeriodEMA = Just 100
  , csrFinalizationPeriodEMSD = Just 200 }

exampleStatusWithoutOptionalFields :: ConsensusStatusResult
exampleStatusWithoutOptionalFields =
  ConsensusStatusResult
  { csrBestBlock = exampleBlockHash1
  , csrGenesisBlock = exampleBlockHash2
  , csrLastFinalizedBlock = exampleBlockHash3
  , csrBestBlockHeight = 1
  , csrLastFinalizedBlockHeight = 2
  , csrBlocksReceivedCount = 3
  , csrBlockLastReceivedTime = Nothing
  , csrBlockReceiveLatencyEMA = 0.01
  , csrBlockReceiveLatencyEMSD = 0.02
  , csrBlockReceivePeriodEMA = Nothing
  , csrBlockReceivePeriodEMSD = Nothing
  , csrBlocksVerifiedCount = 5
  , csrBlockLastArrivedTime = Nothing
  , csrBlockArriveLatencyEMA = 0.0004
  , csrBlockArriveLatencyEMSD = 0.00051
  , csrBlockArrivePeriodEMA = Nothing
  , csrBlockArrivePeriodEMSD = Nothing
  , csrTransactionsPerBlockEMA = 0.11
  , csrTransactionsPerBlockEMSD = 12.13
  , csrFinalizationCount = 14
  , csrLastFinalizedTime = Nothing
  , csrFinalizationPeriodEMA = Nothing
  , csrFinalizationPeriodEMSD = Nothing }

exampleBirkParameters :: BirkParametersResult
exampleBirkParameters =
  BirkParametersResult
  { bprElectionNonce = exampleNonce
  , bprElectionDifficulty = 0.12
  , bprBakers = [BirkParametersBakerResult
                 { bpbrId = 11
                 , bpbrLotteryPower = 0.1
                 , bpbrAccount = exampleAccountAddress1
                 },
                 BirkParametersBakerResult
                 { bpbrId = 12
                 , bpbrLotteryPower = 0.02
                 , bpbrAccount = exampleAccountAddress2
                 }] }

exampleBirkParametersNoBakers :: BirkParametersResult
exampleBirkParametersNoBakers =
  BirkParametersResult
  { bprElectionNonce = exampleNonce
  , bprElectionDifficulty = 0.12
  , bprBakers = [] }

exampleNonce :: LeadershipElectionNonce
exampleNonce = read "50ab4065c5a8194fbd7f3acf06267c7d8023fce9b3b658a74f3a927599eb9322"

exampleAccountAddress1 :: AccountAddress
Right exampleAccountAddress1 = addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"

exampleAccountAddress2 :: AccountAddress
Right exampleAccountAddress2 = addressFromText "4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y"

exampleBlockHash1 :: BlockHash
exampleBlockHash1 = read "0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"

exampleBlockHash2 :: BlockHash
exampleBlockHash2 = read "0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"

exampleBlockHash3 :: BlockHash
exampleBlockHash3 = read "941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"

exampleTime1 :: UTCTime
Right exampleTime1 = parseTimeM False defaultTimeLocale "%s" "1000000"

exampleTime2 :: UTCTime
Right exampleTime2 = parseTimeM False defaultTimeLocale "%s" "1234567"

exampleTime3 :: UTCTime
Right exampleTime3 = parseTimeM False defaultTimeLocale "%s" "2000000"
