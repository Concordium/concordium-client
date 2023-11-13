module SimpleClientTests.BlockSpec where

import Concordium.Client.Output
import qualified Concordium.Types as Types
import Concordium.Types.Queries (BlockInfo (..))

import Control.Monad.Writer
import Data.Time.Clock
import Data.Time.Format

import Test.Hspec

blockSpec :: Spec
blockSpec = describe "block" $ do
    specify "with baker" $
        p exampleBlockInfoWithBaker
            `shouldBe` [ "Hash:                       0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389",
                         "Parent block:               0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd",
                         "Last finalized block:       941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795",
                         "Finalized:                  no",
                         "Receive time:               Mon, 12 Jan 1970 13:46:40 UTC",
                         "Arrive time:                Thu, 15 Jan 1970 06:56:07 UTC",
                         "Slot:                       1337",
                         "Block time:                 Sat, 24 Jan 1970 03:33:20 UTC",
                         "Height:                     121",
                         "Height since last genesis:  121",
                         "Genesis index:              0",
                         "Validator:                  53",
                         "Transaction count:          10",
                         "Transaction energy cost:    101 NRG",
                         "Transactions size:          11",
                         "Protocol version:           P1"
                       ]
    specify "without baker" $
        p exampleBlockInfoWithoutBaker
            `shouldBe` [ "Hash:                       0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389",
                         "Parent block:               0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd",
                         "Last finalized block:       941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795",
                         "Finalized:                  no",
                         "Receive time:               Mon, 12 Jan 1970 13:46:40 UTC",
                         "Arrive time:                Thu, 15 Jan 1970 06:56:07 UTC",
                         "Slot:                       1337",
                         "Block time:                 Sat, 24 Jan 1970 03:33:20 UTC",
                         "Height:                     121",
                         "Height since last genesis:  121",
                         "Genesis index:              0",
                         "Validator:                  none",
                         "Transaction count:          10",
                         "Transaction energy cost:    101 NRG",
                         "Transactions size:          11",
                         "Protocol version:           P3"
                       ]
  where
    p = execWriter . printBlockInfo

exampleBlockInfoWithBaker :: BlockInfo
exampleBlockInfoWithBaker =
    BlockInfo
        { biBlockHash = exampleBlockHash1,
          biBlockParent = exampleBlockHash2,
          biBlockLastFinalized = exampleBlockHash3,
          biFinalized = False,
          biBlockReceiveTime = exampleTime1,
          biBlockArriveTime = exampleTime2,
          biBlockSlot = Just 1337,
          biBlockHeight = 121,
          biGenesisIndex = 0,
          biEraBlockHeight = 121,
          biBlockSlotTime = exampleTime3,
          biBlockBaker = Just 53,
          biTransactionCount = 10,
          biTransactionEnergyCost = 101,
          biTransactionsSize = 11,
          biBlockStateHash = exampleStateHash,
          biProtocolVersion = Types.P1,
          biRound = Nothing,
          biEpoch = Nothing
        }

exampleBlockInfoWithoutBaker :: BlockInfo
exampleBlockInfoWithoutBaker =
    BlockInfo
        { biBlockHash = exampleBlockHash1,
          biBlockParent = exampleBlockHash2,
          biBlockLastFinalized = exampleBlockHash3,
          biFinalized = False,
          biBlockReceiveTime = exampleTime1,
          biBlockArriveTime = exampleTime2,
          biBlockSlot = Just 1337,
          biBlockSlotTime = exampleTime3,
          biBlockHeight = 121,
          biGenesisIndex = 0,
          biEraBlockHeight = 121,
          biBlockBaker = Nothing,
          biTransactionCount = 10,
          biTransactionEnergyCost = 101,
          biTransactionsSize = 11,
          biBlockStateHash = exampleStateHash,
          biProtocolVersion = Types.P3,
          biRound = Nothing,
          biEpoch = Nothing
        }

exampleBlockHash1 :: Types.BlockHash
exampleBlockHash1 = read "0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"

exampleBlockHash2 :: Types.BlockHash
exampleBlockHash2 = read "0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"

exampleBlockHash3 :: Types.BlockHash
exampleBlockHash3 = read "941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"

exampleStateHash :: Types.StateHash
exampleStateHash = read "8cfbc61b88ffc60cf0b052d90314400630304736d7da7ae7dc0a445d7c7c86cd"

exampleTime1 :: UTCTime
exampleTime1 = parseTimeOrError False defaultTimeLocale "%s" "1000000"

exampleTime2 :: UTCTime
exampleTime2 = parseTimeOrError False defaultTimeLocale "%s" "1234567"

exampleTime3 :: UTCTime
exampleTime3 = parseTimeOrError False defaultTimeLocale "%s" "2000000"
