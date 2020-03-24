module SimpleClientTests.BlockSpec where

import Concordium.Client.Cli
import Concordium.Client.Output
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.Crypto.ByteStringHelpers as BSH

import Control.Monad.Writer
import Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format

import Test.Hspec

blockSpec :: Spec
blockSpec = describe "block" $ do
  specify "with baker" $ p exampleBlockInfoWithBaker `shouldBe`
    [ "Hash:                       0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"
    , "Parent block:               0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
    , "Last finalized block:       941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"
    , "Finalized:                  no"
    , "Receive time:               Mon, 12 Jan 1970 13:46:40 UTC"
    , "Arrive time:                Thu, 15 Jan 1970 06:56:07 UTC"
    , "Slot:                       1337"
    , "Slot time:                  Sat, 24 Jan 1970 03:33:20 UTC"
    , "Baker:                      53"
    , "Transaction count:          10"
    , "Transaction energy cost:    101 NRG"
    , "Transactions size:          11"
    , "Transaction execution cost: 102 GTU"
    , "Total amount:               321 GTU"
    , "Total encrypted amount:     123 GTU"
    , "Central bank amount:        42 GTU"
    , "Minted amount per slot:     21 GTU" ]
  specify "without baker" $ p exampleBlockInfoWithoutBaker `shouldBe`
    [ "Hash:                       0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"
    , "Parent block:               0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"
    , "Last finalized block:       941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"
    , "Finalized:                  no"
    , "Receive time:               Mon, 12 Jan 1970 13:46:40 UTC"
    , "Arrive time:                Thu, 15 Jan 1970 06:56:07 UTC"
    , "Slot:                       1337"
    , "Slot time:                  Sat, 24 Jan 1970 03:33:20 UTC"
    , "Baker:                      none"
    , "Transaction count:          10"
    , "Transaction energy cost:    101 NRG"
    , "Transactions size:          11"
    , "Transaction execution cost: 102 GTU"
    , "Total amount:               321 GTU"
    , "Total encrypted amount:     123 GTU"
    , "Central bank amount:        42 GTU"
    , "Minted amount per slot:     21 GTU" ]
  where p = execWriter . printBlockInfo

exampleBlockInfoWithBaker :: BlockInfoResult
exampleBlockInfoWithBaker =
  BlockInfoResult
  { birBlockHash = exampleBlockHash1
  , birBlockParent = exampleBlockHash2
  , birBlockLastFinalized = exampleBlockHash3
  , birFinalized = False
  , birBlockReceiveTime = exampleTime1
  , birBlockArriveTime = exampleTime2
  , birBlockSlot = 1337
  , birBlockSlotTime = exampleTime3
  , birBlockBaker = Just 53
  , birTransactionCount = 10
  , birTransactionEnergyCost = 101
  , birTransactionsSize = 11
  , birExecutionCost = 102
  , birTotalAmount = 321
  , birTotalEncryptedAmount = 123
  , birCentralBankAmount = 42
  , birMintedAmountPerSlot = 21 }

exampleBlockInfoWithoutBaker :: BlockInfoResult
exampleBlockInfoWithoutBaker =
  BlockInfoResult
  { birBlockHash = exampleBlockHash1
  , birBlockParent = exampleBlockHash2
  , birBlockLastFinalized = exampleBlockHash3
  , birFinalized = False
  , birBlockReceiveTime = exampleTime1
  , birBlockArriveTime = exampleTime2
  , birBlockSlot = 1337
  , birBlockSlotTime = exampleTime3
  , birBlockBaker = Nothing
  , birTransactionCount = 10
  , birTransactionEnergyCost = 101
  , birTransactionsSize = 11
  , birExecutionCost = 102
  , birTotalAmount = 321
  , birTotalEncryptedAmount = 123
  , birCentralBankAmount = 42
  , birMintedAmountPerSlot = 21 }

exampleBlockHash1 :: Types.BlockHash
exampleBlockHash1 = read "0a5d64f644461d95315a781475b83f723f74d1c21542bd4f3e234d6173374389"

exampleBlockHash2 :: Types.BlockHash
exampleBlockHash2 = read "0f71eeca9f0a497dc4427cab0544f2bcb820b328ad97be29181e212edea708fd"

exampleBlockHash3 :: Types.BlockHash
exampleBlockHash3 = read "941c24374cd077de2120fb58732306c3115a08bb7b7cda120a04fecc412b1795"

exampleTime1 :: UTCTime
Right exampleTime1 = parseTimeM False defaultTimeLocale "%s" "1000000"

exampleTime2 :: UTCTime
Right exampleTime2 = parseTimeM False defaultTimeLocale "%s" "1234567"

exampleTime3 :: UTCTime
Right exampleTime3 = parseTimeM False defaultTimeLocale "%s" "2000000"
