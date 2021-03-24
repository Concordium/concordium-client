{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Concordium.Client.Types.Transaction where

import           Concordium.Client.Types.Account
import qualified Concordium.ID.Types                 as IDTypes
import           Concordium.Types
import           Concordium.Crypto.EncryptedTransfers

import           Data.Aeson                          as AE
import qualified Data.Aeson.TH                       as AETH
import           Data.Text                           hiding (length, map)
import           GHC.Generics                        (Generic)
import qualified Concordium.Types.Transactions as Types
import qualified Concordium.Types as Types

-- |Cost of checking a header where the sender provides n signatures.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
-- This function does not account for the size of the transaction payload.
checkHeaderEnergyCost :: Int -> Energy
checkHeaderEnergyCost = fromIntegral . (+6) . (*53)

-- |Cost of checking a header where the sender provides n signatures.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost.
-- This function differs from Concordium.Client.Types.Transaction.checkHeaderEnergyCost,
-- in that it accounts for the payload.
checkHeaderEnergyCostWithPayload
  :: PayloadSize -- ^ The number of bytes of serialized payload.
  -> Int -- ^ The number of signatures the transaction signature contains.
  -> Types.Energy
checkHeaderEnergyCostWithPayload size nSig =
  checkHeaderEnergyCost nSig + (headerSize + fromIntegral size) `div` 232
  where headerSize = fromIntegral Types.transactionHeaderSize

-- |Cost of a simple transfer transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
simpleTransferEnergyCost :: Int -> Energy
simpleTransferEnergyCost = checkHeaderEnergyCost

encryptedTransferEnergyCost :: Int -> Energy
encryptedTransferEnergyCost = (+30000) . checkHeaderEnergyCost

-- |Cost of a stake delegation transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountDelegateEnergyCost :: Int -> Int -> Energy
accountDelegateEnergyCost instanceCount = (+100) . (+50*c) . checkHeaderEnergyCost
  where c = fromIntegral instanceCount

-- |Cost of a stake undelegation transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountUndelegateEnergyCost :: Int -> Int -> Energy
accountUndelegateEnergyCost = accountDelegateEnergyCost

-- |Cost of updating the account keys.
-- This must be kept in sync with Concordium.Scheduler.Cost
accountUpdateKeysEnergyCost :: Int -> Int -> Energy
accountUpdateKeysEnergyCost keyCount = (+5*c) . checkHeaderEnergyCost
  where c = fromIntegral keyCount

-- |Cost of updating credentials.
-- This must be kept in sync with Concordium.Scheduler.Cost
accountUpdateCredentialsEnergyCost :: Int -> Int -> Energy
accountUpdateCredentialsEnergyCost credCount = (+5*c) . checkHeaderEnergyCost
  where c = fromIntegral credCount

-- |Cost of updating the account keys.
-- This must be kept in sync with Concordium.Scheduler.Cost
accountAddKeysEnergyCost :: Int -> Int -> Energy
accountAddKeysEnergyCost = accountUpdateKeysEnergyCost

-- |Cost of updating the account keys.
-- This must be kept in sync with Concordium.Scheduler.Cost
accountRemoveKeysEnergyCost :: Int -> Energy
accountRemoveKeysEnergyCost = checkHeaderEnergyCost

-- |Cost of a baker add transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerAddEnergyCost :: Types.PayloadSize -> Int -> Energy
bakerAddEnergyCost psize numSigs = 3000 + checkHeaderEnergyCostWithPayload psize numSigs

-- |Cost of a baker set account transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerSetKeysEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerSetKeysEnergyCost size numSigs = 2980 + checkHeaderEnergyCostWithPayload size numSigs

-- |Cost of a baker set aggregation key transaction
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerSetAggregationKeyEnergyCost :: Int -> Energy
bakerSetAggregationKeyEnergyCost = (+ 2700) . checkHeaderEnergyCost

-- |Cost of a baker set-election-key transaction
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerSetElectionKeyEnergyCost :: Int -> Energy
bakerSetElectionKeyEnergyCost = (+ 90) . checkHeaderEnergyCost

-- |Cost of a baker remove transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerRemoveEnergyCost :: Int -> Energy
bakerRemoveEnergyCost = checkHeaderEnergyCost

-- |Cost to update a baker's stake.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateStakeEnergyCost :: Int -> Energy
bakerUpdateStakeEnergyCost = (+ 90) . checkHeaderEnergyCost

-- |Cost to update a baker's re-staking option.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateRestakeEnergyCost :: Int -> Energy
bakerUpdateRestakeEnergyCost = checkHeaderEnergyCost

-- |Cost of a "set election difficulty" transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
setElectionDifficultyEnergyCost :: Int -> Energy
setElectionDifficultyEnergyCost = checkHeaderEnergyCost

-- |Cost of moving funds from public to encrypted amount of an account.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountEncryptEnergyCost :: Int -> Energy
accountEncryptEnergyCost = (+100) . checkHeaderEnergyCost

-- |Cost of moving funds from encrypted to public balance of an account.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountDecryptEnergyCost :: Int -> Energy
accountDecryptEnergyCost = (+16000) . checkHeaderEnergyCost

-- |The cost of transfer with schedule.
transferWithScheduleEnergyCost ::
  Int -- ^ Number of releases.
  -> Int -- ^Number of signatures.
  -> Energy
transferWithScheduleEnergyCost numRels =
  let sizePremium = (94 + 16 * fromIntegral numRels) `div` 232
  -- the formula comes from counting the size of the transaction payload
  -- + the header size
  in (+ (100 * fromIntegral numRels)) . (+ sizePremium) . checkHeaderEnergyCost

-- |Transaction header type
-- To be populated when deserializing a JSON object.
data TransactionJSONHeader =
  TransactionJSONHeader
    {
      -- |Address of the sender.
      thSenderAddress    :: IDTypes.AccountAddress
      -- |Nonce of the account. If not present it should be derived
      -- from the context or queried to the state
    , thNonce            :: Maybe Nonce
      -- |Amount dedicated for the execution of this transaction.
    , thEnergyAmount     :: Energy
      -- |Absolute time after which transaction will not be executed.
    , thExpiry           :: TransactionExpiryTime
    }
  deriving (Eq, Show)

data ModuleSource
  = ByName Text
  | FromSource Text
  deriving (Eq, Show)

-- |Payload of a transaction
data TransactionJSONPayload
  = DeployModule
      { moduleName :: !Text
      } -- ^ Deploys a blockchain-ready version of the module, as retrieved from the Context
  | InitContract
      { amount       :: !Amount
      , moduleName   :: !Text
      , contractName :: !Text
      , parameter    :: !Text
      } -- ^ Initializes a specific Contract in a Module
  | Update
      { moduleName :: !Text
      , amount     :: !Amount
      , address    :: !ContractAddress
      , message    :: !Text
      } -- ^ Sends a specific message to a Contract
  | Transfer
      { toaddress :: !IDTypes.AccountAddress
      , amount    :: !Amount
      } -- ^ Transfers specific amount to the recipent
  | RemoveBaker
  | TransferToEncrypted{
      -- |Amount to transfer from public to encrypted balance of the account.
      tteAmount :: !Amount
      }
  | TransferToPublic {
      -- |Amount the user wishes to transfer to the public balance.
      ttpData :: !SecToPubAmountTransferData
      }
  | EncryptedAmountTransfer {
      eatTo :: !AccountAddress,
      eatData :: !EncryptedAmountTransferData
      }
  deriving (Show, Generic)

AETH.deriveFromJSON
  (AETH.defaultOptions
     {AETH.sumEncoding = AETH.TaggedObject "transactionType" "contents"})
  ''TransactionJSONPayload

-- |Transaction as retrieved from a JSON object
data TransactionJSON =
  TransactionJSON
    { metadata :: TransactionJSONHeader
    , payload  :: TransactionJSONPayload
    , keys     :: AccountKeyMap
    }
  deriving (Generic, Show)

instance AE.FromJSON TransactionJSON where
  parseJSON = withObject "Transaction" $ \v -> do
    thSenderAddress <- v .: "sender"
    thNonce <- v .:? "nonce"
    thEnergyAmount <- v .: "energyAmount"
    thExpiry <- v .: "expiry"
    let tHeader = TransactionJSONHeader {..}
    tPayload <- v .: "payload"
    keyMap <- v .: "keys"
    return $ TransactionJSON tHeader tPayload keyMap
