{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module Concordium.Client.Types.Transaction where

import           Concordium.Client.Types.Account
import qualified Concordium.ID.Types                 as IDTypes
import           Concordium.Types
import           Concordium.Crypto.EncryptedTransfers
import qualified Concordium.Cost as Cost

import           Data.Aeson                          as AE
import qualified Data.Aeson.TH                       as AETH
import           Data.Text                           hiding (length, map)
import           GHC.Generics                        (Generic)
import qualified Concordium.Types.Transactions as Types
import qualified Concordium.Types as Types

-- | Base cost of checking the transaction. The cost is always at least this,
-- but in most cases it will have a transaction specific cost.
minimumCost :: PayloadSize -- ^ Size of the transaction payload in bytes
            -> Int -- ^ Number of signatures.
            -> Energy
minimumCost psize numSigs = Cost.baseCost totalSize numSigs
  where -- the total size of the transaction. The +1 is for the payload tag.
    totalSize = fromIntegral psize + Types.transactionHeaderSize

-- |Cost of a simple transfer transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
simpleTransferEnergyCost :: Int -> Energy
simpleTransferEnergyCost numSigs = minimumCost 41 numSigs + Cost.simpleTransferCost
    -- FIXME: After Simon's changes are done replace 41 with computed size

encryptedTransferEnergyCost :: Int -> Energy
encryptedTransferEnergyCost numSigs = minimumCost 2617 numSigs + Cost.encryptedTransferCost
    -- FIXME: After Simon's changes are done replace 2617 with computed size

-- |Cost of updating the account keys.
-- This must be kept in sync with Concordium.Scheduler.Cost
accountUpdateKeysEnergyCost ::
  Int -- ^ The number of credentials on the account at the time of the update.
  -> Int -- ^ Number of keys that will belong to the credential after the update.
  -> Int -- ^ Number of signatures that will sign the transaction.
  -> Energy
accountUpdateKeysEnergyCost credentialCount keyCount numSigs = minimumCost psize numSigs + Cost.updateCredentialKeysCost credentialCount keyCount
  where psize = 1 + 48 + 1 + fromIntegral keyCount * 49 + 1
    -- FIXME: After Simon's changes are done replace psize with computed size

-- |Cost of updating the credentials.
-- This must be kept in sync with Concordium.Scheduler.Cost
accountUpdateCredentialsEnergyCost ::
  Int -- ^ The number of credentials on the account at the time of the update.
  -> [Int] -- ^ List of number of keys belonging to each new credential.
  -> Int -- ^ Number of signatures that will sign the transaction.
  -> Energy
accountUpdateCredentialsEnergyCost credentialCount keyCountList numSigs = minimumCost psize numSigs + Cost.updateCredentialsCost credentialCount keyCountList
  where psize = 1 + 48 + 1 -- + fromIntegral keyCount * 49 + 1 -- TODO: fix this
    -- FIXME: After Simon's changes are done replace psize with computed size

-- |Cost of a baker add transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerAddEnergyCost :: Types.PayloadSize -> Int -> Energy
bakerAddEnergyCost psize numSigs = minimumCost psize numSigs + Cost.addBakerCost

-- |Cost of a baker set account transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerSetKeysEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerSetKeysEnergyCost psize numSigs = minimumCost psize numSigs + Cost.updateBakerKeysCost

-- |Cost of a baker remove transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerRemoveEnergyCost :: Int -> Energy
bakerRemoveEnergyCost numSigs = minimumCost 1 numSigs + Cost.removeBakerCost
    -- FIXME: After Simon's changes are done replace 1 with computed size

-- |Cost to update a baker's stake.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateStakeEnergyCost :: Int -> Energy
bakerUpdateStakeEnergyCost numSigs = minimumCost 9 numSigs + Cost.updateBakerStakeCost
    -- FIXME: After Simon's changes are done replace 9 with computed size

-- |Cost to update a baker's re-staking option.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateRestakeEnergyCost :: Int -> Energy
bakerUpdateRestakeEnergyCost numSigs = minimumCost 2 numSigs + Cost.updateBakerRestakeCost
    -- FIXME: After Simon's changes are done replace 2 with computed size

-- |Cost of moving funds from public to encrypted amount of an account.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountEncryptEnergyCost :: Int -> Energy
accountEncryptEnergyCost numSigs = minimumCost 9 numSigs + Cost.transferToEncryptedCost
    -- FIXME: After Simon's changes are done replace 9 with computed size

-- |Cost of moving funds from encrypted to public balance of an account.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountDecryptEnergyCost :: Int -> Energy
accountDecryptEnergyCost numSigs = minimumCost 1405 numSigs + Cost.transferToPublicCost
    -- FIXME: After Simon's changes are done replace 1405 with computed size

-- |The cost of transfer with schedule.
transferWithScheduleEnergyCost ::
  Int -- ^ Number of releases.
  -> Int -- ^Number of signatures.
  -> Energy
transferWithScheduleEnergyCost numRels numSigs = minimumCost (32 + 1 + 1 + fromIntegral numRels * 16) numSigs + Cost.scheduledTransferCost numRels
    -- FIXME: After Simon's changes are done replace (...) with computed size

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
