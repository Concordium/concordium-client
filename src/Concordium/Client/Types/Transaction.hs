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
simpleTransferEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
simpleTransferEnergyCost psize numSigs = minimumCost psize numSigs + Cost.simpleTransferCost

simpleTransferPayloadSize :: PayloadSize
simpleTransferPayloadSize = 41

-- |Cost of an encrypted transfer transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
encryptedTransferEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
encryptedTransferEnergyCost psize numSigs = minimumCost psize numSigs + Cost.encryptedTransferCost

encryptedTransferPayloadSize :: PayloadSize
encryptedTransferPayloadSize = 2617

-- |Cost of updating the account keys.
-- This must be kept in sync with Concordium.Scheduler.Cost
accountUpdateKeysEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^ The number of credentials on the account at the time of the update.
  -> Int -- ^ Number of keys that will belong to the credential after the update.
  -> Int -- ^ Number of signatures that will sign the transaction.
  -> Energy
accountUpdateKeysEnergyCost psize credentialCount keyCount numSigs = minimumCost psize numSigs + Cost.updateCredentialKeysCost credentialCount keyCount

-- |Cost of updating the credentials.
-- This must be kept in sync with Concordium.Scheduler.Cost
accountUpdateCredentialsEnergyCost ::
  PayloadSize -- ^ Size of the entire payload
  -> Int -- ^ The number of credentials on the account at the time of the update.
  -> [Int] -- ^ List of number of keys belonging to each new credential.
  -> Int -- ^ Number of signatures that will sign the transaction.
  -> Energy
accountUpdateCredentialsEnergyCost psize credentialCount keyCountList numSigs = minimumCost psize numSigs + Cost.updateCredentialsCost credentialCount keyCountList

-- |Cost of a baker add transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerAddEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerAddEnergyCost psize numSigs = minimumCost psize numSigs + Cost.addBakerCost

-- |Cost of a baker configure transaction without keys.
bakerConfigureEnergyCostWithoutKeys ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerConfigureEnergyCostWithoutKeys psize numSigs = minimumCost psize numSigs + Cost.configureBakerCostWithoutKeys

-- |Cost of a baker configure transaction with keys.
bakerConfigureEnergyCostWithKeys ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerConfigureEnergyCostWithKeys psize numSigs = minimumCost psize numSigs + Cost.configureBakerCostWithKeys

-- |Cost of a baker set account transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerSetKeysEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerSetKeysEnergyCost psize numSigs = minimumCost psize numSigs + Cost.updateBakerKeysCost

-- |Cost of a baker remove transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerRemoveEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerRemoveEnergyCost psize numSigs = minimumCost psize numSigs + Cost.removeBakerCost

-- |Cost to update a baker's stake.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateStakeEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerUpdateStakeEnergyCost psize numSigs = minimumCost psize numSigs + Cost.updateBakerStakeCost

-- |Cost to update a baker's re-staking option.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateRestakeEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
bakerUpdateRestakeEnergyCost psize numSigs = minimumCost psize numSigs + Cost.updateBakerRestakeCost

-- |Cost of a delegation configure transaction.
delegationConfigureEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
delegationConfigureEnergyCost psize numSigs = minimumCost psize numSigs + Cost.configureDelegationCost

-- |Cost of moving funds from public to encrypted amount of an account.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountEncryptEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
accountEncryptEnergyCost psize numSigs = minimumCost psize numSigs + Cost.transferToEncryptedCost

accountEncryptPayloadSize :: PayloadSize
accountEncryptPayloadSize = 9

-- |Cost of moving funds from encrypted to public balance of an account.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountDecryptEnergyCost ::
  PayloadSize -- ^Size of the payload
  -> Int -- ^Number of signatures
  -> Energy
accountDecryptEnergyCost psize numSigs = minimumCost psize numSigs + Cost.transferToPublicCost

accountDecryptPayloadSize :: PayloadSize
accountDecryptPayloadSize = 1405

-- |The cost of transfer with schedule.
transferWithScheduleEnergyCost ::
  PayloadSize -- ^Size of the payload.
  -> Int -- ^ Number of releases.
  -> Int -- ^Number of signatures.
  -> Energy
transferWithScheduleEnergyCost psize numRels numSigs = minimumCost psize numSigs + Cost.scheduledTransferCost numRels

transferWithSchedulePayloadSize ::
  Int -- ^ Number of releases.
  -> PayloadSize
transferWithSchedulePayloadSize numRels = 32 + 1 + 1 + fromIntegral numRels * 16

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
