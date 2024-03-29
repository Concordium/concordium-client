{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Concordium.Client.Types.Transaction where

import Concordium.Client.Types.Account
import qualified Concordium.Cost as Cost
import Concordium.Crypto.EncryptedTransfers
import qualified Concordium.ID.Types as IDTypes
import Concordium.Types
import Concordium.Types.Execution (bakerKeysWithProofsSize)

import qualified Concordium.Types.Transactions as Types
import Data.Aeson as AE
import qualified Data.Aeson.TH as AETH
import Data.Text hiding (length, map)
import GHC.Generics (Generic)

-- | Base cost of checking the transaction. The cost is always at least this,
-- but in most cases it will have a transaction specific cost.
minimumCost ::
    -- | Size of the transaction payload in bytes
    PayloadSize ->
    -- | Number of signatures.
    Int ->
    Energy
minimumCost psize numSigs = Cost.baseCost totalSize numSigs
  where
    -- the total size of the transaction. The +1 is for the payload tag.
    totalSize = fromIntegral psize + Types.transactionHeaderSize

-- | Cost of a simple transfer transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
simpleTransferEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
simpleTransferEnergyCost psize numSigs = minimumCost psize numSigs + Cost.simpleTransferCost

simpleTransferPayloadSize :: PayloadSize
simpleTransferPayloadSize = 41

-- | Cost of an encrypted transfer transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
encryptedTransferEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
encryptedTransferEnergyCost psize numSigs = minimumCost psize numSigs + Cost.encryptedTransferCost

encryptedTransferPayloadSize :: PayloadSize
encryptedTransferPayloadSize = 2617

-- | Cost of updating the account keys.
--  This must be kept in sync with Concordium.Scheduler.Cost
accountUpdateKeysEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | The number of credentials on the account at the time of the update.
    Int ->
    -- | Number of keys that will belong to the credential after the update.
    Int ->
    -- | Number of signatures that will sign the transaction.
    Int ->
    Energy
accountUpdateKeysEnergyCost psize credentialCount keyCount numSigs = minimumCost psize numSigs + Cost.updateCredentialKeysCost credentialCount keyCount

-- | Cost of updating the credentials.
--  This must be kept in sync with Concordium.Scheduler.Cost
accountUpdateCredentialsEnergyCost ::
    -- | Size of the entire payload
    PayloadSize ->
    -- | The number of credentials on the account at the time of the update.
    Int ->
    -- | List of number of keys belonging to each new credential.
    [Int] ->
    -- | Number of signatures that will sign the transaction.
    Int ->
    Energy
accountUpdateCredentialsEnergyCost psize credentialCount keyCountList numSigs = minimumCost psize numSigs + Cost.updateCredentialsCost credentialCount keyCountList

-- | Cost of a baker add transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerAddEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerAddEnergyCost psize numSigs = minimumCost psize numSigs + Cost.addBakerCost

-- | Cost of a baker configure transaction without keys.
bakerConfigureEnergyCostWithoutKeys ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerConfigureEnergyCostWithoutKeys psize numSigs = minimumCost psize numSigs + Cost.configureBakerCostWithoutKeys

-- | Cost of a baker configure transaction with keys.
bakerConfigureEnergyCostWithKeys ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerConfigureEnergyCostWithKeys psize numSigs = minimumCost psize numSigs + Cost.configureBakerCostWithKeys

-- | The payload size of a configure baker transaction.
bakerConfigurePayloadSize ::
    -- | Capital
    Bool ->
    -- | Restake earnings
    Bool ->
    -- | Pool open status
    Bool ->
    -- | Keys
    Bool ->
    -- | Metadata length
    Maybe Int ->
    -- | Transaction fee commission
    Bool ->
    -- | Baking reward commission
    Bool ->
    -- | Finalization reward commission
    Bool ->
    PayloadSize
bakerConfigurePayloadSize hasCapital hasRestake hasPoolOpen hasKeys mMetadata hasTCom hasBCom hasFCom =
    3
        + (if hasCapital then 8 else 0)
        + (if hasRestake then 1 else 0)
        + (if hasPoolOpen then 1 else 0)
        + (if hasKeys then fromIntegral bakerKeysWithProofsSize else 0)
        + maybe 0 ((+ 2) . fromIntegral) mMetadata
        + (if hasTCom then 4 else 0)
        + (if hasBCom then 4 else 0)
        + (if hasFCom then 4 else 0)

-- | Cost of a baker set account transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerSetKeysEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerSetKeysEnergyCost psize numSigs = minimumCost psize numSigs + Cost.updateBakerKeysCost

-- | Cost of a baker remove transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerRemoveEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerRemoveEnergyCost psize numSigs = minimumCost psize numSigs + Cost.removeBakerCost

-- | Cost to update a baker's stake.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateStakeEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerUpdateStakeEnergyCost psize numSigs = minimumCost psize numSigs + Cost.updateBakerStakeCost

-- | Cost to update a baker's re-staking option.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateRestakeEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerUpdateRestakeEnergyCost psize numSigs = minimumCost psize numSigs + Cost.updateBakerRestakeCost

-- | Cost of a delegation configure transaction.
delegationConfigureEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
delegationConfigureEnergyCost psize numSigs = minimumCost psize numSigs + Cost.configureDelegationCost

-- | Payload size for a register delegation transaction
registerDelegationPayloadSize ::
    -- | Whether delegation is passive
    Bool ->
    PayloadSize
registerDelegationPayloadSize True = 13
registerDelegationPayloadSize False = 21

-- | Payload size for an update delegation transaction
updateDelegationPayloadSize ::
    -- | Whether the amount is updated
    Bool ->
    -- | Whether the restake is updated
    Bool ->
    -- | Whether the target is updated
    Bool ->
    -- | Whether the target is passive delegation
    Bool ->
    PayloadSize
updateDelegationPayloadSize updAmt updRestake updTarget targetPassiveDelegation = 3 + amt + restake + target
  where
    amt = if updAmt then 8 else 0
    restake = if updRestake then 1 else 0
    target = if updTarget then (if targetPassiveDelegation then 1 else 9) else 0

-- | Payload size for a remove delegation transaction
removeDelegationPayloadSize :: PayloadSize
removeDelegationPayloadSize = 11

-- | Cost of moving funds from public to encrypted amount of an account.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountEncryptEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
accountEncryptEnergyCost psize numSigs = minimumCost psize numSigs + Cost.transferToEncryptedCost

accountEncryptPayloadSize :: PayloadSize
accountEncryptPayloadSize = 9

-- | Cost of moving funds from encrypted to public balance of an account.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountDecryptEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
accountDecryptEnergyCost psize numSigs = minimumCost psize numSigs + Cost.transferToPublicCost

accountDecryptPayloadSize :: PayloadSize
accountDecryptPayloadSize = 1405

-- | The cost of transfer with schedule.
transferWithScheduleEnergyCost ::
    -- | Size of the payload.
    PayloadSize ->
    -- | Number of releases.
    Int ->
    -- | Number of signatures.
    Int ->
    Energy
transferWithScheduleEnergyCost psize numRels numSigs = minimumCost psize numSigs + Cost.scheduledTransferCost numRels

transferWithSchedulePayloadSize ::
    -- | Number of releases.
    Int ->
    PayloadSize
transferWithSchedulePayloadSize numRels = 32 + 1 + 1 + fromIntegral numRels * 16

-- | Transaction header type
--  To be populated when deserializing a JSON object.
data TransactionJSONHeader = TransactionJSONHeader
    { -- | Address of the sender.
      thSenderAddress :: IDTypes.AccountAddress,
      -- | Nonce of the account. If not present it should be derived
      --  from the context or queried to the state
      thNonce :: Maybe Nonce,
      -- | Amount dedicated for the execution of this transaction.
      thEnergyAmount :: Energy,
      -- | Absolute time after which transaction will not be executed.
      thExpiry :: TransactionExpiryTime
    }
    deriving (Eq, Show)

data ModuleSource
    = ByName Text
    | FromSource Text
    deriving (Eq, Show)

-- | Payload of a transaction
data TransactionJSONPayload
    = -- | Deploys a blockchain-ready version of the module, as retrieved from the Context
      DeployModule
        { moduleName :: !Text
        }
    | -- | Initializes a specific Contract in a Module
      InitContract
        { amount :: !Amount,
          moduleName :: !Text,
          contractName :: !Text,
          parameter :: !Text
        }
    | -- | Sends a specific message to a Contract
      Update
        { moduleName :: !Text,
          amount :: !Amount,
          address :: !ContractAddress,
          message :: !Text
        }
    | -- | Transfers specific amount to the recipent
      Transfer
        { toaddress :: !IDTypes.AccountAddress,
          amount :: !Amount
        }
    | RemoveBaker
    | TransferToEncrypted
        { -- | Amount to transfer from public to encrypted balance of the account.
          tteAmount :: !Amount
        }
    | TransferToPublic
        { -- | Amount the user wishes to transfer to the public balance.
          ttpData :: !SecToPubAmountTransferData
        }
    | EncryptedAmountTransfer
        { eatTo :: !AccountAddress,
          eatData :: !EncryptedAmountTransferData
        }
    deriving (Show, Generic)

AETH.deriveFromJSON
    ( AETH.defaultOptions
        { AETH.sumEncoding = AETH.TaggedObject "transactionType" "contents"
        }
    )
    ''TransactionJSONPayload

-- | Transaction as retrieved from a JSON object
data TransactionJSON = TransactionJSON
    { metadata :: TransactionJSONHeader,
      payload :: TransactionJSONPayload,
      keys :: AccountKeyMap
    }
    deriving (Generic, Show)

instance AE.FromJSON TransactionJSON where
    parseJSON = withObject "Transaction" $ \v -> do
        thSenderAddress <- v .: "sender"
        thNonce <- v .:? "nonce"
        thEnergyAmount <- v .: "energyAmount"
        thExpiry <- v .: "expiry"
        let tHeader = TransactionJSONHeader{..}
        tPayload <- v .: "payload"
        keyMap <- v .: "keys"
        return $ TransactionJSON tHeader tPayload keyMap
