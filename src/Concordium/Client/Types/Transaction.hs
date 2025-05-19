{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.Client.Types.Transaction where

import qualified Concordium.Cost as Cost
import Concordium.Types
import Concordium.Types.Execution as Types

import qualified Concordium.Types.Transactions as Types
import Data.Aeson as AE

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

-- | Cost of a token holder transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
tokenHolderTransactionEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
tokenHolderTransactionEnergyCost psize numSigs = minimumCost psize numSigs + Cost.tokenHolderBaseCost

-- | Cost of a token governance transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
tokenGovernanceTransactionEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
tokenGovernanceTransactionEnergyCost psize numSigs = minimumCost psize numSigs + Cost.tokenGovernanceBaseCost

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
    -- | Suspended
    Bool ->
    PayloadSize
bakerConfigurePayloadSize hasCapital hasRestake hasPoolOpen hasKeys mMetadata hasTCom hasBCom hasFCom hasSuspended =
    3
        + (if hasCapital then 8 else 0)
        + (if hasRestake then 1 else 0)
        + (if hasPoolOpen then 1 else 0)
        + (if hasKeys then fromIntegral bakerKeysWithProofsSize else 0)
        + maybe 0 ((+ 2) . fromIntegral) mMetadata
        + (if hasTCom then 4 else 0)
        + (if hasBCom then 4 else 0)
        + (if hasFCom then 4 else 0)
        + (if hasSuspended then 1 else 0)

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

-----------------------------------------------------------------

-- * JSON representation of a signed/partially-signed transaction

-----------------------------------------------------------------

-- | A 'SignedTransaction' is a transaction that is signed by an account (the signer)
-- with some keys. The representation might be a fully signed transaction ready to be
-- sent on-chain or a partially-signed transaction that needs additional signatures
-- added to be ready to be sent on-chain.
--
-- The `ToJSON` instance has the purpose converting the object into a human-readable
-- representation ready to be printed into a JSON file. This file can be shared among
-- different tools of the Concordium ecosystem for adding additional signatures.
--
--  The chosen representation is the minimal necessary data needed to construct the
-- 'TransactionSignHash' which is the value that is signed by the signer. The
-- 'TransactionSignHash' should be re-computed when processing a 'SignedTransaction'
-- (e.g. when adding signatures or sending the transaction on-chain).
data SignedTransaction = SignedTransaction
    { -- | Amount of energy dedicated to the execution of this transaction.
      stEnergy :: !Energy,
      -- | Absolute expiration time after which transaction will not be executed.
      stExpiryTime :: !TransactionExpiryTime,
      -- | Account nonce.
      stNonce :: !Nonce,
      -- | Signer account address.
      stSigner :: !AccountAddress,
      -- | The payload of the transaction.
      stPayload :: !Types.Payload,
      -- | Signatures generated by the signer account. This map might contain enough signatures to send the transaction on-chain or
      -- additional signatures are needed before the transaction is considered fully signed.
      stSignature :: !Types.TransactionSignature
    }
    deriving (Eq, Show)

-- | Implement `ToJSON` instance for `SignedTransaction`.
instance ToJSON SignedTransaction where
    toJSON SignedTransaction{..} =
        AE.object
            [ "version" AE..= signedTransactionVersion,
              "energy" AE..= stEnergy,
              "expiryTime" AE..= stExpiryTime,
              "nonce" AE..= stNonce,
              "signer" AE..= stSigner,
              "payload" AE..= stPayload,
              "signature" AE..= stSignature
            ]

-- Implement `FromJSON` instance for `SignedTransaction`.
instance FromJSON SignedTransaction where
    parseJSON = AE.withObject "SignedTransaction" $ \obj -> do
        stVersion <- obj AE..: "version"
        if stVersion /= signedTransactionVersion
            then fail $ "Unexpected version: " ++ show stVersion
            else do
                stEnergy <- obj AE..: "energy"
                stExpiryTime <- obj AE..: "expiryTime"
                stNonce <- obj AE..: "nonce"
                stSigner <- obj AE..: "signer"
                stSignature <- obj AE..: "signature"
                stPayload <- obj AE..: "payload"
                return SignedTransaction{..}

-- | The initial version of the above `SignedTransaction` JSON representation.
-- The version will be incremented when introducing a new format in the future.
signedTransactionVersion :: Int
signedTransactionVersion = 1
