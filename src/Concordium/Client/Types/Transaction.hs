{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.Client.Types.Transaction where

import qualified Concordium.Cost as Cost
import Concordium.Types
import Concordium.Types.Execution as Types

import Concordium.Client.Types.Account
import qualified Concordium.Types as Types
import Concordium.Types.Transactions (AccountTransaction (AccountTransaction), AccountTransactionV1 (AccountTransactionV1))
import qualified Concordium.Types.Transactions as Types
import Data.Aeson as AE
import qualified Data.Map as Map

-- | Base cost of checking the normal (i.e. non-extended) transaction.
-- The cost is always at least this, but in most cases it will have a
-- transaction specific cost.
minimumCostNormal ::
    -- | Size of the transaction payload in bytes
    PayloadSize ->
    -- | Number of signatures.
    Int ->
    Energy
minimumCostNormal psize numSigs = Cost.baseCost totalSize numSigs
  where
    -- the total size of the transaction. The +1 is for the payload tag.
    totalSize = fromIntegral psize + Types.transactionHeaderSize

-- | The options of the extended transaction format which influence the
-- cost of the transaction.
newtype ExtendedCostOptions = ExtendedCostOptions
    { -- | Whether the transaction is sponsored
      hasSponsor :: Bool
    }

-- | Base cost of checking the transaction. The cost is always at least this,
-- but in most cases it will have a transaction specific cost.
minimumCost ::
    -- | Size of the transaction payload in bytes
    PayloadSize ->
    -- | Number of signatures.
    Int ->
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
minimumCost psize numSigs (Just extOpts) = minimumCostExtended psize numSigs extOpts
minimumCost psize numSigs Nothing = minimumCostNormal psize numSigs

-- | Base cost of checking the extended transaction. The cost is always at least this,
-- but in most cases it will have a transaction specific cost.
minimumCostExtended ::
    -- | Size of the transaction payload in bytes
    PayloadSize ->
    -- | Number of signatures.
    Int ->
    -- | The extended transaction options influencing the transaction cost
    ExtendedCostOptions ->
    Energy
minimumCostExtended psize numSigs extOpts = Cost.baseCost totalSize numSigs
  where
    -- the size of the options specified for the extended transactions.
    optsSize = if hasSponsor extOpts then 32 else 0
    -- the total size of the transaction. 2 is for the bitmap in the v1 transaction format.
    totalSize = 2 + Types.transactionHeaderSize + fromIntegral psize + optsSize

-- | Cost of a simple transfer transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
simpleTransferEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
simpleTransferEnergyCost psize numSigs = (+ Cost.simpleTransferCost) . minimumCost psize numSigs

-- | Cost of a token transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
tokenUpdateTransactionEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | The energy cost of the specific operation
    Energy ->
    -- | Number of signatures
    Int ->
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
tokenUpdateTransactionEnergyCost psize opCost numSigs = (+ (Cost.tokenUpdateBaseCost + opCost)) . minimumCost psize numSigs

simpleTransferPayloadSize :: PayloadSize
simpleTransferPayloadSize = 41

-- | Cost of an encrypted transfer transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
encryptedTransferEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
encryptedTransferEnergyCost psize numSigs = (+ Cost.encryptedTransferCost) . minimumCost psize numSigs

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
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
accountUpdateKeysEnergyCost psize credentialCount keyCount numSigs = (+ Cost.updateCredentialKeysCost credentialCount keyCount) . minimumCost psize numSigs

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
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
accountUpdateCredentialsEnergyCost psize credentialCount keyCountList numSigs = (+ Cost.updateCredentialsCost credentialCount keyCountList) . minimumCost psize numSigs

-- | Cost of a baker add transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerAddEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerAddEnergyCost psize numSigs = minimumCostNormal psize numSigs + Cost.addBakerCost

-- | Cost of a baker configure transaction without keys.
bakerConfigureEnergyCostWithoutKeys ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
bakerConfigureEnergyCostWithoutKeys psize numSigs = (+ Cost.configureBakerCostWithoutKeys) . minimumCost psize numSigs

-- | Cost of a baker configure transaction with keys.
bakerConfigureEnergyCostWithKeys ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
bakerConfigureEnergyCostWithKeys psize numSigs = (+ Cost.configureBakerCostWithKeys) . minimumCost psize numSigs

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
bakerSetKeysEnergyCost psize numSigs = minimumCostNormal psize numSigs + Cost.updateBakerKeysCost

-- | Cost of a baker remove transaction.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerRemoveEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerRemoveEnergyCost psize numSigs = minimumCostNormal psize numSigs + Cost.removeBakerCost

-- | Cost to update a baker's stake.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateStakeEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerUpdateStakeEnergyCost psize numSigs = minimumCostNormal psize numSigs + Cost.updateBakerStakeCost

-- | Cost to update a baker's re-staking option.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerUpdateRestakeEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    Energy
bakerUpdateRestakeEnergyCost psize numSigs = minimumCostNormal psize numSigs + Cost.updateBakerRestakeCost

-- | Cost of a delegation configure transaction.
delegationConfigureEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
delegationConfigureEnergyCost psize numSigs = (+ Cost.configureDelegationCost) . minimumCost psize numSigs

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
accountEncryptEnergyCost psize numSigs = minimumCostNormal psize numSigs + Cost.transferToEncryptedCost

accountEncryptPayloadSize :: PayloadSize
accountEncryptPayloadSize = 9

-- | Cost of moving funds from encrypted to public balance of an account.
--  This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountDecryptEnergyCost ::
    -- | Size of the payload
    PayloadSize ->
    -- | Number of signatures
    Int ->
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
accountDecryptEnergyCost psize numSigs = (+ Cost.transferToPublicCost) . minimumCost psize numSigs

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
    -- | The extended transaction options influencing the transaction cost
    Maybe ExtendedCostOptions ->
    Energy
transferWithScheduleEnergyCost psize numRels numSigs = (+ Cost.scheduledTransferCost numRels) . minimumCost psize numSigs

transferWithSchedulePayloadSize ::
    -- | Number of releases.
    Int ->
    PayloadSize
transferWithSchedulePayloadSize numRels = 32 + 1 + 1 + fromIntegral numRels * 16

-- | Contains possible transaction variants.
data Transaction
    = -- | A "normal" transaction, i.e. the original transaction format
      NormalTransaction {tnTransaction :: Types.AccountTransaction}
    | -- | An "extended" transaction, i.e. the extended transaction format
      ExtendedTransaction {teTransaction :: Types.AccountTransactionV1}

-- | converts a 'Transaction' to its corresponding block item representation.
transactionBlockItem :: Transaction -> Types.BareBlockItem
transactionBlockItem NormalTransaction{tnTransaction = tx} = Types.NormalTransaction{biTransaction = tx}
transactionBlockItem ExtendedTransaction{teTransaction = tx} = Types.ExtendedTransaction{biTransactionV1 = tx}

-- | converts a 'Transaction' to a 'SignableTransaction'.
transactionToSignable :: ProtocolVersion -> Transaction -> Either String SignableTransaction
transactionToSignable pv NormalTransaction{tnTransaction = tx} = transactionToSignable' pv tx
transactionToSignable pv ExtendedTransaction{teTransaction = tx} = transactionToSignable' pv tx

-- | converts any 'TransactionData' instance to a 'SignableTransaction'.
transactionToSignable' :: forall t. (Types.TransactionData t) => ProtocolVersion -> t -> Either String SignableTransaction
transactionToSignable' pv tx = do
    payload <- case Types.promoteProtocolVersion pv of
        Types.SomeProtocolVersion spv -> Types.decodePayload spv $ Types.transactionPayload tx
    let header = Types.transactionHeader tx
    return
        SignableTransaction
            { stEnergy = Types.transactionGasAmount tx,
              stExpiryTime = Types.thExpiry header,
              stNonce = Types.thNonce header,
              stSigner = Types.thSender header,
              stSponsor = Types.transactionSponsor tx,
              stPayload = payload,
              stSignature = Types.transactionSignature tx,
              stSponsorSignature = Types.transactionSponsorSignature tx,
              stExtended = Types.transactionIsExtended tx
            }

-- | converts a 'SignableTransaction to a 'Transaction'. This potentially constructs an invalid transaction
-- due to missing signature from transaction sender, so this should be checked.
transactionFromSignable :: SignableTransaction -> Transaction
transactionFromSignable stx =
    let SignableTransaction{..} = stx
        encPayload = Types.encodePayload $ stPayload
        headerV0 = Types.TransactionHeader stSigner stNonce stEnergy (Types.payloadSize encPayload) stExpiryTime
    in  case stExtended of
            False ->
                let signHash = Types.transactionSignHashFromHeaderPayload headerV0 encPayload
                    tnTransaction = Types.AccountTransaction stSignature headerV0 encPayload signHash
                in  NormalTransaction{..}
            True ->
                let headerV1 = Types.TransactionHeaderV1 headerV0 stSponsor
                    signHash = Types.transactionV1SignHashFromHeaderPayload headerV1 encPayload
                    signatures = Types.TransactionSignaturesV1{tsv1Sponsor = stSponsorSignature, tsv1Sender = stSignature}
                    teTransaction = Types.AccountTransactionV1 signatures headerV1 encPayload signHash
                in  ExtendedTransaction{..}

-- | Format the header and the encoded transaction payload and return a Transaction.
unsignedTransaction ::
    Types.EncodedPayload ->
    Types.AccountAddress ->
    Types.Energy ->
    Types.Nonce ->
    Types.TransactionExpiryTime ->
    TransactionFormat ->
    Transaction
unsignedTransaction encPayload sender energy nonce expiry version = case version of
    NormalFormat -> NormalTransaction{tnTransaction = Types.makeAccountTransaction emptySignature headerV0 encPayload}
    ExtendedFormat ->
        -- TODO: SPO-65 add sponsor address
        let headerV1 = Types.TransactionHeaderV1{thv1Sponsor = Nothing, thv1HeaderV0 = headerV0}
        in  ExtendedTransaction{teTransaction = Types.makeAccountTransactionV1 (Types.TransactionSignaturesV1 emptySignature Nothing) headerV1 encPayload}
  where
    headerV0 =
        Types.TransactionHeader
            { thSender = sender,
              thPayloadSize = Types.payloadSize encPayload,
              thNonce = nonce,
              thEnergyAmount = energy,
              thExpiry = expiry
            }
    emptySignature = Types.TransactionSignature mempty

-- | An enum for the different transaction formats.
data TransactionFormat = NormalFormat | ExtendedFormat

-- | Sign a transaction payload and configuration into a "normal" AccountTransaction.
encodeAndSignTransaction ::
    Types.Payload ->
    Types.AccountAddress ->
    Types.Energy ->
    Types.Nonce ->
    Types.TransactionExpiryTime ->
    AccountKeyMap ->
    TransactionFormat ->
    Transaction
encodeAndSignTransaction txPayload = formatAndSignTransaction (Types.encodePayload txPayload)

-- | Format the header of the transaction and sign it together with the encoded transaction payload and return a Transaction.
formatAndSignTransaction ::
    Types.EncodedPayload ->
    Types.AccountAddress ->
    Types.Energy ->
    Types.Nonce ->
    Types.TransactionExpiryTime ->
    AccountKeyMap ->
    TransactionFormat ->
    Transaction
formatAndSignTransaction encPayload sender energy nonce expiry keys version = case unsigned of
    NormalTransaction{tnTransaction = AccountTransaction{..}} ->
        NormalTransaction{tnTransaction = signEncodedTransaction atrPayload atrHeader keys}
    ExtendedTransaction{teTransaction = AccountTransactionV1{..}} ->
        ExtendedTransaction{teTransaction = signEncodedTransactionExt atrv1Payload atrv1Header keys}
  where
    unsigned = unsignedTransaction encPayload sender energy nonce expiry version

-- | Sign an encoded transaction payload, and header with the account key map
-- and return a "normal" AccountTransaction.
signEncodedTransaction ::
    Types.EncodedPayload ->
    Types.TransactionHeader ->
    AccountKeyMap ->
    Types.AccountTransaction
signEncodedTransaction encPayload header accKeys =
    let keys = Map.toList $ fmap Map.toList accKeys
    in  Types.signTransaction keys header encPayload

-- | Sign an encoded transaction payload, and header with the account key map
-- and return an "extended" AccountTransaction.
signEncodedTransactionExt ::
    Types.EncodedPayload ->
    Types.TransactionHeaderV1 ->
    AccountKeyMap ->
    Types.AccountTransactionV1
signEncodedTransactionExt encPayload header accKeys =
    let keys = Map.toList $ fmap Map.toList accKeys
    in  Types.signTransactionV1 keys header encPayload

-----------------------------------------------------------------

-- * JSON representation of a signed/partially-signed transaction

-----------------------------------------------------------------

-- | A 'SignableTransaction' is a transaction that can be signed by an account
-- with some keys. The representation might be a fully signed transaction ready to be
-- sent on-chain or a partially-/non-signed transaction that needs additional signatures
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
data SignableTransaction = SignableTransaction
    { -- | Amount of energy dedicated to the execution of this transaction.
      stEnergy :: !Energy,
      -- | Absolute expiration time after which transaction will not be executed.
      stExpiryTime :: !TransactionExpiryTime,
      -- | Account nonce.
      stNonce :: !Nonce,
      -- | Sender account address.
      stSigner :: !AccountAddress,
      -- | Sponsor account address.
      stSponsor :: !(Maybe AccountAddress),
      -- | The payload of the transaction.
      stPayload :: !Types.Payload,
      -- | Signatures generated for the sender account. This map might contain enough signatures to send the transaction on-chain or
      -- additional signatures are needed before the transaction is considered fully signed.
      stSignature :: !Types.TransactionSignature,
      -- | Signatures generated for the sponsor account. This map might contain enough signatures to send the transaction on-chain or
      -- additional signatures are needed before the transaction is considered fully signed.
      stSponsorSignature :: !(Maybe Types.TransactionSignature),
      -- | Whether the signed transaction should be represented in its extended format.
      stExtended :: !Bool
    }
    deriving (Eq, Show)

-- | Implement `ToJSON` instance for `SignableTransaction`.
instance ToJSON SignableTransaction where
    toJSON SignableTransaction{..} =
        AE.object
            ( [ "version" AE..= signableTransactionVersion,
                "energy" AE..= stEnergy,
                "expiryTime" AE..= stExpiryTime,
                "nonce" AE..= stNonce,
                "signer" AE..= stSigner,
                "payload" AE..= stPayload,
                "signature" AE..= stSignature,
                "extended" AE..= stExtended
              ]
                ++ maybe [] (\s -> ["sponsor" AE..= s]) stSponsor
                ++ maybe [] (\s -> ["signatureSponsor" AE..= s]) stSponsorSignature
            )

-- Implement `FromJSON` instance for `SignableTransaction`.
instance FromJSON SignableTransaction where
    parseJSON = AE.withObject "SignableTransaction" $ \obj -> do
        stVersion <- obj AE..: "version"
        if stVersion /= signableTransactionVersion
            then fail $ "Unexpected version: " ++ show stVersion
            else do
                stEnergy <- obj AE..: "energy"
                stExpiryTime <- obj AE..: "expiryTime"
                stNonce <- obj AE..: "nonce"
                stSigner <- obj AE..: "signer"
                stSponsor <- obj AE..:? "sponsor"
                stSignature <- obj AE..: "signature"
                stSponsorSignature <- obj AE..:? "signatureSponsor"
                stPayload <- obj AE..: "payload"

                stExtended <- obj AE..:? "extended" AE..!= False

                case (stExtended, stSponsor, stSponsorSignature) of
                    (False, Just _, _) -> fail "Non-extended transaction cannot have sponsor"
                    (False, _, Just _) -> fail "Non-extended transaction cannot have sponsor signature"
                    _ -> return SignableTransaction{..}

-- | The initial version of the above `SignableTransaction` JSON representation.
-- The version will be incremented when introducing a new format in the future.
signableTransactionVersion :: Int
signableTransactionVersion = 1

-- | Sign a "SignableTransaction", adding the signature corresponding to the given "AccountKeyMap" onto
-- the transaction. If signatures already exist, they will be merged with the ones created.
signSignableTransaction :: SignableTransaction -> AccountKeyMap -> SignableTransaction
signSignableTransaction stx keys =
    let SignableTransaction{..} = stx
        existingSignature = Types.tsSignatures stSignature
        headerV0 = Types.TransactionHeader stSigner stNonce stEnergy (Types.payloadSize encPayload) stExpiryTime
        encPayload = Types.encodePayload $ stPayload

        newSignature
            | stExtended =
                let headerV1 = Types.TransactionHeaderV1 headerV0 (stSponsor)
                    signed = signEncodedTransactionExt encPayload headerV1 keys
                in  Types.tsSignatures $ Types.tsv1Sender $ Types.atrv1Signature signed
            | otherwise =
                let signed = signEncodedTransaction encPayload headerV0 keys
                in  Types.tsSignatures $ Types.atrSignature signed

        mergedSignature = Types.TransactionSignature $ Map.unionWith Map.union existingSignature newSignature
    in  stx{stSignature = mergedSignature}
