-- |Provides functionality for loading transactions exported from the desktop wallet.
module Concordium.Client.Types.DesktopWalletTransaction(
    DesktopWalletTransaction(..),
    DesktopWalletUpdateInstruction(..),
    UpdateSignature(..),
    DWT(..)
) where

import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import Data.Serialize (runPut)
import qualified Data.Text as Text
import Text.Read (readEither)

import Concordium.Crypto.SignatureScheme
import Concordium.Types.Updates
import Concordium.Types.Transactions
import Concordium.Types.Execution
import Concordium.Types

-- |A transaction (with an unencoded payload).
-- This provides a 'FromJSON' instance that deserializes transactions exported by the desktop
-- wallet.
data DesktopWalletTransaction = DesktopWalletTransaction {
        -- |The transaction
        dwtAccountTransaction :: !AccountTransaction,
        -- |The unencoded payload
        dwtPayload :: !Payload
    } deriving (Eq, Show)

-- |A wrapper for an integer type that is serialized as a bigint object in JSON.
-- Deserialization reads the value as an 'Integer' and then 'fromInteger' to convert.
-- This can result in unchecked over/underflows, but this will only happen if the data
-- is bad.
newtype BigInt a = BigInt {unBigInt :: a}

instance Num a => FromJSON (BigInt a) where
    parseJSON = withObject "bigint" $ \o -> do
        t <- o .: "@type"
        unless (t == ("bigint" :: Text.Text)) $ fail "Expected a bigint"
        s <- o .: "value"
        case readEither s of
            Left err -> fail err
            Right res -> return $ BigInt (fromInteger res)

-- |A wrapper for an integer type that is serialized as a string in JSON.
-- Deserialization reads the value as an 'Integer' and then 'fromInteger' to convert.
-- This can result in unchecked over/underflows, but this will only happen if the data
-- is bad.
newtype StringNum a = StringNum { unStringNum :: a }

instance Num a => FromJSON (StringNum a) where
    parseJSON (String txt) = case readEither (Text.unpack txt) of
        Left err -> fail err
        Right res -> return $ StringNum (fromInteger res)
    parseJSON _ = fail "Expected a String"

-- |Wrapper for a scheduled release, providing a 'FromJSON' instance.
newtype ScheduleRelease = ScheduleRelease { unScheduleRelease :: (Timestamp, Amount) }

instance FromJSON ScheduleRelease where
    parseJSON = withObject "ScheduledRelease" $ \o -> ScheduleRelease <$> ((,) <$>
            (unStringNum <$> o .: "timestamp")
            <*>  o .: "amount")

-- |Wrapper for an index-value pair, with a 'FromJSON' instance.
newtype IndexValue i v = IndexValue {unIndexValue :: (i, v)}

instance (FromJSON i, FromJSON v) => FromJSON (IndexValue i v) where
    parseJSON = withObject "Index-Value" $ \o -> IndexValue <$> ((,) <$> o .: "index" <*> o .: "value")

instance FromJSON DesktopWalletTransaction where
    parseJSON = withObject "Desktop wallet transaction" $ \o -> do
        thSender <- o .: "sender"
        thNonce <- o .: "nonce"
        thEnergyAmount <- unStringNum <$> o .: "energyAmount"
        thExpiry <- unBigInt <$> o .: "expiry"
        transactionKind <- o .: "transactionKind"
        dwtPayload <- o .: "payload" >>= withObject "payload" (\payloadObj -> case transactionKind :: Int of
                3 -> do
                    tToAddress <- payloadObj .: "toAddress"
                    tAmount <- unStringNum <$> payloadObj .: "amount"
                    return Transfer{..}
                4 -> do
                    abElectionVerifyKey <- payloadObj .: "electionVerifyKey"
                    abSignatureVerifyKey <- payloadObj .: "signatureVerifyKey"
                    abAggregationVerifyKey <- payloadObj .: "aggregationVerifyKey"
                    abProofSig <- payloadObj .: "proofSignature"
                    abProofElection <- payloadObj .: "proofElection"
                    abProofAggregation <- payloadObj .: "proofAggregation"
                    abBakingStake <- unBigInt <$> payloadObj .: "bakingStake"
                    abRestakeEarnings <- payloadObj .: "restakeEarnings"
                    return AddBaker{..}
                5 -> return RemoveBaker
                6 -> UpdateBakerStake . unBigInt <$> payloadObj .: "stake"
                7 -> UpdateBakerRestakeEarnings <$> payloadObj .: "restakeEarnings"
                8 -> do
                    ubkElectionVerifyKey <- payloadObj .: "electionVerifyKey"
                    ubkSignatureVerifyKey <- payloadObj .: "signatureVerifyKey"
                    ubkAggregationVerifyKey <- payloadObj .: "aggregationVerifyKey"
                    ubkProofSig <- payloadObj .: "proofSignature"
                    ubkProofElection <- payloadObj .: "proofElection"
                    ubkProofAggregation <- payloadObj .: "proofAggregation"
                    return UpdateBakerKeys{..}
                19 -> do
                    twsTo <- payloadObj .: "toAddress"
                    twsSchedule <- fmap unScheduleRelease <$> payloadObj .: "schedule"
                    return TransferWithSchedule{..}
                20 -> do
                    ucNewCredInfos <- Map.fromList . fmap unIndexValue
                            <$> payloadObj .: "addedCredentials"
                    ucRemoveCredIds <- payloadObj .: "removedCredIds"
                    ucNewThreshold <- payloadObj .: "threshold"
                    return UpdateCredentials{..}
                _ -> fail "Unsupported transaction type."
                )
        let trPayload = encodePayload dwtPayload
        let thPayloadSize = payloadSize trPayload
        let trHeader = TransactionHeader{..}
        sigs <- o .: "signatures"
        let dwtAccountTransaction = makeAccountTransaction sigs trHeader trPayload
        return $ DesktopWalletTransaction{..}

data UpdateSignature = UpdateSignature {
        usSignature :: Signature,
        usAuthorizationPublicKey :: VerifyKey
    }
    deriving (Eq, Show)

instance FromJSON UpdateSignature where
    parseJSON = withObject "UpdateSignature" $ \o ->
        UpdateSignature <$> o .: "signature" <*> o .: "authorizationPublicKey"

data DesktopWalletUpdateInstruction = DesktopWalletUpdateInstruction {
        dwRawUpdateInstruction :: RawUpdateInstruction,
        dwSigningHash :: UpdateInstructionSignHash,
        dwSignatures :: [UpdateSignature]
    }
    deriving (Eq, Show)

instance FromJSON DesktopWalletUpdateInstruction where
    parseJSON = withObject "Desktop wallet UpdateInstruction" $ \o -> do
        updType <- o .: "type"
        ruiPayload <- o .: "payload" >>= withObject "payload" (\payloadObject -> case updType :: Int of
            2 -> EuroPerEnergyUpdatePayload <$> o .: "payload"
            3 -> MicroGTUPerEuroUpdatePayload <$> o .: "payload"
            4 -> FoundationAccountUpdatePayload <$> payloadObject .: "address"
            _ -> fail "Unsupported update type.")

        dwRawUpdateInstruction <- o .: "header" >>= withObject "header" (\headerObject -> do
            ruiSeqNumber <- headerObject .: "sequenceNumber"
            ruiEffectiveTime  <- headerObject .: "effectiveTime"
            ruiTimeout  <- headerObject .: "timeout"
            return RawUpdateInstruction{..})

        let dwSigningHash = makeUpdateInstructionSignHash $ runPut $
                putRawUpdateInstruction dwRawUpdateInstruction

        dwSignatures <- o .: "signatures"
        return DesktopWalletUpdateInstruction{..}

-- |A transaction or update instruction with JSON deserialization support for the desktop wallet.
data DWT
    = DWTransaction DesktopWalletTransaction
    | DWUpdateInstruction DesktopWalletUpdateInstruction
    deriving (Eq, Show)

instance FromJSON DWT where
    parseJSON = withObject "Desktop wallet transaction" $ \o ->
        if isJust (HM.lookup "type" o) then
            DWUpdateInstruction <$> parseJSON (Object o)
        else
            DWTransaction <$> parseJSON (Object o)
