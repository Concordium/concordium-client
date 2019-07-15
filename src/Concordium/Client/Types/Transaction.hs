{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Concordium.Client.Types.Transaction where

import qualified Concordium.Crypto.SHA256            as Hash
import           Concordium.Crypto.SignatureScheme   (SchemeId (..),
                                                      SignKey (..),
                                                      VerifyKey (..))
import qualified Concordium.Crypto.VRF               as VRF
import           Concordium.GlobalState.Transactions
import qualified Concordium.ID.Account               as AH
import qualified Concordium.ID.Types                 as IDTypes
import qualified Concordium.Scheduler.Types          as Types
import           Concordium.Types
import           Control.Applicative
import           Data.Aeson                          as AE
import qualified Data.Aeson.TH                       as AETH
import           Data.Aeson.Types                    (typeMismatch)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Base16              as BS16
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.FixedByteString                as FBS
import           Data.Maybe
import           Data.Serialize                      as S
import           Data.Text                           hiding (length, map)
import qualified Data.Text.Encoding                  as Text
import           Data.Word
import           GHC.Generics                        (Generic)

-- Number
instance FromJSON Nonce where
  parseJSON v = Nonce <$> parseJSON v

-- Number
instance FromJSON Energy where
  parseJSON v = Energy <$> parseJSON v

-- Number
instance FromJSON Amount where
  parseJSON v = Amount <$> parseJSON v

-- Length + data (serializes with `put :: Bytestring -> Put`)
instance FromJSON VerifyKey where
  parseJSON v = do
   verifKey <- parseJSON v
   let plainBs = fst . BS16.decode . Text.encodeUtf8 $ verifKey
   case S.decode . flip BS.append plainBs $ S.encode (fromIntegral . BS.length $ plainBs :: Word16) of
     Left e  -> fail e
     Right n -> return n

-- Data (serializes with `putByteString :: Bytestring -> Put`)
instance FromJSON BlockHash where
  parseJSON v = do
   hash <- parseJSON v
   case S.decode . fst . BS16.decode . Text.encodeUtf8 $ hash of
    Left e  -> fail e
    Right n -> return n

instance FromJSON AccountAddress where
  parseJSON v = AH.base58decodeAddr <$> parseJSON v
  parseJSONList v = map AH.base58decodeAddr <$> parseJSONList v

instance FromJSON Address where
  parseJSON (Object v) = do
    r <- v .:? "accountAddress"
    case r of
      Nothing -> AddressContract <$> (v .: "contractAddress")
      Just a  -> return (AddressAccount a)
  parseJSON invalid = typeMismatch "Address" invalid

-- Data (serializes with `putByteString :: Bytestring -> Put`)
instance FromJSON IDTypes.CredentialRegistrationID where
  parseJSON v = do
   crid <- parseJSON v
   case S.decode . fst . BS16.decode . Text.encodeUtf8 $ crid of
    Left e  -> fail e
    Right n -> return n

-- Length + data (serializes with `put :: Bytestring -> Put`)
instance FromJSON IDTypes.AnonimityRevokerIdentity where
  parseJSON v =do
   arid <- parseJSON v
   let plainBs = fst . BS16.decode . Text.encodeUtf8 $ arid
   case S.decode . flip BS.append plainBs $ S.encode (fromIntegral . BS.length $ plainBs :: Word16) of
    Left e  -> fail e
    Right n -> return n

-- Length + data (serializes with `put :: Bytestring -> Put`)
instance FromJSON IDTypes.IdentityProviderIdentity where
  parseJSON v = do
   ipid <- parseJSON v
   let plainBs = fst . BS16.decode . Text.encodeUtf8 $ ipid
   case S.decode . flip BS.append plainBs $ S.encode (fromIntegral . BS.length $ plainBs :: Word16) of
    Left e  -> fail e
    Right n -> return n

-- Enum
instance FromJSON SchemeId where
  parseJSON v = do
    toEnum <$> parseJSON v

instance FromJSON IDTypes.PolicyItem where
  parseJSON (Object v) = do
    -- Num
    idx <- v .: "index"
    -- Data (serializes with `fsbPut`)
    val <- v .: "value"
    case S.decode val of
       Left e  -> fail e
       Right n -> return $ IDTypes.PolicyItem idx n

instance FromJSON IDTypes.Policy where
  parseJSON (Object v) = do
    -- Num
    variant <- v .: "variant"
    -- Num
    expiry <- v .: "expiry"
    items <- v .: "items"
    return $ IDTypes.Policy variant expiry items

instance FromJSON IDTypes.CredentialDeploymentValues where
  parseJSON (Object v) = do
    scheme <- v .: "scheme"
    verif <- v .: "verifyKey"
    reg <- v .: "registrationId"
    idpi <- v .: "identityProvider"
    pol <- v .: "policy"
    return $ IDTypes.CredentialDeploymentValues scheme verif reg idpi pol

instance FromJSON IDTypes.CredentialDeploymentInformation where
  parseJSON (Object v) = do
    cdi <- v .: "values"
    proofs <- v .: "proofs"
    let plainBs = fst . BS16.decode . Text.encodeUtf8 $ proofs
    case S.decode . flip BS.append plainBs $ S.encode (fromIntegral . BS.length $ plainBs :: Word32) of
      Left e  -> fail e
      Right n -> return $ IDTypes.CredentialDeploymentInformation cdi n

-- Length + data (serializes with `put :: Bytestring -> Put`)
instance FromJSON IDTypes.AccountEncryptionKey where
  parseJSON v = do
   aek <- parseJSON v
   let plainBs = fst . BS16.decode . Text.encodeUtf8 $ aek
   case S.decode . flip BS.append plainBs $ S.encode (fromIntegral . BS.length $ plainBs :: Word16) of
    Left e  -> fail e
    Right n -> return n

-- Data (serializes with `putByteString :: Bytestring -> Put`)
instance FromJSON BakerElectionVerifyKey where
  parseJSON v = do
   b16 <- parseJSON v
   case S.decode . fst . BS16.decode . Text.encodeUtf8 $ b16 of
    Left e  -> fail e
    Right n -> return n

-- Data (serializes with `putByteString :: Bytestring -> Put`)
instance FromJSON Types.Proof where
  parseJSON v = fst . BS16.decode . Text.encodeUtf8 <$> parseJSON v

-- Number
instance FromJSON BakerId where
  parseJSON v = BakerId <$> parseJSON v

-- |Transaction header type
-- To be populated when deserializing a JSON object.
data TransactionJSONHeader =
  TransactionJSONHeader {
  -- |Verification key of the sender.
  thSenderKey          :: IDTypes.AccountVerificationKey
  -- |Nonce of the account. If not present it should be derived
  -- from the context or queried to the state
  , thNonce            :: Maybe Nonce
  -- |Amount dedicated for the execution of this transaction.
  , thGasAmount        :: Energy
  -- |Pointer to a finalized block. If this is too out of date at
  -- the time of execution the transaction is dropped
  , thFinalizedPointer :: BlockHash
    }
  deriving (Eq, Show)

-- |Payload of a transaction
data TransactionJSONPayload
  = DeployModule
      { moduleName :: Text
      } -- ^ Deploys a blockchain-ready version of the module, as retrieved from the Context
  | InitContract
      { amount       :: Amount
      , moduleName   :: Text
      , contractName :: Text
      , parameter    :: Text
      } -- ^ Initializes a specific Contract in a Module
  | Update
      { moduleName :: Text
      , amount     :: Amount
      , address    :: ContractAddress
      , message    :: Text
      } -- ^ Sends a specific message to a Contract
  | Transfer
      { toaddress :: Address
      , amount    :: Amount
      } -- ^ Transfers specific amount to the recipent
  | DeployCredential
      { credential :: IDTypes.CredentialDeploymentInformation
      } -- ^ Deploy credentials, creating a new account if one does not yet exist.
  | DeployEncryptionKey
      { key :: IDTypes.AccountEncryptionKey
      }
  | AddBaker
      { electionVerifyKey  :: BakerElectionVerifyKey
      , signatureVerifyKey :: BakerSignVerifyKey
      , bakerAccount       :: AccountAddress
      , proof              :: Types.Proof
      }
  | RemoveBaker
      { removeId :: BakerId
      , proof    :: Types.Proof
      }
  | UpdateBakerAccount
      { bakerId        :: BakerId
      , accountAddress :: AccountAddress
      , proof          :: Types.Proof
      }
  | UpdateBakerSignKey
      { bakerId    :: BakerId
      , newSignKey :: BakerSignVerifyKey
      , proof      :: Types.Proof
      }
  | DelegateStake
      { bakerId :: BakerId
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
    , signKey  :: SignKey
    }
  deriving (Generic, Show)

instance AE.FromJSON TransactionJSON where
  parseJSON (Object v) = do
    thSenderKey <- v .: "verifyKey"
    thNonce <- v .:? "nonce"
    thGasAmount <- v .: "gasAmount"
    thFinalizedPointer <- v .: "finalizedPointer"
    let tHeader = TransactionJSONHeader {..}
    tPayload <- v .: "payload"
    tSignKey <-
      SignKey . fst . BS16.decode . Text.encodeUtf8 <$> (v .: "signKey")
    return $ TransactionJSON tHeader tPayload tSignKey
  parseJSON invalid = typeMismatch "Transaction" invalid

-- |Creates a proper transaction header populating the Nonce if needed
makeTransactionHeaderWithNonce ::
     TransactionJSONHeader -> Types.Nonce -> Types.TransactionHeader
makeTransactionHeaderWithNonce (TransactionJSONHeader sk _ ga fp) nonce =
  makeTransactionHeader Ed25519 sk nonce ga fp
