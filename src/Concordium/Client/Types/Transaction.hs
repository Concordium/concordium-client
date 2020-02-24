{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Concordium.Client.Types.Transaction where

import           Concordium.Crypto.SignatureScheme   (KeyPair(..))
import           Concordium.Crypto.Proofs
import qualified Concordium.ID.Types                 as IDTypes
import           Concordium.Types
import           Concordium.Types.Execution          as Types
import           Data.Aeson                          as AE
import qualified Data.Aeson.TH                       as AETH
import           Data.Aeson.Types                    (typeMismatch)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Base16              as BS16
import qualified Data.HashMap.Strict                 as Map
import           Data.Serialize                      as S
import           Data.Text                           hiding (length, map)
import qualified Data.Text.Encoding                  as Text
import           Data.Word
import           GHC.Generics                        (Generic)

instance FromJSON Address where
  parseJSON (Object v) = do
    r <- v .:? "accountAddress"
    case r of
      Nothing -> AddressContract <$> (v .: "contractAddress")
      Just a  -> return (AddressAccount a)
  parseJSON invalid = typeMismatch "Address" invalid

-- Length + data (serializes with `put :: Bytestring -> Put`)
instance FromJSON IDTypes.AccountEncryptionKey where
  parseJSON v = do
    aek <- parseJSON v
    let plainBs = fst . BS16.decode . Text.encodeUtf8 $ aek
    case S.decode . flip BS.append plainBs $
         S.encode (fromIntegral . BS.length $ plainBs :: Word16) of
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
  TransactionJSONHeader
  -- |Address of the sender.
    { thSenderAddress    :: IDTypes.AccountAddress
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
  = DeployModuleFromSource
      { moduleSource :: FilePath
      } -- ^ Read a serialized module from a file and deploy it.
  | DeployModule
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
      { electionVerifyKey    :: BakerElectionVerifyKey
      , signatureVerifyKey   :: BakerSignVerifyKey
      , aggregationVerifyKey :: BakerAggregationVerifyKey
      , bakerAccount         :: AccountAddress
      , proofSig             :: Dlog25519Proof
      , proofElection        :: Dlog25519Proof
      , proofAccounts        :: AccountOwnershipProof
      , proofAggregation     :: BakerAggregationProof
      }
  | RemoveBaker
      { removeId :: BakerId
      , proof    :: Types.Proof
      }
  | UpdateBakerAccount
      { bakerId        :: BakerId
      , accountAddress :: AccountAddress
      , proofBa        :: AccountOwnershipProof
      }
  | UpdateBakerSignKey
      { bakerId    :: BakerId
      , newSignKey :: BakerSignVerifyKey
      , proofBs    :: Dlog25519Proof
      }
  | DelegateStake
      { bakerId :: BakerId
      }
  deriving (Show, Generic)

AETH.deriveFromJSON
  (AETH.defaultOptions
     {AETH.sumEncoding = AETH.TaggedObject "transactionType" "contents"})
  ''TransactionJSONPayload

type KeyMap = Map.HashMap IDTypes.KeyIndex KeyPair

type SenderData = (IDTypes.AccountAddress, KeyMap)

-- |Transaction as retrieved from a JSON object
data TransactionJSON =
  TransactionJSON
    { metadata :: TransactionJSONHeader
    , payload  :: TransactionJSONPayload
    , keys     :: KeyMap
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
