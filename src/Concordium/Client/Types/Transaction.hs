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
import qualified Concordium.ID.Attributes            as IDA
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
import           Data.Serialize.Get
import           Data.Text                           hiding (map)
import qualified Data.Text.Encoding                  as Text
import           Data.Word
import           GHC.Generics                        (Generic)

instance FromJSON Nonce where
  parseJSON v = do
   nonce <- parseJSON v
   case decode (BSL.fromStrict $ Text.encodeUtf8 nonce) of
    Nothing -> fail "Error decoding Nonce"
    Just n  -> return n

instance FromJSON Energy where
  parseJSON v = do
   energy <- parseJSON v
   case decode (BSL.fromStrict $ Text.encodeUtf8 energy) of
    Nothing -> fail "Error decoding Energy"
    Just e  -> return e

instance FromJSON Amount where
  parseJSON v = do
   amount <- parseJSON v
   case decode (BSL.fromStrict $ Text.encodeUtf8 amount) of
    Nothing -> fail "Error decoding Amount"
    Just a  -> return a

instance FromJSON VerifyKey where
  parseJSON v = do
   verifKey <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ verifKey) of
    Nothing -> fail "Error decoding VerifyKey"
    Just v  -> return v

instance FromJSON BlockHash where
  parseJSON v = do
   hash <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ hash) of
    Nothing -> fail "Error decoding Amount"
    Just h  -> return h

instance FromJSON AccountAddress where
  parseJSON v = do
   acAddr <- parseJSON v
   case decode (BSL.fromStrict . Text.encodeUtf8 $ acAddr) of
    Nothing -> fail "Error decoding AccountAddress"
    Just ac -> return ac
  parseJSONList v = do
   acAddr <- parseJSONList v
   let parsed = mapM (decode . BSL.fromStrict . Text.encodeUtf8) acAddr
   case parsed of
    Nothing -> fail "Error decoding AccountAddresses"
    Just ac -> return ac

instance FromJSON Address where
  parseJSON (Object v) = do
    r <- v .:? "accountAddress"
    case r of
      Nothing -> AddressContract <$> (v .: "contractAddress")
      Just a  -> return (AddressAccount a)
  parseJSON invalid = typeMismatch "Address" invalid

instance FromJSON IDTypes.CredentialRegistrationID where
  parseJSON v = do
   crid <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ crid) of
    Nothing -> fail "Error decoding CredentialRegistrationID"
    Just cr -> return cr

instance FromJSON IDTypes.AnonimityRevokerIdentity where
  parseJSON v = do
   arid <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ arid) of
    Nothing -> fail "Error decoding AnonimityRevokerIdentity"
    Just ar -> return ar

instance FromJSON IDTypes.SecretShare where
  parseJSON v = do
   ss <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ ss) of
    Nothing -> fail "Error decoding SecretShare"
    Just s  -> return s

instance FromJSON IDTypes.IdentityProviderIdentity where
  parseJSON v = do
   ipi <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ ipi) of
    Nothing -> fail "Error decoding IdentityProviderIdentity"
    Just ip -> return ip

instance FromJSON IDTypes.ZKProof where
  parseJSON v = do
   zk <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ zk) of
    Nothing -> fail "Error decoding ZKProof"
    Just z  -> return z

-- instance FromJSON IDA.Policy where
--   parseJSON (Object v) = do
--     atomicBD <- v .:? "birthDate"
--     maxAccount <- v .:? "maxAccount"
--     citizenship <- v .:? "citizenships"
--     conj <- v .:? "conj"
--     disj <- v .:? "disj"
--     return . fromJust $ IDA.AtomicBD <$> atomicBD <|>
--       IDA.AtomicMaxAccount <$> maxAccount <|>
--        IDA.AtomicCitizenship <$> citizenship -- <|>
--        IDA.Conj <$> conj <|>
--         IDA.Disj <$> disj
instance FromJSON IDTypes.CredentialDeploymentInformation where
  parseJSON (Object v) = do
    verifKey <- v .: "verificationKey"
    regId <- v .: "registrationId"
    arData <- v .: "revocationData"
    ipId <- v .: "identityProvider"
    let policy = IDA.AtomicMaxAccount (IDA.LessThan 100) -- <- v .: "policy" --hard coded until it gets more structured
    auxData <- fst . BS16.decode . Text.encodeUtf8 <$> v .: "auxData"
    proof <- v .: "proof"
    return $ IDTypes.CDI verifKey Ed25519 regId arData ipId policy auxData proof

instance FromJSON IDTypes.AccountEncryptionKey where
  parseJSON v = do
   aek <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ aek) of
    Nothing -> fail "Error decoding AccountEncryptionKey"
    Just ae -> return ae

instance FromJSON BakerElectionVerifyKey where
  parseJSON v = do
   b16 <- parseJSON v
   case decode (BSL.fromStrict . fst . BS16.decode . Text.encodeUtf8 $ b16) of
    Nothing -> fail "Error decoding VRF PublicKey"
    Just pk -> return pk

-- instance FromJSON BakerSignVerifyKey where
--   parseJSON v = undefined
instance FromJSON Types.Proof where
  parseJSON v = fst . BS16.decode . Text.encodeUtf8 <$> parseJSON v

instance FromJSON BakerId where
  parseJSON v = do
   bid <- parseJSON v
   case decode (BSL.fromStrict . Text.encodeUtf8 $ bid) of
    Nothing -> fail "Error decoding VRF PublicKey"
    Just bi -> return bi

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
  deriving (Generic)

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
