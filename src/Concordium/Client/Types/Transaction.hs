{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Concordium.Client.Types.Transaction where

import           Concordium.Crypto.SignatureScheme   (KeyPair(..))
import           Concordium.Crypto.Proofs
import qualified Concordium.ID.Types                 as IDTypes
import           Concordium.Types
import           Concordium.Types.Execution          as Types
import           Data.Aeson                          as AE
import qualified Data.Aeson.TH                       as AETH
import qualified Data.ByteString.Base16              as BS16
import qualified Data.HashMap.Strict                 as Map
import           Data.Text                           hiding (length, map)
import qualified Data.Text.Encoding                  as Text
import           GHC.Generics                        (Generic)

-- |Cost of checking a header where the sender provides n signatures.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
checkHeaderEnergyCost :: Int -> Energy
checkHeaderEnergyCost = fromIntegral . (+6) . (*53)

-- |Cost of a simple transfer transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
simpleTransferEnergyCost :: Int -> Energy
simpleTransferEnergyCost = checkHeaderEnergyCost

-- |Cost of a stake delegation transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountDelegateEnergyCost :: Int -> Int -> Energy
accountDelegateEnergyCost instanceCount = (+100) . (+50*c) . checkHeaderEnergyCost
  where c = fromIntegral instanceCount

-- |Cost of a stake undelegation transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
accountUndelegateEnergyCost :: Int -> Int -> Energy
accountUndelegateEnergyCost = accountDelegateEnergyCost

-- |Cost of a baker add transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
-- The (+2) is added due to the size of the transaction.
-- TODO Compute (+2) the correct way to take into account that it depends
--      on the number of signatures.
bakerAddEnergyCost :: Int -> Energy
bakerAddEnergyCost = (+2) . (+3000) . checkHeaderEnergyCost

-- |Cost of a baker set account transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
-- The (+1) is added due to the size of the transaction.
-- TODO Compute (+1) the correct way to take into account that it depends
--      on the number of signatures.
bakerSetAccountEnergyCost :: Int -> Energy
bakerSetAccountEnergyCost = (+1) . bakerSetKeyEnergyCost

-- |Cost of a baker set account transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerSetKeyEnergyCost :: Int -> Energy
bakerSetKeyEnergyCost = (+ 90) . checkHeaderEnergyCost

-- |Cost of a baker set aggregation key transaction
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerSetAggregationKeyEnergyCost :: Int -> Energy
bakerSetAggregationKeyEnergyCost = (+ 2700) . checkHeaderEnergyCost

-- |Cost of a baker remove transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
bakerRemoveEnergyCost :: Int -> Energy
bakerRemoveEnergyCost = checkHeaderEnergyCost

-- |Cost of a "set election difficulty" transaction.
-- This must be kept in sync with the cost in Concordium.Scheduler.Cost
setElectionDifficultyEnergyCost :: Int -> Energy
setElectionDifficultyEnergyCost = checkHeaderEnergyCost

-- Data (serializes with `putByteString :: Bytestring -> Put`)
instance FromJSON Types.Proof where
  parseJSON v = fst . BS16.decode . Text.encodeUtf8 <$> parseJSON v

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
  | RemoveBaker
      { removeId :: BakerId
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
  | UpdateElectionDifficulty
      { difficulty :: !ElectionDifficulty
      }
  deriving (Show, Generic)

AETH.deriveFromJSON
  (AETH.defaultOptions
     {AETH.sumEncoding = AETH.TaggedObject "transactionType" "contents"})
  ''TransactionJSONPayload

type KeyMap = Map.HashMap IDTypes.KeyIndex KeyPair

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
