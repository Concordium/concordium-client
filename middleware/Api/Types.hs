{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Api.Types where

import           Data.Text (Text)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Word
import           GHC.Generics

import           Concordium.Client.Runner(StatusOfPeers)
import           Concordium.Client.Types.Account
import           Concordium.Client.Types.Transaction ()
import qualified Concordium.Types as Types
import           Concordium.Utils

import Concordium.Types (BakerId)

data AccountInfoResponse = AccountInfoResponse
  { accountAmount :: !Types.Amount
  , accountNonce :: !Types.Nonce
  }
  deriving (Show)


instance FromJSON AccountInfoResponse where
  parseJSON = withObject "Account info" $ \v -> do
    accountAmount <- v .: "accountAmount"
    accountNonce <- v .: "accountNonce"
    return $ AccountInfoResponse {..}

data AccountWithKeys =
  AccountWithKeys
    { address :: Types.AccountAddress
    , keys :: AccountKeyMap
    }
  deriving (FromJSON, Generic, Show)

accountToPair :: AccountWithKeys -> (Types.AccountAddress, AccountKeyMap)
accountToPair (AccountWithKeys address keys) =
  (address, keys)

data TransferRequest =
  TransferRequest
    { account :: AccountWithKeys
    , to :: Types.Address
    , amount :: Types.Amount
    }
  deriving (Generic, Show)

instance FromJSON TransferRequest

newtype TransferResponse =
  TransferResponse
    { transactionId :: Types.TransactionHash
    }
  deriving newtype (ToJSON, Show)


data GetNodeStateResponse =
  GetNodeStateResponse
    { name :: Text
    , id :: Text
    , version :: Text
    , running :: Bool
    , runningSince :: Int
    , sent :: Int
    , received :: Int
    , isBaking :: Bool
    , isInBakingCommittee :: Bool
    , bakerId :: Maybe Word64
    , isFinalizing :: Bool
    , isInFinalizingCommittee :: Bool
    , signatureVerifyKey :: Text
    , selectionVerifyKey :: Text
    , timestamp :: Int
    , peersStatus :: StatusOfPeers
    }
  deriving (Generic, Show)

instance FromJSON GetNodeStateResponse
instance ToJSON GetNodeStateResponse

data SetNodeStateRequest =
  SetNodeStateRequest
    { name :: Maybe Text
    , id :: Maybe Text
    , version :: Maybe Text
    , running :: Maybe Bool
    , runningSince :: Maybe Int
    , sent :: Maybe Int
    , received :: Maybe Int
    , isBaking :: Maybe Bool
    , isInBakingCommittee :: Maybe Bool
    , isFinalizing :: Maybe Bool
    , isInFinalizingCommittee :: Maybe Bool
    }
  deriving (Generic, Show)

instance FromJSON SetNodeStateRequest
instance ToJSON SetNodeStateRequest

data SetNodeStateResponse =
  SetNodeStateResponse
    { success :: Bool }
  deriving (Generic, FromJSON, ToJSON, Show)

data OutcomeDetails =
  BakingReward {
    odBakerId :: !Types.BakerId,
    odAmount :: !Types.Amount
    }
  | Rejected {
      odTransactionType :: !Text,
      odReason :: !Text,
      odTransactionHash :: !Types.TransactionHash
      }
  | SimpleTransfer {
      odAmount :: !Types.Amount,
      odFromAddress :: !Types.AccountAddress,
      odToAddress :: !Types.AccountAddress,
      odTransactionHash :: !Types.TransactionHash
      }
  | OtherTransaction {
      odTransactionType :: !Text,
      odTransactionHash :: !Types.TransactionHash
      }deriving(Show)


$(deriveJSON defaultOptions{sumEncoding=TaggedObject{
                               tagFieldName = "detailsKind",
                               contentsFieldName = "details"},
                            fieldLabelModifier = firstLower . drop 2
                           }
   ''OutcomeDetails
  )

data RemoveBakerRequest =
  RemoveBakerRequest
  { sender :: Maybe Text
  -- | The password to decrypt the account signing keys.
  -- It must be UTF-8 encoded.
  , password :: Text }
  deriving (FromJSON, Generic, Show)

data RemoveBakerResponse =
  RemoveBakerResponse
  { hash :: Maybe Text }
  deriving (ToJSON, Generic, Show)

data Baker = Baker {
  bakerId :: BakerId,
  account :: Text,
  lotteryPower :: Double
} deriving (ToJSON, Generic, Show)

data GetBakersResponse =
  GetBakersResponse
  { bakers :: [Baker] }
  deriving (ToJSON, Generic, Show)
