{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api.Messages where

import           Data.Text (Text)
import           Data.Aeson
import           GHC.Generics

import           Concordium.Client.Types.Transaction ()
import           Concordium.Crypto.SignatureScheme (KeyPair(..))
import qualified Concordium.ID.Types
import qualified Concordium.Types as Types
import qualified Concordium.Types.Transactions as Types

import           SimpleIdClientApi

data BetaIdProvisionRequest =
  BetaIdProvisionRequest
    { attributes :: [(Text,Text)]
    , accountKeys :: Maybe KeyPair
    }
  deriving (FromJSON, Generic, Show)

-- The BetaIdProvisionResponse is just what the SimpleIdClient returns for Identity Object provisioning
type BetaIdProvisionResponse = IdObjectResponse

data BetaAccountProvisionRequest =
  BetaAccountProvisionRequest
    { ipIdentity :: Int
    , preIdentityObject :: PreIdentityObject
    , privateData :: PrivateData
    , signature :: Text
    , revealedItems :: [Text]
    }
  deriving (ToJSON, FromJSON, Generic, Show)


data BetaAccountProvisionResponse =
  BetaAccountProvisionResponse
    { accountKeys :: KeyPair
    , spio :: Concordium.ID.Types.CredentialDeploymentInformation
    , address :: Text
    }
  deriving (ToJSON, Generic, Show)


newtype BetaGtuDropResponse =
  BetaGtuDropResponse
    { transactionId :: Types.TransactionHash
    }
  deriving (ToJSON, Generic, Show)

data TransferRequest =
  TransferRequest
    { keypair :: KeyPair
    , to :: Types.Address
    , amount :: Types.Amount
    }
  deriving (FromJSON, Generic, Show)

newtype TransferResponse =
  TransferResponse
    { transactionId :: Types.TransactionHash
    }
  deriving (ToJSON, Generic, Show)

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
    , isFinalizing :: Bool
    , isInFinalizingCommittee :: Bool
    , signatureVerifyKey :: Text
    , selectionVerifyKey :: Text
    , timestamp :: Int
    }
  deriving (FromJSON, ToJSON, Generic, Show)


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
  deriving (FromJSON, ToJSON, Generic, Show)


newtype SetNodeStateResponse =
  SetNodeStateResponse
    { success :: Bool }
  deriving (FromJSON, ToJSON, Generic, Show)

newtype ReplayTransactionsRequest = ReplayTransactionsRequest  { adminToken :: Text }
  deriving (FromJSON, ToJSON, Generic, Show)

newtype ReplayTransactionsResponse = ReplayTransactionsResponse { success :: Bool }
  deriving (FromJSON, ToJSON, Generic, Show)
