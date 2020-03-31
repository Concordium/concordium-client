{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api.Types where

import           Data.Text (Text)
import           Data.Aeson
import           Data.Word
import           Data.Map (Map)
import           GHC.Generics

import           Concordium.Client.Types.Transaction ()
import           Concordium.Crypto.SignatureScheme (KeyPair(..))
import qualified Concordium.ID.Types
import qualified Concordium.Types as Types
import qualified Concordium.Types.Transactions as Types
import qualified Concordium.Client.Types.Transaction as Types
import qualified Concordium.ID.Types as IDTypes

import           SimpleIdClientApi


data BetaIdProvisionRequest =
  BetaIdProvisionRequest
    { attributes :: Map Text Text
    , accountKeys :: Maybe KeyPair
    }
  deriving (FromJSON, Generic, Show)


-- The BetaIdProvisionResponse is the same as what the SimpleIdClient returns for Identity Object provisioning
data BetaIdProvisionResponse =
  BetaIdProvisionResponse
    { value :: Value
    }
  deriving (FromJSON, ToJSON, Generic, Show)


data BetaAccountProvisionRequest =
  BetaAccountProvisionRequest
    { ipIdentity :: Int
    , identityObject :: IdentityObject
    , idUseData :: IdUseData
    , revealedAttributes :: Map Text Text
    , accountNumber :: Word8
    }
  deriving (ToJSON, Generic, Show)


instance FromJSON BetaAccountProvisionRequest where
  parseJSON = withObject "BetaAccountProvisionRequest" $ \v -> do
    ipIdentity <- v .: "ipIdentity"
    identityObject <- v .: "identityObject"
    idUseData <- v .: "idUseData"
    revealedAttributes <- v .: "revealedAttributes"
    accountNumber <- v .:? "accountNumber" .!= 0
    return BetaAccountProvisionRequest{..}


data BetaAccountProvisionResponse =
  BetaAccountProvisionResponse
    { accountKeys :: Types.KeyMap
    , spio :: Concordium.ID.Types.CredentialDeploymentInformation
    , address :: Text
    , transactionHash :: Types.TransactionHash
    }
  deriving (ToJSON, Generic, Show)


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


newtype TestnetGtuDropResponse =
  TestnetGtuDropResponse
    { transactionId :: Types.TransactionHash
    }
  deriving (ToJSON, Generic, Show)


data TransferRequest =
  TransferRequest
    { account :: (IDTypes.AccountAddress, Types.KeyMap)
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


data AccountTransactionsResponse =
  AccountTransactionsResponse
    { transactions :: [TransactionOutcome]
    , accountAddress :: Types.AccountAddress
    }
  deriving (ToJSON, Generic, Show)


data TransactionOutcome =
  TransactionOutcome
    { id :: Text
    , message_type :: Text
    , timestamp :: Text
    , block_hash :: Text
    , transaction_hash :: Text
    , amount :: Text
    , cost :: Text
    , result :: Maybe Text
    , from_account :: Maybe Types.AccountAddress
    , to_account :: Maybe Types.AccountAddress
    , from_contract :: Maybe Types.ContractAddress
    , to_contract :: Maybe Types.ContractAddress
    , finalized :: Bool
    }
  deriving (FromJSON, ToJSON, Generic, Show)
