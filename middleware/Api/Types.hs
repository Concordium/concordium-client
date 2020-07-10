{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Api.Types where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Word
import           Data.Map (Map)
import           GHC.Generics

import           Concordium.Types.Utils
import           Concordium.Client.Types.Account
import           Concordium.Client.Encryption
import           Concordium.Client.Runner(StatusOfPeers)
import           Concordium.Client.Types.Transaction ()
import           Concordium.Crypto.SignatureScheme (KeyPair(..))
import qualified Concordium.ID.Types
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.Types.Transactions as Types

import           SimpleIdClientApi
import           PerAccountTransactions
import Concordium.Types (ElectionDifficulty, BakerId)

data BetaIdProvisionRequest =
  BetaIdProvisionRequest
    { attributes :: Map Text Text
    , accountKeys :: Maybe AccountKeyPair
    }
  deriving (Generic, Show, FromJSON)


data BetaAccountProvisionRequest =
  BetaAccountProvisionRequest
    { ipIdentity :: Int
    , identityObject :: IdentityObject
    , idUseData :: IdUseData
    , revealedAttributes :: Map Text Text
    , accountNumber :: Word8
    }
  deriving (Generic, Show, ToJSON)

instance FromJSON BetaAccountProvisionRequest where
  parseJSON = withObject "BetaAccountProvisionRequest" $ \v -> do
    ipIdentity <- v .: "ipIdentity"
    identityObject <- v .: "identityObject"
    idUseData <- v .: "idUseData"
    revealedAttributes <- v .: "revealedAttributes"
    accountNumber <- v .:? "accountNumber" .!= 0
    return BetaAccountProvisionRequest{..}


-- NOTE: This will be removed.
data BetaAccountProvisionResponse =
  BetaAccountProvisionResponse
    { accountKeys :: AccountKeyMap
    , spio :: Concordium.ID.Types.CredentialDeploymentInformation
    , address :: Text
    , transactionHash :: Types.TransactionHash
    }
  deriving (Generic, Show)

instance ToJSON BetaAccountProvisionResponse

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
  deriving newtype (ToJSON, Show)


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

data TransactionOutcome = TransactionOutcome {
  -- |Account whose perspective this outcome is from.
  toAccount :: !Types.AccountAddress,
  -- |Hash of the block this outcome refers to.
  toBlockHash :: !Types.BlockHash,
  -- |Timestamp in milliseconds.
  toBlockTime :: !Word64,
  -- |Whether the transaction is incoming or outgoing for the given account.
  -- If outgoing then this field is present and contains the fee, if incoming
  -- this field is Nothing.
  toTransactionFee :: !(Maybe Types.Amount),
  -- |Details of the change.
  toDetails :: !OutcomeDetails,
  -- |Status of the transaction.
  toFinalized :: !Bool
  } deriving(Show)

outcomeFromPretty :: PrettyEntry -> TransactionOutcome
outcomeFromPretty PrettyEntry{..} = TransactionOutcome{..}
  where toBlockHash = peBlockHash
        toBlockTime = Types.tsMillis peBlockTime

        toAccount = peAccount

        -- At the moment all transactions are finalized that we get from the database.
        toFinalized = True

        toTransactionFee =
          case peTransactionSummary of
            SpecialTransaction _ -> Nothing
            BlockTransaction summary ->
              if Types.tsSender summary == Just peAccount
              then Just (Types.tsCost summary)
              else Nothing
        prettyType Nothing = "CredentialDeployment"
        prettyType (Just tt) = Text.pack (Prelude.drop 2 (show tt))

        toDetails =
          case peTransactionSummary of
            SpecialTransaction reward -> BakingReward (Types.stoBakerId reward) (Types.stoRewardAmount reward)
            BlockTransaction Types.TransactionSummary{..} ->
              case tsResult of
                Types.TxReject reason ->
                  Rejected{
                    odTransactionType = prettyType tsType,
                    odReason = Text.pack (show reason),
                    odTransactionHash = tsHash
                    }
                Types.TxSuccess _ ->
                  case tsType of
                    Just Types.TTTransfer |
                        Types.TxSuccess [Types.Transferred{etTo = Types.AddressAccount odToAddress,
                                                           etFrom = Types.AddressAccount odFromAddress, ..}] <- tsResult
                            -> SimpleTransfer{odAmount = etAmount, odTransactionHash = tsHash,..}

                    _ -> OtherTransaction{odTransactionType = prettyType tsType, odTransactionHash = tsHash }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . drop 2 } ''TransactionOutcome)

data AccountTransactionsResponse =
  AccountTransactionsResponse
    { transactions :: [TransactionOutcome]
    , accountAddress :: Types.AccountAddress
    }
  deriving (Generic, Show)

instance ToJSON AccountTransactionsResponse

data ImportAccountRequest
  = ImportAccountRequestWeb
    { -- | The unencrypted web-formatted export.
      contents :: Text
      -- | An optional name for the to-be-imported account.
    , alias :: Maybe Text
      -- | The password to encrypt the signing keys with.
      -- It must be UTF-8 encoded.
    , password :: Text
    }
  | ImportAccountRequestMobile
    { -- | The encrypted mobile-formatted export.
      contents :: Text
      -- | The password to decrypt the export and to encrypt the signing keys with.
      -- It must be UTF-8 encoded.
    , password :: Text
    }
  deriving (FromJSON, ToJSON, Generic, Show)


data ImportAccountResponse =
  ImportAccountResponse
  { success :: Maybe String }
  deriving (ToJSON, Generic, Show)

data GetAccountsResponse =
  GetAccountsResponse [GetAccountsResponseItem]
  deriving (ToJSON, Generic, Show)

data GetAccountsResponseItem =
  GetAccountsResponseItem
  { name :: Maybe Text
  , address :: Text }
  deriving (ToJSON, Generic, Show)

data AddBakerRequest =
  AddBakerRequest
  { sender :: Maybe Text
  -- | The password to decrypt the account signing keys.
  -- It must be UTF-8 encoded.
  , password :: Text }
  deriving (FromJSON, Generic, Show)

data AddBakerResponse =
  AddBakerResponse
  { hash :: Maybe Text }
  deriving (ToJSON, Generic, Show)

data RemoveBakerRequest =
  RemoveBakerRequest
  { sender :: Maybe Text
  , bakerId :: Word64
  -- | The password to decrypt the account signing keys.
  -- It must be UTF-8 encoded.
  , password :: Text }
  deriving (FromJSON, Generic, Show)

data RemoveBakerResponse =
  RemoveBakerResponse
  { hash :: Maybe Text }
  deriving (ToJSON, Generic, Show)

data GetBakersResponse =
  GetBakersResponse
  { bakers :: [(BakerId, Text,  ElectionDifficulty)] }
  deriving (ToJSON, Generic, Show)
