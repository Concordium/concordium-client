{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Api.Types where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Word
import           GHC.Generics

import           Concordium.Client.Utils
import           Concordium.Client.Types.Transaction ()
import           Concordium.Crypto.SignatureScheme (KeyPair(..))
import qualified Concordium.ID.Types
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.Types.Transactions as Types
import qualified Concordium.Client.Types.Transaction as Types
import qualified Concordium.ID.Types as IDTypes

import           SimpleIdClientApi
import           PerAccountTransactions

data BetaIdProvisionRequest =
  BetaIdProvisionRequest
    { attributes :: [(Text,Text)]
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
    , revealedItems :: [Text]
    , accountNumber :: Word8
    }
  deriving (ToJSON, Generic, Show)


instance FromJSON BetaAccountProvisionRequest where
  parseJSON = withObject "BetaAccountProvisionRequest" $ \v -> do
    ipIdentity <- v .: "ipIdentity"
    identityObject <- v .: "identityObject"
    idUseData <- v .: "idUseData"
    revealedItems <- v .:? "revealedItems" .!= []
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

data OutcomeDetails =
  BakingReward {
    odBakerId :: !Types.BakerId,
    odAmountt :: !Types.Amount
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
  -- |Hash of the block this outcome refers to.
  toBlockHash :: !Types.BlockHash,
  -- |Unix timestamp (in seconds).
  toBlockTime :: !Types.Timestamp,
  -- |Whether the transaction is incoming or outgoing for the given account.
  -- If outgoing then this field is present and contains the fee, if incoming
  -- this field is Nothing.
  toTransactionFee :: !(Maybe Types.Amount),
  -- |Details of the change.
  toDetails :: !OutcomeDetails,
  -- |Status of the transaction.
  toStatus :: !Text
  } deriving(Show)

outcomeFromPretty :: PrettyEntry -> TransactionOutcome
outcomeFromPretty PrettyEntry{..} = TransactionOutcome{..}
  where toBlockHash = peBlockHash
        toBlockTime = peBlockTime

        -- At the moment all transactions are finalized that we get from the database.
        toStatus = "finalized"

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
  deriving (ToJSON, Generic, Show)

