{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.Client.TransactionStatus where

import Data.Aeson
import Data.Aeson.TH
import Control.Monad.IO.Class
import qualified Data.Text as Text

import Concordium.Types.Execution
import qualified Concordium.ID.Types as IDTypes
import Concordium.Types
import Concordium.Types.Utils

import Concordium.Client.Types.TransactionStatus
import Concordium.Client.GRPC

data FailedTransferReason = InvalidTargetAccount
                          | NonExistentAmount
                          | MalformedTransaction
                          | InsufficientEnergy
                          deriving(Eq,Show)

$(deriveJSON defaultOptions{constructorTagModifier=firstLower} ''FailedTransferReason)

data SimpleTransactionResult = TransferSuccess { to :: !AccountAddress, amount :: !Amount }
                             | NewAccount { accountAddress :: !AccountAddress }
                             | NewCredential { credentialId :: !IDTypes.CredentialRegistrationID,
                                               onAccount :: !AccountAddress
                                             }
                             | FailedTransfer {
                                 reason :: !FailedTransferReason
                                 }

$(deriveJSON defaultOptions{sumEncoding=TaggedObject{
                               tagFieldName = "outcome",
                               contentsFieldName = "details"},
                             constructorTagModifier = firstLower
                           }
   ''SimpleTransactionResult
  )

type SimpleTransactionSummary = TransactionSummary' SimpleTransactionResult

type SimpleTransactionStatusResult = TransactionStatusResult' SimpleTransactionResult

-- |Get a simplified transaction status of a transaction which is
-- either a simple transfer from an account to an account, or
-- a credential deployment which either deploys a new credential onto
-- an existing account, or 
getSimpleTransactionStatus :: MonadIO m => TransactionHash -> ClientMonad m (Either String SimpleTransactionStatusResult)
getSimpleTransactionStatus hash = do
  eitherStatus <- getTransactionStatus (Text.pack (show hash))
  return $ do
    status <- eitherStatus
    case fromJSON status of
      Error err -> Left err
      Success TransactionStatusResult{..} -> do
        newMap <- mapM processSummary tsrResults
        return TransactionStatusResult{tsrResults = newMap,..}

      where processSummary Nothing = return Nothing
            processSummary (Just TransactionSummary{..}) = Just <$> 
              case tsType of
                Nothing -> -- credential deployment, only two possible outcomes
                  case tsResult of
                    TxSuccess [AccountCreated addr, _] ->
                      return TransactionSummary{tsResult = NewAccount addr,..}
                    TxSuccess [CredentialDeployed regId addr] ->
                      return TransactionSummary{tsResult = NewCredential regId addr,..}
                    es ->
                      Left $ "Unexpected outcome of credential deployment: " ++ show es
                Just TTTransfer -> -- simple transfer. This might be rejected.
                  case tsResult of
                    TxSuccess [Transferred{etTo = AddressAccount addr,..}] ->
                      return TransactionSummary{tsResult = TransferSuccess{to = addr, amount = etAmount},..}
                    TxReject (InvalidAccountReference _) ->
                      return TransactionSummary{tsResult = FailedTransfer InvalidTargetAccount,..}
                    TxReject (ReceiverAccountNoCredential _) ->
                      return TransactionSummary{tsResult = FailedTransfer InvalidTargetAccount,..}
                    TxReject (AmountTooLarge _ _) ->
                      return TransactionSummary{tsResult = FailedTransfer NonExistentAmount,..}
                    TxReject OutOfEnergy ->
                      return TransactionSummary{tsResult = FailedTransfer InsufficientEnergy,..}
                    TxReject SerializationFailure ->
                      return TransactionSummary{tsResult = FailedTransfer MalformedTransaction,..}
                    es ->
                      Left $ "Unexpected outcome of simple transfer: " ++ show es
                _ ->
                  Left "Unsupported transaction type for simple statuses."
