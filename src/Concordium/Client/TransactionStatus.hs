module Concordium.Client.TransactionStatus where

import Data.Aeson
import Data.Aeson.Types (parseEither, Pair)
import Control.Monad.IO.Class
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HM

import Concordium.Types.Execution
import Concordium.Types

import Concordium.Client.GRPC

getSimpleTransactionStatus :: MonadIO m => TransactionHash -> ClientMonad m (Either String Value)
getSimpleTransactionStatus trHash = do
    eitherStatus <- getTransactionStatus (Text.pack $ show trHash)
    return $
      eitherStatus >>= \case
        Null -> return $ object ["status" .= String "absent"]
        Object o ->
          parseEither (.: "status") o >>= \case
            "received" -> return $ object ["status" .= String "received"]
            "finalized" -> HM.toList <$> parseEither (.: "outcomes") o >>= \case
              [(bh,outcome)] -> do
                fields <- outcomeToPairs outcome
                return $ object $ ["status" .= String "finalized", "blockHashes" .= [bh :: BlockHash]] <> fields
              _ -> Left "expected exactly one outcome for a finalized transaction"
            "committed" -> do
              outcomes <- HM.toList <$> parseEither (.: "outcomes") o
              fields <- outcomesToPairs (snd <$> outcomes)
              return $ object $ ["status" .= String "committed", "blockHashes" .= (fst <$> outcomes :: [BlockHash])] <> fields
            s -> Left ("unexpected \"status\": " <> s)
        _ -> Left "expected null or object"
      -- TransactionStatusResult{..} <- eitherStatus >>= parseEither parseJSON :: Either String TransactionStatusResult
  where
    outcomeToPairs :: TransactionSummary -> Either String [Pair]
    outcomeToPairs TransactionSummary{..} =
      (["transactionHash" .= tsHash
      , "sender" .= tsSender
      , "cost" .= tsCost] <>) <$>
      case tsType of
        TSTCredentialDeploymentTransaction _ -> -- credential deployment
          case tsResult of
            TxSuccess [AccountCreated {}, _] ->
              return ["outcome" .= String "newAccount"]
            TxSuccess [CredentialDeployed {}] ->
              return ["outcome" .= String "newCredential"]
            es ->
              Left $ "Unexpected outcome of credential deployment: " ++ show es
        TSTAccountTransaction (Just TTTransfer) ->
          case tsResult of
            TxSuccess [Transferred{etTo = AddressAccount addr,..}] ->
              return ["outcome" .= String "transferSuccess", "to" .= addr, "amount" .= etAmount]
            TxReject (InvalidAccountReference _) ->
              return ["outcome" .= String "invalidTargetAccount"]
            TxReject (ReceiverAccountNoCredential _) ->
              return ["outcome" .= String "invalidTargetAccount"]
            TxReject (AmountTooLarge _ _) ->
              return ["outcome" .= String "nonExistentAmount"]
            TxReject OutOfEnergy ->
              return ["outcome" .= String "insufficientEnergy"]
            TxReject SerializationFailure ->
              return ["outcome" .= String "malformedTransaction"]
            es ->
              Left $ "Unexpected outcome of simple transfer: " ++ show es
        _ ->
          Left "Unsupported transaction type for simple statuses."
    outcomesToPairs :: [TransactionSummary] -> Either String [Pair]
    outcomesToPairs l = do
      outcomes <- mapM outcomeToPairs l
      case outcomes of
        [] -> Left "Expected at least one transaction outcome for a committed transaction"
        [o] -> return o
        (h:r)
          | all (h==) r -> return h
          | otherwise -> return ["outcome" .= String "ambiguous"]
