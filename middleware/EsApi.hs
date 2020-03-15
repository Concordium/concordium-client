{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module EsApi where

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Clock
import Data.Time.ISO8601

import Data.Aeson as AE
import Data.Aeson.Types          (ToJSON, FromJSON, typeMismatch)
import Data.Text                 (Text)
import NeatInterpolation
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Servant.API.Generic

import Concordium.Client.Types.Transaction ()
import qualified Concordium.Crypto.ByteStringHelpers
import qualified Concordium.Types as Types
import qualified Concordium.Types.Transactions as Types

import Concordium.Types
import Http

-- API requests

getAccountTransactions :: Text -> Text -> IO AccountTransactionsResponse
getAccountTransactions esUrl address = do
  let
    -- @TODO what other transfer types do we care about for now?
    q =
      [text|
        {
          "size": "10000",
          "sort": [
          { "timestamp":
            {"order" : "desc"}
          }
          ],
          "query": {
            "bool": {
              "must": [
                { "bool": { "should": [
                  { "match": { "to_account": "$address" } },
                  { "match": { "from_account": "$address" } }
                ] } },
                { "bool": { "must": [
                  { "match": { "message_type": "DirectTransfer" } }
                ] } }
              ]
            }
          }
        }
      |]

  (firstOutcomes :: Either HttpException OutcomesSearchResponseJson) <-
    postTextRequestEither (T.unpack esUrl ++ "/index_transfer_log/_search?scroll=1m") q

  allOutcomes <- case firstOutcomes of
          Left _ -> return firstOutcomes
          Right trans -> getOutcomes esUrl trans

  case allOutcomes of
    Right outcomes ->
      pure $ AccountTransactionsResponse (results outcomes) address
    Left err -> do
      putStrLn $ "❌ failed to find transactions for " ++ show address
      print err
      pure $ AccountTransactionsResponse [] address
  where
     getOutcomes :: Text -> OutcomesSearchResponseJson -> IO (Either HttpException OutcomesSearchResponseJson)
     getOutcomes url t@(OutcomesSearchResponseJson td scrollId) = do
        let
          q =
            [text|
                 {
                 "scroll": "1m",
                 "scroll_id": "$scrollId"
                 }
                 |]
        (transactions :: Either HttpException OutcomesSearchResponseJson) <-
          postTextRequestEither (T.unpack esUrl ++ "/_search/scroll") q
        case transactions of
          Left _ -> return transactions
          Right (OutcomesSearchResponseJson [] _) -> return $ Right t
          Right (OutcomesSearchResponseJson td2 scrId) -> getOutcomes url $  OutcomesSearchResponseJson (td ++ td2) scrId


data AccountTransactionsResponse =
  AccountTransactionsResponse
    { transactions :: [TransactionOutcome]
    , accountAddress :: Text
    }
  deriving (ToJSON, Generic, Show)


data OutcomesSearchResponseJson =
  OutcomesSearchResponseJson
    {
      results :: [TransactionOutcome]
      , scrollId :: Text
    }
  deriving (Show)


instance AE.FromJSON OutcomesSearchResponseJson where
  parseJSON (Object v) = do
    scrollId <- v .: "_scroll_id"
    results <- (v .: "hits") >>= (.: "hits") >>= mapM (.: "_source")

    return $ OutcomesSearchResponseJson results scrollId

  parseJSON invalid = typeMismatch "OutcomesSearchResponseJson" invalid


data TransactionOutcome =
  TransactionOutcome
    { id :: Text
    , message_type :: Text
    , timestamp :: Text
    , block_hash :: Text
    , slot :: Text
    , transaction_hash :: Text
    , amount :: Text
    , from_account :: Maybe Text
    , to_account :: Maybe Text
    , from_contract :: Maybe Text
    , to_contract :: Maybe Text
    , baker_id :: Maybe Text
    }
  deriving (FromJSON, ToJSON, Generic, Show)

data TransactionsQueryResponse =
  TransactionsQueryResponse
  {
    transactions :: [TransactionData]
  , scrollId :: Text
  }
  deriving (Generic, Show)

instance AE.FromJSON TransactionsQueryResponse where
  parseJSON (Object v) = do
    scrollId <- v .: "_scroll_id"
    results <- (v .: "hits") >>= (.: "hits") >>= mapM (.: "_source")

    return $ TransactionsQueryResponse results scrollId

  parseJSON invalid = typeMismatch "TransactionsQueryResponse" invalid

newtype TransactionData =
  TransactionData
  {
    transaction :: Text
  }
  deriving (Generic, Show)

instance AE.FromJSON TransactionData where
  parseJSON (Object v) = do
    tx <- v .:? "transaction"
    return . TransactionData $ fromMaybe T.empty tx

  parseJSON invalid = typeMismatch "TransactionsQueryResponse" invalid
