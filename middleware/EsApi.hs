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

{-

Uses ElasticSearch Optimistic Concurrency Control:
https://www.elastic.co/guide/en/elasticsearch/reference/current/optimistic-concurrency-control.html

In future we'll need to rearchitect this to also handle transaction failures and
re-syncing chain nonce (i.e in untracked transaction scenarios), but for beta
we're skipping this for now.

-}
takeNextNonceFor :: Text -> Text -> IO Types.Nonce
takeNextNonceFor esUrl address = do

  let
    unexpected :: HttpException -> IO a
    unexpected err = do
        putStrLn "❌ Unexpected HttpException:"
        print err
        -- @TODO handle failures here
        error "Nonce acquisition failed, fatal error."

  (nonceQueryResponseE :: Either HttpException NonceQueryResponse) <-
    getJsonRequestEither (T.unpack esUrl ++ "/index_nonce_log/_doc/" ++ T.unpack address)

  case nonceQueryResponseE of
    Right nonceQueryResponse -> do
      putStrLn "✅ got nonceQueryResponse, progressing..."
      let
        nonceBody nonceValue =
          [text|
            { "nonce": $nonceValue }
          |]

        newNonce = nonce nonceQueryResponse + 1

        url =
          T.unpack esUrl ++ "/index_nonce_log/_doc/" ++ T.unpack address ++
          "?if_seq_no=" ++ show (_seq_no nonceQueryResponse) ++
          "&if_primary_term=" ++ show (_primary_term nonceQueryResponse)

        body (Nonce v) = nonceBody . T.pack . show $ v

      (nonceIncrementResponseE :: Either HttpException NonceIncrementResponse) <- postTextRequestEither url (body newNonce)

      case nonceIncrementResponseE of
        Right nonceIncrementResponse ->
          case nonceIncrementResponse of
            NonceConfirmed -> pure newNonce
            NonceAlreadyTaken -> do
              -- @TODO never fires as decoders not attempted on non-200 - how to fix this?
              putStrLn "⚠️ Nonce already taken! Retrying..."
              takeNextNonceFor esUrl address
        Left err ->
          case err of
            HttpExceptionRequest _ (StatusCodeException res _) ->
              if statusCode (responseStatus res) == 409 then do
                putStrLn "⚠️ Nonce already taken! Retrying..."
                takeNextNonceFor esUrl address
              else
                unexpected err
            _ -> unexpected err
    Left err ->
      case err of
        HttpExceptionRequest _ (StatusCodeException res _) ->
          if statusCode (responseStatus res) == 404 then do
            putStrLn "✅ No nonce exists, initializing"
            _ <- initializeNonce esUrl address -- @TODO handle failures here
            pure $ Nonce 1
          else
            unexpected err
        _ -> unexpected err


initializeNonce :: Text -> Text -> IO NonceInitializeResponse
initializeNonce esUrl address = do
  let
    newNonce = [text|
      { "nonce": 1 }
    |]

    url = T.unpack esUrl ++ "/index_nonce_log/_doc/" ++ T.unpack address

  postTextRequest url newNonce


data NonceInitializeResponse =
  NonceInitialized

instance AE.FromJSON NonceInitializeResponse where
  parseJSON (Object v) = do
    result :: Text <- v .: "result"
    if result == "created" then
      return NonceInitialized
    else
      fail "NonceInitializeResponse did not see a result=created value"

  parseJSON invalid = typeMismatch "NonceInitializeResponse" invalid


data NonceIncrementResponse =
  NonceConfirmed | NonceAlreadyTaken
  deriving (Show)


instance AE.FromJSON NonceIncrementResponse where
  parseJSON (Object v) = do
    _seq_no :: Int <- v .: "_seq_no"
    _primary_term :: Int <- v .: "_primary_term"

    if _seq_no > 0 && _primary_term > 0 then
      return NonceConfirmed
    else do
      e <- v .: "error"
      tipe :: Text <- e .: "type"

      if tipe == "version_conflict_engine_exception" then
        return NonceAlreadyTaken
      else
        fail "Document did not match either Confirmed or AlreadyTaken expected structures."

  parseJSON invalid = typeMismatch "OutcomesSearchResponseJson" invalid


data NonceQueryResponse =
  NonceQueryResponse
    { _seq_no :: Int
    , _primary_term :: Int
    , nonce :: Types.Nonce
    }
  deriving (Show)


instance AE.FromJSON NonceQueryResponse where
  parseJSON (Object v) = do
    _seq_no <- v .: "_seq_no"
    _primary_term <- v .: "_primary_term"

    _source <- v .: "_source"
    nonce <- _source .: "nonce"

    return $ NonceQueryResponse {..}

  parseJSON invalid = typeMismatch "OutcomesSearchResponseJson" invalid


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
