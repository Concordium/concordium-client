{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module EsApi where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.ISO8601

import Data.Aeson as AE
import Data.Text                 (Text)
import Data.Aeson.Types          (ToJSON, FromJSON, typeMismatch)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import NeatInterpolation
import Servant.API.Generic

import qualified Concordium.Crypto.ByteStringHelpers
import qualified Concordium.Scheduler.Types as Types
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
takeNextNonceFor esUrl accountAddress = do

  (nonceQueryResponseE :: Either HttpException NonceQueryResponse) <-
    getJsonRequestEither (T.unpack esUrl ++ "/index_nonce_log/_doc/" ++ T.unpack accountAddress)

  case nonceQueryResponseE of
    Right nonceQueryResponse -> do
      putStrLn $ "✅ got nonceQueryResponse, progressing..."
      let
        nonceBody newNonce =
          [text|
            { "nonce": $newNonce }
          |]

        newNonce = (nonce nonceQueryResponse) + 1

        url =
          T.unpack esUrl ++ "/index_nonce_log/_doc/" ++ (T.unpack accountAddress) ++
          "?if_seq_no=" ++ (show $ _seq_no nonceQueryResponse) ++
          "&if_primary_term=" ++ (show $ _primary_term nonceQueryResponse)

        body (Nonce v) = (nonceBody $ T.pack . show $ v)

      (nonceIncrementResponseE :: Either HttpException NonceIncrementResponse) <- postTextRequestEither url (body newNonce)

      case nonceIncrementResponseE of
        Right nonceIncrementResponse ->
          case nonceIncrementResponse of
            NonceConfirmed -> pure newNonce
            NonceAlreadyTaken -> do
              -- @TODO never fires as decoders not attempted on non-200 - how to fix this?
              putStrLn "⚠️ Nonce already taken! Retrying..."
              takeNextNonceFor esUrl accountAddress
        Left err -> do
          case err of
            HttpExceptionRequest _ (StatusCodeException res _) ->
              if statusCode (responseStatus res) == 409 then do
                putStrLn "⚠️ Nonce already taken! Retrying..."
                takeNextNonceFor esUrl accountAddress

              else do
                putStrLn "❌ Unexpected HttpException:"
                putStrLn $ show err
                -- @TODO handle failures here
                error "Nonce acquisition failed, fatal error."

    Left err -> do
      case err of
        HttpExceptionRequest _ (StatusCodeException res _) ->
          if statusCode (responseStatus res) == 404 then do
            putStrLn $ "✅ No nonce exists, initializing"
            initializeNonce esUrl accountAddress -- @TODO handle failures here
            pure $ Nonce 1
          else do
            putStrLn "❌ Unexpected HttpException:"
            putStrLn $ show err
            -- @TODO handle failures here
            error "Nonce acquisition failed, fatal error."


initializeNonce :: Text -> Text -> IO NonceInitializeResponse
initializeNonce esUrl address = do
  let
    newNonce = [text|
      { "nonce": 1 }
    |]

    url = T.unpack esUrl ++ "/index_nonce_log/_doc/" ++ (T.unpack address)

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
getAccountTransactions esUrl accountAddress = do
  let
    -- @TODO what other transfer types do we care about for now?
    q =
      [text|
        {
          "query": {
            "bool": {
              "must": [
                { "bool": { "should": [
                  { "match": { "to_account": "$accountAddress" } },
                  { "match": { "from_account": "$accountAddress" } }
                ] } },
                { "bool": { "must": [
                  { "match": { "message_type": "DirectTransfer" } }
                ] } }
              ]
            }
          }
        }
      |]

  (outcomes :: Either HttpException OutcomesSearchResponseJson) <-
    postTextRequestEither (T.unpack esUrl ++ "/index_transfer_log/_search") q

  case outcomes of
    Right outcomes ->
      pure $ AccountTransactionsResponse (results outcomes) accountAddress
    Left err -> do
      putStrLn $ "❌ failed to find transactions for " ++ show accountAddress
      putStrLn $ show err
      pure $ AccountTransactionsResponse [] accountAddress



data AccountTransactionsResponse =
  AccountTransactionsResponse
    { transactions :: [TransactionOutcome]
    , accountAddress :: Text
    }
  deriving (ToJSON, Generic, Show)


data OutcomesSearchResponseJson =
  OutcomesSearchResponseJson
    { results :: [TransactionOutcome]
    }
  deriving (Show)


instance AE.FromJSON OutcomesSearchResponseJson where
  parseJSON (Object v) = do
    hits_ <- v .: "hits"
    hits <- hits_ .: "hits"
    results <- mapM (.: "_source") hits

    return $ OutcomesSearchResponseJson results

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


-- Logs base16 encoded transactions to ES for replay on testnet reboots
logBareTransaction :: Text -> Types.BareTransaction -> Types.AccountAddress -> IO ()
logBareTransaction esUrl bareTransaction address = do

  created <- getCurrentTime

  let
    encodeCreated = T.pack $ formatISO8601 created

    encodedTransaction = Concordium.Crypto.ByteStringHelpers.serializeBase16 bareTransaction

    encodedAddress = TL.toStrict $ TL.decodeUtf8 $ AE.encode address

    body =
      [text|
        { "timestamp": "$encodeCreated"
        , "originating_account_address": $encodedAddress
        , "transaction": "$encodedTransaction"
        }
      |]

    url = T.unpack esUrl ++ "/index_transaction_log/_doc"

  putStrLn $ T.unpack body

  (result :: Either HttpException LogBareTransactionResponse) <-
    postTextRequestEither url body

  putStrLn $ show result

  pure ()


data LogBareTransactionResponse =
  LogBareTransactionResponse
  { result :: Text
  }
  deriving (FromJSON, Generic, Show)
