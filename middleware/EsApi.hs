{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module EsApi where

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Text                 (Text)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Aeson                          as AE
import Data.Aeson                (eitherDecode, encode, parseJSONList)
import Data.Aeson.Types          (ToJSON, FromJSON, typeMismatch)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusCode)
import NeatInterpolation
import Servant.API.Generic
import Data.Map
import Control.Monad.IO.Class
import qualified Concordium.Scheduler.Types as Types
import Concordium.Client.Types.Transaction
import Concordium.Types

-- API requests

{-

Uses ElasticSearch Optimistic Concurrency Control:
https://www.elastic.co/guide/en/elasticsearch/reference/current/optimistic-concurrency-control.html

In future we'll need to rearchitect this to also handle transaction failures and
re-syncing chain nonce (i.e in untracked transaction scenarios), but for beta
we're skipping this for now.

-}
takeNextNonceFor :: Text -> IO Types.Nonce
takeNextNonceFor accountAddress = do

  (nonceQueryResponseE :: Either HttpException NonceQueryResponse) <-
    getJsonRequestEither ("http://localhost:9200/index_nonce_log/_doc/" ++ T.unpack accountAddress)

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
          "http://localhost:9200/index_nonce_log/_doc/" ++ (T.unpack accountAddress) ++
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
              takeNextNonceFor accountAddress
        Left err -> do
          case err of
            HttpExceptionRequest _ (StatusCodeException res _) ->
              if statusCode (responseStatus res) == 409 then do
                putStrLn "⚠️ Nonce already taken! Retrying..."
                takeNextNonceFor accountAddress

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
            initializeNonce accountAddress -- @TODO handle failures here
            pure $ Nonce 1
          else do
            putStrLn "❌ Unexpected HttpException:"
            putStrLn $ show err
            -- @TODO handle failures here
            error "Nonce acquisition failed, fatal error."


initializeNonce :: Text -> IO NonceInitializeResponse
initializeNonce address = do
  let
    newNonce = [text|
      { "nonce": 1 }
    |]

    url =
      "http://localhost:9200/index_nonce_log/_doc/" ++ (T.unpack address)

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


getAccountTransactions :: Text -> IO AccountTransactionsResponse
getAccountTransactions accountAddress = do
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

  (outcomes :: OutcomesSearchResponseJson) <-
    -- @TODO paramaterise local port for elasticsearch
    postTextRequest "http://localhost:9200/index_transfer_log/_search" q

  putStrLn $ show outcomes

  pure $ AccountTransactionsResponse (results outcomes) accountAddress


-- API Helpers

getJsonRequest ::  (FromJSON response) => String -> IO response
getJsonRequest url = do
  req <- setHeaders <$> parseUrlThrow url
  jsonRequest req


getJsonRequestEither ::  (FromJSON response) => String -> IO (Either HttpException response)
getJsonRequestEither url = do
  req <- setHeaders <$> parseUrlThrow url
  jsonRequestEither req


postTextRequest :: FromJSON response => String -> Text -> IO response
postTextRequest url request = do
  req <- setRequestBodyLBS (BS.fromStrict $ E.encodeUtf8 request) . setHeaders . setPost <$> parseUrlThrow url
  jsonRequest req


postTextRequestEither :: FromJSON response => String -> Text -> IO (Either HttpException response)
postTextRequestEither url request = do
  req <- setRequestBodyLBS (BS.fromStrict $ E.encodeUtf8 request) . setHeaders . setPost <$> parseUrlThrow url
  jsonRequestEither req


postJsonRequest ::  (ToJSON request, FromJSON response) => String -> request -> IO response
postJsonRequest url requestObject = do
  req <- setRequestBodyJSON requestObject . setHeaders . setPost <$> parseUrlThrow url
  jsonRequest req


jsonRequest :: FromJSON response => Request -> IO response
jsonRequest req = do
  res <- jsonRequestEither req

  case res of
    Left err -> do
      putStrLn "❌ Unhandled Left:"
      putStrLn "Request:"
      putStrLn $ case requestBody req of
        RequestBodyLBS lbs -> BS.unpack lbs
        _ -> "<RequestBody non-LBS>"

      error $ show err

    Right result -> pure result


jsonRequestEither :: FromJSON response => Request -> IO (Either HttpException response)
jsonRequestEither req = do
  manager <- newManager tlsManagerSettings -- @TODO put the tlsManger into reader

  let
    doReq = do
      res <- Network.HTTP.Conduit.httpLbs req manager
      case eitherDecode (responseBody res) of
        Right response -> pure $ Right response
        Left err -> error $ show err

    errorHandler err =
      case err of
        HttpExceptionRequest _ (StatusCodeException res _) ->
          pure $ Left err
        _ -> E.throw err

  res <- E.catch doReq errorHandler

  pure res


textRequest :: Request -> IO Text
textRequest req = do
  manager <- newManager tlsManagerSettings -- @TODO put the tlsManger into reader
  res     <- Network.HTTP.Conduit.httpLbs req manager

  pure $ E.decodeUtf8 $ BS.toStrict $ responseBody res


setHeaders :: Request -> Request
setHeaders h = h { requestHeaders = [("Content-Type", "application/json")] }


setPost :: Request -> Request
setPost h = h { method = "POST" }


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
