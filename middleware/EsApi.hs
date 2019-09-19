{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module EsApi where

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
import NeatInterpolation
import Servant.API.Generic
import Data.Map
import Control.Monad.IO.Class


-- API requests

commitWithNextNonce :: Text -> IO Int
commitWithNextNonce address =
  pure 1




getNonceFor :: Text -> IO Int
getNonceFor accountAddress = do

  (nonceQueryResponse :: NonceQueryResponse) <-
    getJsonRequest ("http://localhost:9200/index_nonce_log/account/" ++ T.unpack accountAddress)

  putStrLn $ show nonceQueryResponse

  let
    -- @TODO what other transfer types do we care about for now?

    nonceBody newNonce =
      [text|{ "nonce": $newNonce }|]

    newNonce = (nonce nonceQueryResponse) + 1

    url =
      "http://localhost:9200/index_nonce_log/_doc/" ++ (T.unpack accountAddress) ++
      "?if_seq_no=" ++ (show $ _seq_no nonceQueryResponse - 1) ++
      "&if_primary_term=" ++ (show $ _primary_term nonceQueryResponse)

    body = (nonceBody $ T.pack . show $ newNonce)

  putStrLn url
  putStrLn $ show body

  (nonceIncrementResponse :: NonceIncrementResponse) <-
    postTextRequest
      url
      body

  putStrLn $ show nonceIncrementResponse

  pure newNonce



-- http://localhost:9200/index_nonce_log/account/4q2FFMLFPWDr1e7wUCLkXpXgMKz5?if_seq_no=11&if_primary_term=1
-- http://localhost:9200/index_nonce_log/account/4q2FFMLFPWDr1e7wUCLkXpXgMKz5?if_seq_no=12&if_primary_term=1

  -- {
  --     "_index": "index_nonce_log",
  --     "_type": "account",
  --     "_id": "4q2FFMLFPWDr1e7wUCLkXpXgMKz5",
  --     "_version": 11,
  --     "result": "updated",
  --     "_shards": {
  --         "total": 2,
  --         "successful": 1,
  --         "failed": 0
  --     },
  --     "_seq_no": 10,
  --     "_primary_term": 1
  -- }
  --
  -- {
  --   "error": {
  --       "root_cause": [
  --           {
  --               "type": "version_conflict_engine_exception",
  --               "reason": "[4q2FFMLFPWDr1e7wUCLkXpXgMKz5]: version conflict, required seqNo [9], primary term [1]. current document has seqNo [10] and primary term [1]",
  --               "index_uuid": "-5XtMieBTYO71rawLpqfDg",
  --               "shard": "0",
  --               "index": "index_nonce_log"
  --           }
  --       ],
  --       "type": "version_conflict_engine_exception",
  --       "reason": "[4q2FFMLFPWDr1e7wUCLkXpXgMKz5]: version conflict, required seqNo [9], primary term [1]. current document has seqNo [10] and primary term [1]",
  --       "index_uuid": "-5XtMieBTYO71rawLpqfDg",
  --       "shard": "0",
  --       "index": "index_nonce_log"
  --   },
  --   "status": 409
  -- }


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
    , nonce :: Int
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


postTextRequest :: FromJSON response => String -> Text -> IO response
postTextRequest url request = do
  req <- setRequestBodyLBS (BS.fromStrict $ E.encodeUtf8 request) . setHeaders . setPost <$> parseUrlThrow url
  jsonRequest req


postJsonRequest ::  (ToJSON request, FromJSON response) => String -> request -> IO response
postJsonRequest url requestObject = do
  req <- setRequestBodyJSON requestObject . setHeaders . setPost <$> parseUrlThrow url
  jsonRequest req


jsonRequest :: FromJSON response => Request -> IO response
jsonRequest req = do
  manager <- newManager tlsManagerSettings -- @TODO put the tlsManger into reader
  res     <- Network.HTTP.Conduit.httpLbs req manager

  case eitherDecode (responseBody res) of
    Left  err -> do
      putStrLn "There was an error decoding this response:"
      putStrLn $ show $ responseBody res
      error err
    Right result -> pure result


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
