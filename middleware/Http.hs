{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Http where

import qualified Control.Exception as E
import qualified Data.Text.Encoding as E
import Data.Text                 (Text)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson                (eitherDecode)
import Data.Aeson.Types          (ToJSON, FromJSON)
import Network.HTTP.Conduit
import Network.HTTP.Simple


-- HTTP helpers

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
        HttpExceptionRequest _ (StatusCodeException _ _) ->
          pure $ Left err
        _ -> E.throw err

  E.catch doReq errorHandler


textRequest :: Request -> IO Text
textRequest req = do
  manager <- newManager tlsManagerSettings -- @TODO put the tlsManger into reader
  res     <- Network.HTTP.Conduit.httpLbs req manager

  pure $ E.decodeUtf8 $ BS.toStrict $ responseBody res


setHeaders :: Request -> Request
setHeaders h = h { requestHeaders = [("Content-Type", "application/json")] }


setPost :: Request -> Request
setPost h = h { method = "POST" }
