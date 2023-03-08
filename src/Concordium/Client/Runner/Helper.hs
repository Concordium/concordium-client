{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}

module Concordium.Client.Runner.Helper
  ( toGRPCResult
  , toGRPCResult'
  , printJSON
  , printJSONValues
  , getResponseValue
  , getResponseValueOrDie
  , extractResponseValueOrDie
  , GRPCResult(..)
  , GRPCOutput(..)
  , GRPCResponse(..)
  , GRPCHeaderList
  ) where

import Concordium.Client.Cli (logFatal)

import           Control.Monad.IO.Class
import           Data.Aeson                    hiding (Error)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8    as BSL8
import qualified Data.CaseInsensitive          as CI
import           Network.GRPC.Client           hiding (Invalid)
import           Network.GRPC.HTTP2.Types
import qualified Network.URI.Encode            (decode)
import           Prelude                       hiding (fail)

-- |The response contains headers and a response value.
data GRPCResponse a = GRPCResponse
  { grpcHeaders :: GRPCHeaderList,
    grpcResponseVal :: a
  } deriving (Show, Functor)

-- |Result of running a GRPC request.
data GRPCResult a =
    StatusOk (GRPCResponse a) -- ^ The request was successful with status code 'OK', and a response is available.
  | StatusNotOk (GRPCStatusCode, String) -- ^ The request was successful, with a non-'OK' status code indicating a GRPC error occurred.
  | StatusInvalid -- ^ The request was successful, but an invalid status code was returned.
  | RequestFailed String -- ^ The request failed due to an I/O or HTTP/2 error.
  deriving (Functor)

-- |Headers in GRPC call response.
type GRPCHeaderList = CIHeaderList

-- |GRPC call helper output type, with variants corresponding to the result of a unary or streaming call.
-- This is here due to the differing output types of the GRPC helpers @rawUnary@ and @rawStreamServer@,
-- that we use to invoke the GRPC procedure. For more info, see the documentation at:
-- http://hackage.haskell.org/package/http2-client-grpc-0.7.0.0/docs/Network-GRPC-Client-Helpers.html
data GRPCOutput a =
      -- |The output returned by invoking a GRPC procedure using 'rawUnary'.
      RawUnaryOutput (RawReply a)
      -- |The output returned by invoking a GRPC procedure using 'rawStreamServer'.
      -- The second and third element of the triple represents the response headers,
      -- respectively trailers.
    | ServerStreamOutput (a, HeaderList, HeaderList)

-- |Convert a GRPC helper output to a unified result type.
toGRPCResult' :: GRPCOutput t  -> GRPCResult t
toGRPCResult' =
    \case
      -- @RawUnaryOutput@ models the result of invoking a non-streaming GRPC call.
      -- It wraps a @RawReply@ which either indicates if a problem occurred at the
      -- application layer, whose nature is then available in an @ErrorCode@, or if the
      -- request was successful the result is available in a triple comprimising HTTP/2
      -- response headers, trailers and a GRPC response. The response in turn either
      -- indicates if a non-'OK' status code was returned or if an 'OK' status code was
      -- returned. In the former case, a server response message is available, and in the
      -- latter case the GRPC message payload is available.
      RawUnaryOutput r ->
        case r of
          -- The request was successful.
          Right val -> do
            let (hds, _, response) = val
            case response of
              -- The status code was 'OK'.
              Right v -> StatusOk (GRPCResponse hds v)
              -- Otherwise, we got a non-'OK' status code.
              Left e  -> do
                let statusCode = readTrailers hds
                case statusCode of
                  -- Which is either a valid non-'OK' status code.
                  Right (GRPCStatus c _) -> StatusNotOk (c, "GRPC error: " ++ Network.URI.Encode.decode e)
                  -- Or an invalid status code.
                  Left (InvalidGRPCStatus _) -> StatusInvalid
          -- The request failed.
          Left e -> RequestFailed $ "Unable to send query: " ++ show e
      -- @ServerStreamOutput@ contains a triple consisting of a result,
      -- headers and trailers. The trailers are unused.
      ServerStreamOutput (t, hds, _trs) -> do
        let hs = map (\(hn, hv) -> (CI.mk hn, hv)) hds
        StatusOk (GRPCResponse hs t)

-- |Convert a GRPC helper output to a unified result type.
toGRPCResult :: Maybe (GRPCOutput t) -> GRPCResult t
toGRPCResult ret =
  case ret of
    Nothing -> RequestFailed "Cannot connect to GRPC server."
    Just v -> toGRPCResult' v

printJSON :: MonadIO m => Either String Value -> m ()
printJSON v =
  case v of
    Left err       -> liftIO $ putStrLn err
    Right jsonVals -> printJSONValues jsonVals

printJSONValues :: MonadIO m => Value -> m ()
printJSONValues = liftIO . BSL8.putStrLn . encodePretty

-- |Extract the response value of a @GRPCResult@, if present, and return it
-- under the provided mapping.
-- Returns a @Left@ wrapping an error string describing its nature if the
-- request could not be made, or if the GRPC status code was not 'OK', or a
-- @Right@ wrapping the response value under the provided mapping otherwise.
extractResponseValue :: (a -> b) -> GRPCResult (Either String a) -> Either (Maybe GRPCStatusCode, String) b
extractResponseValue f res =
  case res of
    StatusOk resp ->
      case grpcResponseVal resp of
        Left err -> Left (Just OK, "Unable to convert GRPC response payload: " <> err)
        Right val -> Right $ f val
    StatusNotOk (status, err) -> Left (Just status, "A GRPC error occurred: " <> err)
    StatusInvalid -> Left (Nothing, "A GRPC error occurred: Response contained an invalid return code.")
    RequestFailed err -> Left (Nothing, "The GRPC request failed: " <> err)

-- |Get the response value of a @GRPCResult@, if present.
-- Returns a @Left@ wrapping an error string describing its nature if the
-- request could not be made, or if the GRPC status code was not 'OK', or a
-- @Right@ wrapping the response value otherwise.
getResponseValue :: GRPCResult (Either String a) -> Either (Maybe GRPCStatusCode, String) a
getResponseValue = extractResponseValue id

-- |Extract the response value of a @GRPCResult@, if present, and return it
-- under the provided mapping, or fail printing the cause if the result
-- contains an error.
extractResponseValueOrDie :: (MonadIO m)
  => (a -> b)
  -> GRPCResult (Either String a)
  -> m b
extractResponseValueOrDie f res =
  case extractResponseValue f res of
    Left err -> logFatal [snd err]
    Right v -> return v

-- |Get the response value of a @GRPCResult@ if present, or fail printing the
-- cause if the result contains an error.
getResponseValueOrDie :: (MonadIO m)
  => GRPCResult (Either String a)
  -> m a
getResponseValueOrDie = extractResponseValueOrDie id
