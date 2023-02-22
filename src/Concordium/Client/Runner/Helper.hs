{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}

module Concordium.Client.Runner.Helper
  ( outputGRPC
  , outputGRPC'
  , toGRPCResult
  , toGRPCResult'
  , printJSON
  , printJSONValues
  , getJSON
  , getValue
  , getResponseValue
  , getResponseValueOrDie
  , extractResponseValueOrDie
  , GRPCResult
  , GRPCResultV2(..)
  , GRPCOutput(..)
  , GRPCResponse(..)
  , GRPCHeaderList
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson                    hiding (Error)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as BSL8
import qualified Data.CaseInsensitive          as CI
import qualified Data.ProtoLens.Field          as Field
import           Data.Text                     (Text)
import           Data.Text.Encoding
import           Lens.Micro.Platform
import           Network.GRPC.Client           hiding (Invalid)
import           Network.GRPC.HTTP2.Types
import qualified Network.URI.Encode            (decode)
import           Prelude                       hiding (fail)
import qualified Proto.ConcordiumP2pRpc_Fields as CF
import Concordium.Client.Cli (logFatal)

-- |The response contains headers and a response value.
data GRPCResponse a = GRPCResponse
  { grpcHeaders :: GRPCHeaderList,
    grpcResponseVal :: a
  } deriving (Show, Functor)

-- |Result of running a GRPC request. Either the request fails or there is a response.
type GRPCResult a = Either String (GRPCResponse a)

-- |Result of running a GRPC request. Either the request succeeded
-- with an OK GRPC status code and a result, a non-OK status code and
-- an error message string, or with an invalid error code. Otherwise
-- the request failed.
data GRPCResultV2 a =
    StatusOk (GRPCResponse a) -- ^ The request was made and a response is available.
  | StatusNotOk (GRPCStatusCode, String) -- ^ The request was made, but a status code indicating error is present in the response.
  | StatusInvalid -- ^ The request was made, but an invalid status code was present in the response.
  | RequestFailed String -- ^ The request failed.
  deriving (Functor)

-- |Headers in GRPC call response.
type GRPCHeaderList = CIHeaderList

-- |GRPC call helper output type, with variants corresponding to the result of a unary or streaming call.
-- This is here due to the differing output types of the GRPC helpers `rawUnary` and `rawStreamServer`,
-- that we use to invoke the GRPC procedure. For more info, see the documentation at:
-- http://hackage.haskell.org/package/http2-client-grpc-0.7.0.0/docs/Network-GRPC-Client-Helpers.html
data GRPCOutput a =
      -- |The output returned by invoking a GRPC procedure using `rawUnary`.
      RawUnaryOutput (RawReply a)
      -- |The output returned by invoking a GRPC procedure using `rawStreamServer`.
      -- The second and third element of the triple represents the response headers,
      -- respectively trailers.
    | ServerStreamOutput (a, HeaderList, HeaderList)

-- |Convert a GRPC helper output to a unified result type.
toGRPCResult' :: GRPCOutput t  -> GRPCResultV2 t
toGRPCResult' =
    \case
      RawUnaryOutput r ->
        case r of
          Right val -> do
            let (hds, _, response) = val
            case response of
              -- The status code was OK.
              Right v -> StatusOk (GRPCResponse hds v)
              -- Otherwise, we got a non-OK status code.
              Left e  -> do
                let statusCode = readTrailers hds
                case statusCode of
                  -- Valid non-OK status code.
                  Right (GRPCStatus c _) -> StatusNotOk (c, "gRPC error: " ++ Network.URI.Encode.decode e ++ "\n" ++ show hds)
                  -- Invalid status code.
                  Left (InvalidGRPCStatus _) -> StatusInvalid
          Left e -> RequestFailed $ "Unable to send query: " ++ show e
      -- ServerStreamOutput contains a triple consisting of a result,
      -- headers and trailers. The trailers are unused.
      ServerStreamOutput (t, hds, _trs) -> do
        let hs = map (\(hn, hv) -> (CI.mk hn, hv)) hds
        StatusOk (GRPCResponse hs t)

-- |Convert a GRPC helper output to a unified result type.
toGRPCResult :: Maybe (GRPCOutput t) -> GRPCResultV2 t
toGRPCResult ret =
  case ret of
    Nothing -> RequestFailed "Cannot connect to GRPC server."
    Just v -> toGRPCResult' v

-- The complexity of the first parameter comes from the return type of
-- rawUnary. See the documentation
-- http://hackage.haskell.org/package/http2-client-grpc-0.7.0.0/docs/Network-GRPC-Client-Helpers.html#v:rawUnary
outputGRPC ::
     (Show a1)
  => Maybe (Either a1 (GRPCHeaderList, b, Either String t))
  -> GRPCResult t
outputGRPC ret =
  case ret of
    Nothing -> Left "Cannot connect to GRPC server."
    Just v -> outputGRPC' v

-- The complexity of the first parameter comes from the return type of
-- rawUnary. See the documentation
-- http://hackage.haskell.org/package/http2-client-grpc-0.7.0.0/docs/Network-GRPC-Client-Helpers.html#v:rawUnary
outputGRPC' ::
     (Show a1)
  => Either a1 (GRPCHeaderList, b, Either String t)
  -> GRPCResult t
outputGRPC' ret =
  case ret of
    Right val -> do
      let (hds, _, response) = val
      case response of
        Left e  ->
          Left $ "gRPC error: " ++ Network.URI.Encode.decode e
        Right v -> Right (GRPCResponse hds v)
    Left e -> Left $ "Unable to send query: " ++ show e

-- |Decode JSON from response. Assumes that the response from a GRPC call has a @value@ field containing the JSON.
getJSON :: (Field.HasField a "value" Text) => SimpleGetter (GRPCResponse a) (GRPCResponse Value)
getJSON  = to (fmap (value . encodeUtf8 <$> (^. CF.value)))

printJSON :: MonadIO m => Either String Value -> m ()
printJSON v =
  case v of
    Left err       -> liftIO $ putStrLn err
    Right jsonVals -> printJSONValues jsonVals

printJSONValues :: MonadIO m => Value -> m ()
printJSONValues = liftIO . BSL8.putStrLn . encodePretty

-- |Decode a JSON string
value :: BS.ByteString -> Value
value s =
  case eitherDecodeStrict' s of
    Right v -> v
    Left err -> error ("Error in gRPC output decoding as a json: " ++ err)

-- |Extract a value from the response. This assumes that the response from a GRPC call has a @value@ field.
getValue :: forall a b. (Field.HasField a "value" b) => SimpleGetter (GRPCResponse a) (GRPCResponse b)
getValue = to (fmap (^. CF.value))

-- |Extract the response value of a GRPCResult and return it under the
-- provided mapping.
-- Returns @Left@ wrapping a error string describing its nature if the
-- request could not be made or if the GRPC status code was not OK, or a
-- @Right@ wrapping the response value under the provided mapping otherwise.
-- VH/FIXME: Change @Either String a@ to @FromProtoResult a @ after re-
-- factoring.
extractResponseValue :: (a -> b) -> GRPCResultV2 (Either String a) -> Either (Maybe GRPCStatusCode, String) b
extractResponseValue f res =
  case res of
    StatusOk resp ->
      case grpcResponseVal resp of
        Left err -> Left (Nothing, "Unable to convert GRPC response payload: " <> err)
        Right val -> Right $ f val
    StatusNotOk (status, err) -> Left (Just status, "A GRPC error occurred: " <> err)
    StatusInvalid -> Left (Nothing, "A GRPC error occurred: Response contained an invalid return code.")
    RequestFailed err -> Left (Nothing, "The GRPC request failed: " <> err)

-- |Get the response value of a GRPCResult or fail if the result
-- contains an error.
getResponseValue :: GRPCResultV2 (Either String a) -> Either (Maybe GRPCStatusCode, String) a
getResponseValue = extractResponseValue id

-- |Extract the response value of a `GRPCResult` and return it under
-- the provided mapping or fail printing the error if the result
-- contains an error.
extractResponseValueOrDie :: (MonadIO m)
  => (a -> b)
  -> GRPCResultV2 (Either String a)
  -> m b
extractResponseValueOrDie f res =
  case extractResponseValue f res of
    Left err -> logFatal [snd err]
    Right v -> return v

-- |Get the response value of a GRPCResult or fail if the result
-- contains an error.
getResponseValueOrDie :: (MonadIO m)
  => GRPCResultV2 (Either String a)
  -> m a
getResponseValueOrDie = extractResponseValueOrDie id