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
  , GRPCResult
  , GRPCOutput(..)
  , GRPCResponse(..)
  , GRPCHeaderList
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as BSL8
import qualified Data.CaseInsensitive          as CI
import qualified Data.ProtoLens.Field          as Field
import           Data.Text                     (Text)
import           Data.Text.Encoding
import           Lens.Micro.Platform
import           Network.GRPC.Client
import qualified Network.URI.Encode            (decode)
import           Prelude                       hiding (fail)
import qualified Proto.ConcordiumP2pRpc_Fields as CF

-- |The response contains headers and a response value.
data GRPCResponse a = GRPCResponse
  { grpcHeaders :: GRPCHeaderList,
    grpcResponseVal :: a
  } deriving (Show, Functor)

-- |Result of running a GRPC request. Either the request fails or there is a response.
type GRPCResult a = Either String (GRPCResponse a)

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
    | ServerStreamOutput (a, HeaderList, HeaderList)

-- |Convert a GRPC helper output to a unified result type.
toGRPCResult' :: GRPCOutput t  -> GRPCResult t
toGRPCResult' =
    \case
      RawUnaryOutput r ->
        case r of
          Right val -> do
            let (hds, _, response) = val
            case response of
              Left e  -> Left $ "gRPC error: " ++ Network.URI.Encode.decode e
              Right v -> Right (GRPCResponse hds v)
          Left e -> Left $ "Unable to send consensus query: " ++ show e
      -- ServerStreamOutput contains a triple consisting of a result,
      -- headers and trailers. The trailers are not used.
      ServerStreamOutput (t, hds, _trs) -> do
        let hs = map (\(hn, hv) -> (CI.mk hn, hv)) hds
        Right (GRPCResponse hs t)

-- |Convert a GRPC helper output to a unified result type.
toGRPCResult :: Maybe (GRPCOutput t) -> GRPCResult t
toGRPCResult ret =
  case ret of
    Nothing -> Left "Cannot connect to GRPC server."
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
    Left e -> Left $ "Unable to send consensus query: " ++ show e

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
