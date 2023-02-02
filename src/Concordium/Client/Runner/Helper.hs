{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}

module Concordium.Client.Runner.Helper
  ( outputGRPC
  , outputGRPC'
  , outputGRPCV2
  , outputGRPCV2'
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

-- The GRPC call helper output, either resulting from a streaming or unary call.
-- This is here due to the differing output types of the GRPC helpers `rawUnary` and `rawStreamServer`.
data GRPCOutput a =
      RawUnaryOutput (RawReply a)
    | ServerStreamOutput (a, HeaderList, HeaderList)

-- The complexity comes from the return type of `rawUnary` and `rawStreamServer`. See the documentation
-- http://hackage.haskell.org/package/http2-client-grpc-0.7.0.0/docs/Network-GRPC-Client-Helpers.html
outputGRPCV2' :: GRPCOutput t  -> GRPCResult t
outputGRPCV2' =
    \case
      RawUnaryOutput r ->
        case r of
          Right val -> do
            let (hds, _, response) = val
            case response of
              Left e  -> Left $ "gRPC error: " ++ Network.URI.Encode.decode e
              Right v -> Right (GRPCResponse hds v)
          Left e -> Left $ "Unable to send consensus query: " ++ show e
      ServerStreamOutput (t, hds, _hds) -> do
        let hs = map (\(hn, hv) -> (CI.mk hn, hv)) hds
        Right (GRPCResponse hs t)

-- The complexity comes from the return type of `rawUnary` and `rawStreamServer`. See the documentation
-- http://hackage.haskell.org/package/http2-client-grpc-0.7.0.0/docs/Network-GRPC-Client-Helpers.html
outputGRPCV2 :: Maybe (GRPCOutput t) -> GRPCResult t
outputGRPCV2 ret =
  case ret of
    Nothing -> Left "Cannot connect to GRPC server."
    Just v -> outputGRPCV2' v

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
