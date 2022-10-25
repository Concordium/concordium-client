{-# LANGUAGE DataKinds        #-}
module Concordium.Client.Runner.Helper
  ( outputGRPC
  , outputGRPC'
  , printJSON
  , printJSON'
  , printJSONValues
  , processJSON
  , processJSON'
  , value'
  ) where

import Concordium.Client.Types.GRPC
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as BSL8
import qualified Data.ProtoLens.Field          as Field
import           Data.Text                     (Text)
import           Data.Text.Encoding
import           Lens.Micro.Platform
import qualified Network.URI.Encode            (decode)
import           Prelude                       hiding (fail)
import qualified Proto.ConcordiumP2pRpc_Fields as CF

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

processJSON :: (Field.HasField a "value" Text) => a -> Value
processJSON val = do
  let r = val ^. CF.value
  value . encodeUtf8 $ r

processJSON' :: (Field.HasField a "value" Text) => GRPCResponse a -> GRPCResponse Value
processJSON' = fmap processJSON

printJSON :: MonadIO m => Either String Value -> m ()
printJSON v =
  case v of
    Left err       -> liftIO $ putStrLn err
    Right jsonVals -> printJSONValues jsonVals

printJSON' :: MonadIO m => GRPCResult Value -> m ()
printJSON' = printJSON . fmap grpcResponse

printJSONValues :: MonadIO m => Value -> m ()
printJSONValues = liftIO . BSL8.putStrLn . encodePretty

value :: BS.ByteString -> Value
value s =
  case eitherDecodeStrict' s of
    Right v -> v
    Left err -> error ("Error in gRPC output decoding as a json: " ++ err)

value' :: forall a b. (Field.HasField a "value" b) => GRPCResponse a -> GRPCResponse b
value' = fmap (^. CF.value)
