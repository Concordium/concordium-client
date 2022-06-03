{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import           Concordium.Client.GRPC
import qualified Concordium.Client.GRPC as GRPC

import qualified Concordium.Types as Types
import           Control.Monad.Except
import           Data.Word

data Routes r = Routes
    -- Public Middleware APIs
    { consensusStatus :: r :-
        "v1" :> "consensusStatus" :> Get '[JSON] Aeson.Value

    , blockSummary :: r :-
        "v1" :> "blockSummary" :> Capture "blockHash" Text
                               :> Get '[JSON] Aeson.Value

    , blockInfo :: r :-
        "v1" :> "blockInfo" :> Capture "blockHash" Text
                            :> Get '[JSON] Aeson.Value

    , blocksByHeight :: r :-
        "v1" :> "blocksByHeight" :> Capture "blockHeight" Word64
                                 :> Get '[JSON] Aeson.Value

    , transactionStatus :: r :-
        "v1" :> "transactionStatus" :> Capture "hash" Text
                                    :> Get '[JSON] Aeson.Value
    }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

servantApp :: EnvData -> Application
servantApp nodeBackend = genericServe routesAsServer
 where
  routesAsServer = Routes {..} :: Routes AsServer

  consensusStatus :: Handler Aeson.Value
  consensusStatus = liftIO $ proxyGrpcCall nodeBackend GRPC.getConsensusStatus

  blockSummary :: Text -> Handler Aeson.Value
  blockSummary blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockSummary blockhash)

  blockInfo :: Text -> Handler Aeson.Value
  blockInfo blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockInfo blockhash)

  blocksByHeight :: Word64 -> Handler Aeson.Value
  blocksByHeight height = liftIO $
    proxyGrpcCall nodeBackend (GRPC.getBlocksAtHeight Types.BlockHeight {theBlockHeight = height} Nothing Nothing)

  transactionStatus :: Text -> Handler Aeson.Value
  transactionStatus hash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getTransactionStatus hash)

proxyGrpcCall :: EnvData -> ClientMonad IO (Either a Aeson.Value) -> IO Aeson.Value
proxyGrpcCall nodeBackend query = do
  result <- runGRPC nodeBackend query
  case result of
    Right obj ->
      pure $ Aeson.toJSON obj
    _ ->
      pure Aeson.Null

runGRPC :: (MonadIO m) => EnvData -> ClientMonad m a -> m a
runGRPC envData c =
  runClient envData c >>= \case
    Left err -> liftIO $ fail (show err)
    Right x -> return x
