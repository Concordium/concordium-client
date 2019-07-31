{-# LANGUAGE OverloadedStrings #-}

module Concordium.Client.GRPC where

import           Data.ByteString.Char8
import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client

data GrpcConfig =
  GrpcConfig
    { host   :: HostName
    , port   :: PortNumber
    -- Target node, i.e. "node-0" for use with grpc-proxy.eu.test.concordium.com against testnet
    , target :: Maybe String
    }
  deriving (Show)

mkGrpcClient :: GrpcConfig -> IO GrpcClient
mkGrpcClient config =
  let auth = ("authentication", "rpcadmin")
      header =
        case target config of
          Just t  -> [auth, ("target", pack t)]
          Nothing -> [auth]
   in setupGrpcClient
        ((grpcClientConfigSimple (host config) (port config) False)
           { _grpcClientConfigCompression = gzip
           , _grpcClientConfigHeaders = header
           })
