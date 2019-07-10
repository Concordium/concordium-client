{-# LANGUAGE OverloadedStrings #-}

module Concordium.Client.GRPC where

import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client

data GrpcConfig =
  GrpcConfig
    { host :: HostName
    , port :: PortNumber
    }
  deriving (Show)

mkGrpcClient :: GrpcConfig -> IO GrpcClient
mkGrpcClient config =
  setupGrpcClient
    ((grpcClientConfigSimple (host config) (port config) False)
       { _grpcClientConfigCompression = gzip
       , _grpcClientConfigHeaders = [("authentication", "rpcadmin")]
       })
