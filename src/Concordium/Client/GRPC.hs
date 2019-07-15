{-# LANGUAGE OverloadedStrings #-}

module Concordium.Client.GRPC where

import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client
import           Data.ByteString.Char8

data GrpcConfig =
  GrpcConfig
    { host :: HostName
    , port :: PortNumber
    -- Target node, i.e. "node-0" for use with grpc-proxy.eu.test.concordium.com against testnet
    , target :: Maybe String
    }
  deriving (Show)

mkGrpcClient :: GrpcConfig -> IO GrpcClient
mkGrpcClient config =
  let
    auth = ("authentication", "rpcadmin")

    headers =
      case target config of
        Just target ->
          [auth, ("target", pack target)]

        Nothing ->
          [auth]
  in
  setupGrpcClient
    ((grpcClientConfigSimple (host config) (port config) False)
       { _grpcClientConfigCompression = gzip
       , _grpcClientConfigHeaders = headers
       })
