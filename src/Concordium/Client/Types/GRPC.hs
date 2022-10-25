module Concordium.Client.Types.GRPC (
    GRPCResult
  , GRPCResponse(..)
  , GRPCHeaderList
  ) where

import Network.GRPC.Client

data GRPCResponse a = GRPCResponse
  { grpcHeaders :: CIHeaderList,
    grpcResponse :: a
  } deriving Show

instance Functor GRPCResponse where
  fmap f (GRPCResponse hds v) = GRPCResponse hds $ f v

type GRPCResult a = Either String (GRPCResponse a)
type GRPCHeaderList = CIHeaderList