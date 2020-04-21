module Concordium.Client.Parse
  ( parseTransactionHash
  , parseBlockHash ) where

import Concordium.Crypto.SHA256

import Data.Text

parseHash :: Text -> Maybe Hash
parseHash t =
  case reads $ unpack t :: [(Hash, String)] of
    [(h, "")] -> Just h
    _ -> Nothing

parseTransactionHash :: Text -> Maybe Hash
parseTransactionHash = parseHash

parseBlockHash :: Text -> Maybe Hash
parseBlockHash = parseHash
