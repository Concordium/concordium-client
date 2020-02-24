module Concordium.Client.Validate
  ( isValidTransactionHash
  , isValidBlockHash
  , validateTransactionHash
  , validateBlockHash ) where

import Concordium.Crypto.SHA256

import Control.Monad hiding (fail)
import Data.Text
import Text.Printf
import System.Exit (die)

isValidHash :: Text -> Bool
isValidHash h =
  case reads $ unpack h :: [(Hash, String)] of
    [(_, "")] -> True
    _ -> False

validateHash :: Text -> IO ()
validateHash h = when (not $ isValidHash h) (die $ printf "invalid hash: %s" h)

isValidTransactionHash :: Text -> Bool
isValidTransactionHash = isValidHash

isValidBlockHash :: Text -> Bool
isValidBlockHash = isValidHash

validateTransactionHash :: Text -> IO ()
validateTransactionHash = validateHash

validateBlockHash :: Text -> IO ()
validateBlockHash = validateHash
