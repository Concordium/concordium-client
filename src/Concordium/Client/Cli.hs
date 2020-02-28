{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Concordium.Client.Cli where

import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Client.Types.Transaction
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types.Transactions as Types

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Aeson as AE
import Data.Aeson.Types as AE
import Data.List
import Data.Text
import Data.Text.Encoding
import Prelude hiding (fail)
import Text.Printf
import System.Exit (die)

-- Hardcode network ID and hook.
defaultNetId :: Int
defaultNetId = 100

defaultHook :: Bool
defaultHook = True

getArg :: String -> Maybe a -> IO a
getArg name input = case input of
  Nothing -> die $ name ++ " not provided"
  Just v -> return v

getJsonArg :: FromJSON a => String -> Maybe Text -> IO a
getJsonArg key input = do
  v <- getArg key input
  case AE.eitherDecodeStrict $ encodeUtf8 v of
    Left err -> die $ printf "%s: cannot parse '%s' as JSON: %s" key v err
    Right r -> return r

getKeysArg :: Maybe Text -> IO KeyMap
getKeysArg = getJsonArg "keys"

getAddressArg :: String -> Maybe Text -> IO IDTypes.AccountAddress
getAddressArg name input = do
  v <- getArg name input
  case IDTypes.addressFromText v of
    Left err -> die $ printf "%s: %s" name err
    Right a -> return a

data TransactionState = Pending | Committed | Finalized | Absent deriving (Eq, Ord, Show)

data TransactionStatusResultItem = TransactionStatusResultItem
  { tsriBlockHash :: !BlockHash
  , tsriResult :: !Text
  , tsriEvents :: ![Text]
  , tsriExecutionEnergyCost :: !Energy
  , tsriExecutionCost :: !Amount }
  deriving (Eq, Show)

data TransactionStatusResult = TransactionStatusResult
  { tsrState :: !TransactionState
  , tsrResults :: ![TransactionStatusResultItem]
  , tsrHash :: !TransactionHash }
  deriving (Eq, Show)

instance AE.FromJSON TransactionStatusResultItem where
  parseJSON = withObject "Transaction status result" $ \v -> do
    tsriBlockHash <- v .: "blockHash"
    tsriResult <- v .: "result"
    tsriEvents <- v .: "events"
    tsriExecutionEnergyCost <- v .: "executionEnergyCost"
    tsriExecutionCost <- v .: "executionCost"
    return $ TransactionStatusResultItem {..}

instance AE.FromJSON TransactionStatusResult where
  parseJSON = withObject "Transaction status" $ \v -> do
    tsrState <- (v .: "status" :: Parser String) >>= \case
      "pending" -> return Pending
      "absent" -> return Absent
      "committed" -> return Committed
      "finalized" -> return Finalized
      s -> fail $ printf "invalid status '%s'" s
    tsrResults <- v .: "results"
    tsrHash <- v .: "transactionHash"
    return $ TransactionStatusResult {..}

class (Monad m) => TransactionStatusQuery m where
  queryTransactionStatus :: Types.TransactionHash -> m TransactionStatusResult
  wait :: Int -> m ()
