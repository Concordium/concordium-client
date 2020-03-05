{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Concordium.Client.Cli where

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Client.Types.Transaction
import qualified Concordium.ID.Types as IDTypes

import Control.Monad hiding (fail)
import Control.Monad.Fail
import qualified Data.HashMap.Strict as HM
import Data.Aeson as AE
import Data.Aeson.Types as AE
import Data.List
import Data.Text
import Data.Text.Encoding
import Prelude hiding (fail)
import Text.Printf
import System.Exit (die)

data AccountInfoResult = AccountInfoResult
  { airAmount :: !Amount
  , airNonce :: !Nonce
  , airDelegation :: !(Maybe BakerId),
    -- TODO Change to ![IDTypes.CredentialDeploymentValues] once backend is updated.
    airCredentials :: ![(Int, IDTypes.CredentialDeploymentValues)] }
  deriving (Show)

instance AE.FromJSON AccountInfoResult where
  parseJSON = withObject "Account info" $ \v -> do
    airAmount <- v .: "accountAmount"
    airNonce <- v .: "accountNonce"
    airDelegation <- v .: "accountDelegation"
    airCredentials <- v .: "accountCredentials"
    return $ AccountInfoResult {..}

-- Hardcode network ID and hook.
defaultNetId :: Int
defaultNetId = 100

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

data TransactionState = Received | Committed | Finalized | Absent deriving (Eq, Ord, Show)

data TransactionStatusResultItem = TransactionStatusResultItem
  { tsriBlockHash :: !BlockHash
  , tsriResult :: !Text
  , tsriEvents :: ![Text]
  , tsriExecutionEnergyCost :: !Energy
  , tsriExecutionCost :: !Amount }
  deriving (Eq, Show)

data TransactionStatusResult = TransactionStatusResult
  { tsrState :: !TransactionState
  , tsrResults :: !(HM.HashMap BlockHash (Maybe TransactionSummary))
  }
  deriving (Eq, Show)

instance AE.FromJSON TransactionStatusResult where
  parseJSON Null = return TransactionStatusResult{tsrState = Absent, tsrResults = HM.empty}
  parseJSON v = flip (withObject "Transaction status") v $ \obj -> do
    tsrState <- (obj .: "status" :: Parser String) >>= \case
      "received" -> return Received
      "committed" -> return Committed
      "finalized" -> return Finalized
      s -> fail $ printf "invalid status '%s'" s
    tsrResults <- foldM (\hm (k, summary) -> do
                            case AE.fromJSON (String k) of
                              AE.Error _ -> return hm
                              AE.Success bh -> flip (HM.insert bh) hm <$> parseJSON summary
                        ) HM.empty (HM.toList obj)
    return $ TransactionStatusResult {..}

class (Monad m) => TransactionStatusQuery m where
  queryTransactionStatus :: TransactionHash -> m TransactionStatusResult
  wait :: Int -> m ()
