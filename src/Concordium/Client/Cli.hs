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

decodeJsonArg :: FromJSON a => String -> Maybe Text -> Maybe (Either String a)
decodeJsonArg key input = do
  v <- input
  Just $ case AE.eitherDecodeStrict $ encodeUtf8 v of
    Left err -> Left $ printf "%s: cannot parse '%s' as JSON: %s" key v err
    Right r -> Right r

decodeKeysArg :: Maybe Text -> Maybe (Either String KeyMap)
decodeKeysArg = decodeJsonArg "keys"

getAddressArg :: String -> Maybe Text -> IO IDTypes.AccountAddress
getAddressArg name input = do
  v <- getArg name input
  case IDTypes.addressFromText v of
    Left err -> die $ printf "%s: %s" name err
    Right a -> return a

data TransactionState = Received | Committed | Finalized | Absent deriving (Eq, Ord, Show)

type TransactionBlockResults = HM.HashMap BlockHash (Maybe TransactionSummary)

data TransactionStatusResult = TransactionStatusResult
  { tsrState :: !TransactionState
  , tsrResults :: !TransactionBlockResults } -- TODO Rename to "blocks".
  deriving (Eq, Show)

instance AE.FromJSON TransactionStatusResult where
  parseJSON Null = return TransactionStatusResult{tsrState = Absent, tsrResults = HM.empty}
  parseJSON v = flip (withObject "Transaction status") v $ \obj -> do
    tsrState <- (obj .: "status" :: Parser String) >>= \case
      "received" -> return Received
      "committed" -> return Committed
      "finalized" -> return Finalized
      s -> fail $ printf "invalid status '%s'" s
    tsrResults <- obj .:? "outcomes" .!= HM.empty
    return $ TransactionStatusResult {..}

class (Monad m) => TransactionStatusQuery m where
  queryTransactionStatus :: TransactionHash -> m TransactionStatusResult
  wait :: Int -> m ()
