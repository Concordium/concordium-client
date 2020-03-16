{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Concordium.Client.Cli where

import Concordium.Types
import Concordium.Client.Types.TransactionStatus
import Concordium.Client.Types.Transaction
import qualified Concordium.ID.Types as IDTypes

import Control.Monad hiding (fail)
import Data.Aeson as AE
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

class (Monad m) => TransactionStatusQuery m where
  queryTransactionStatus :: TransactionHash -> m TransactionStatusResult
  wait :: Int -> m ()
