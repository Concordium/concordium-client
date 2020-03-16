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
import Data.Char
import Data.List
import Data.Text (Text)
import Data.Text.Encoding
import Prelude hiding (fail, log)
import Text.PrettyPrint
import Text.Printf
import System.Exit (die, exitFailure)
import System.IO

data Level = Info | Warn | Err deriving (Eq)

-- Logs a list of sentences. The sentences are pretty printed (capital first letter and dot at the end),
-- so the input messages should only contain capital letters for names and have no dot suffix.
-- Sentences will be joined on the same line as long as the resulting line doesn't exceed 90 chars.
-- Depending on the log level, an appropriate prefix is added to the first line.
-- All lines will be indented such that they align with the first line
-- (i.e. as if they had all been prefixed).
log :: Level -> [String] -> IO ()
log lvl msgs =
  logStrLn $ renderStyle s doc
  where
    s = Style { mode = PageMode, lineLength = 90, ribbonsPerLine = 1.0 }
    doc = prefix <+> fsep (map (text . prettyMsg) msgs)
    prefix = case lvl of
               Info -> empty
               Warn-> text "Warning:"
               Err -> text "Error:"

logInfo :: [String] -> IO ()
logInfo = log Info

logWarn :: [String] -> IO ()
logWarn = log Warn

logError :: [String] -> IO ()
logError = log Err

logFatal :: [String] -> IO a
logFatal msgs = log Err msgs >> exitFailure

prettyMsg :: String -> String
prettyMsg = \case
  "" -> ""
  (x:xs) -> (toUpper x : xs) ++ "."

logStr :: String -> IO ()
logStr = hPutStr stderr

logStrLn :: String -> IO ()
logStrLn = hPutStrLn stderr

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
