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
import Data.Char
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time
import Data.Word
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

data ConsensusStatusResult = ConsensusStatusResult
  { csrBestBlock :: BlockHash
  , csrGenesisBlock :: BlockHash
  , csrLastFinalizedBlock :: BlockHash
  , csrBestBlockHeight :: Word64
  , csrLastFinalizedBlockHeight :: Word64
  , csrBlocksReceivedCount :: Int
  , csrBlockLastReceivedTime :: Maybe UTCTime
  , csrBlockReceiveLatencyEMA :: Double
  , csrBlockReceiveLatencyEMSD :: Double
  , csrBlockReceivePeriodEMA :: Maybe Double
  , csrBlockReceivePeriodEMSD :: Maybe Double
  , csrBlocksVerifiedCount :: Int
  , csrBlockLastArrivedTime :: Maybe UTCTime
  , csrBlockArriveLatencyEMA :: Double
  , csrBlockArriveLatencyEMSD :: Double
  , csrBlockArrivePeriodEMA :: Maybe Double
  , csrBlockArrivePeriodEMSD :: Maybe Double
  , csrTransactionsPerBlockEMA :: Double
  , csrTransactionsPerBlockEMSD :: Double
  , csrFinalizationCount :: Int
  , csrLastFinalizedTime :: Maybe UTCTime
  , csrFinalizationPeriodEMA :: Maybe Double
  , csrFinalizationPeriodEMSD :: Maybe Double }

instance AE.FromJSON ConsensusStatusResult where
  parseJSON = withObject "Consensus state" $ \v -> do
    csrBestBlock <- v .: "bestBlock"
    csrGenesisBlock <- v .: "genesisBlock"
    csrLastFinalizedBlock <- v .: "lastFinalizedBlock"
    csrBestBlockHeight <- v .: "bestBlockHeight"
    csrLastFinalizedBlockHeight <- v .: "lastFinalizedBlockHeight"
    csrBlocksReceivedCount <- v .: "blocksReceivedCount"
    csrBlockLastReceivedTime <- v .: "blockLastReceivedTime"
    csrBlockReceiveLatencyEMA <- v .: "blockReceiveLatencyEMA"
    csrBlockReceiveLatencyEMSD <- v .: "blockReceiveLatencyEMSD"
    csrBlockReceivePeriodEMA <- v .: "blockReceivePeriodEMA"
    csrBlockReceivePeriodEMSD <- v .: "blockReceivePeriodEMSD"
    csrBlocksVerifiedCount <- v .: "blocksVerifiedCount"
    csrBlockLastArrivedTime <- v .: "blockLastArrivedTime"
    csrBlockArriveLatencyEMA <- v .: "blockArriveLatencyEMA"
    csrBlockArriveLatencyEMSD <- v .: "blockArriveLatencyEMSD"
    csrBlockArrivePeriodEMA <- v .: "blockArrivePeriodEMA"
    csrBlockArrivePeriodEMSD <- v .: "blockArrivePeriodEMSD"
    csrTransactionsPerBlockEMA <- v .: "transactionsPerBlockEMA"
    csrTransactionsPerBlockEMSD <- v .: "transactionsPerBlockEMSD"
    csrFinalizationCount <- v .: "finalizationCount"
    csrLastFinalizedTime <- v .: "lastFinalizedTime"
    csrFinalizationPeriodEMA <- v .: "finalizationPeriodEMA"
    csrFinalizationPeriodEMSD <- v .: "finalizationPeriodEMSD"
    return $ ConsensusStatusResult {..}

data BirkParametersResult = BirkParametersResult
  { bprElectionNonce :: LeadershipElectionNonce
  , bprElectionDifficulty :: ElectionDifficulty
  , bprBakers :: [BirkParametersBakerResult] }

instance AE.FromJSON BirkParametersResult where
  parseJSON = withObject "Birk parameters" $ \v -> do
    bprElectionNonce <- v .: "electionNonce"
    bprElectionDifficulty <- v .: "electionDifficulty"
    bprBakers <- v .: "bakers"
    return $ BirkParametersResult {..}

data BirkParametersBakerResult = BirkParametersBakerResult
  { bpbrId :: BakerId
  , bpbrLotteryPower :: ElectionDifficulty
  , bpbrAccount :: IDTypes.AccountAddress }

instance AE.FromJSON BirkParametersBakerResult where
  parseJSON = withObject "Baker" $ \v -> do
    bpbrId <- v .: "bakerId"
    bpbrLotteryPower <- v .: "bakerLotteryPower"
    bpbrAccount <- v .: "bakerAccount"
    return $ BirkParametersBakerResult {..}

data BlockInfoResult = BlockInfoResult
  { birBlockHash :: BlockHash
  , birBlockParent :: BlockHash
  , birBlockLastFinalized :: BlockHash
  , birBlockReceiveTime :: UTCTime
  , birBlockArriveTime :: UTCTime
  , birBlockSlot :: Word64
  , birBlockSlotTime :: UTCTime
  , birBlockBaker :: Maybe BakerId
  , birFinalized :: Bool
  , birTransactionCount :: Integer
  , birTransactionEnergyCost :: Amount
  , birTransactionsSize :: Integer
  , birTotalAmount :: Amount
  , birTotalEncryptedAmount :: Amount
  , birCentralBankAmount :: Amount
  , birMintedAmountPerSlot :: Amount
  , birExecutionCost :: Amount }

instance AE.FromJSON BlockInfoResult where
  parseJSON = withObject "Block info" $ \v -> do
    birBlockHash <- v .: "blockHash"
    birBlockParent <- v .: "blockParent"
    birBlockLastFinalized <- v .: "blockLastFinalized"
    birBlockReceiveTime <- v .: "blockReceiveTime"
    birBlockArriveTime <- v .: "blockArriveTime"
    birBlockSlot <- v .: "blockSlot"
    birBlockSlotTime <- v .: "blockSlotTime"
    birBlockBaker <- v .: "blockBaker"
    birFinalized <- v .: "finalized"
    birTransactionCount <- v .: "transactionCount"
    birTransactionEnergyCost <- v .: "transactionEnergyCost"
    birTransactionsSize <- v .: "transactionsSize"
    birTotalAmount <- v .: "totalAmount"
    birTotalEncryptedAmount <- v .: "totalEncryptedAmount"
    birCentralBankAmount <- v .: "centralBankAmount"
    birMintedAmountPerSlot <- v .: "mintedAmountPerSlot"
    birExecutionCost <- v .: "executionCost"
    return BlockInfoResult {..}

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
