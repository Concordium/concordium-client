module Concordium.Client.Cli where

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.ID.Types as IDTypes
import Concordium.Types

import Concordium.Client.Parse
import Concordium.Client.Types.TransactionStatus
import Concordium.Client.Types.Transaction

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as C
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe
import Data.Text hiding (empty, lines, map, null, last)
import Data.Text.Encoding
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Prelude hiding (fail, log)
import Text.PrettyPrint
import Text.Printf
import System.Console.ANSI
import System.Exit (exitFailure, exitSuccess)
import System.IO

data Level = Info | Warn | Err deriving (Eq)

-- |Log a list of sentences. The sentences are pretty printed (capital first letter and dot at the end),
-- so the input messages should only contain capital letters for names and have no dot suffix.
-- Sentences will be joined on the same line as long as the resulting line doesn't exceed 90 chars.
-- Depending on the log level, an appropriate prefix is added to the first line.
-- All lines will be indented such that they align with the first line
-- (i.e. as if they had all been prefixed).
log :: MonadIO m => Level -> Maybe Color -> [String] -> m ()
log lvl color msgs = do
  let doc = prefix <+> fsep (expandLines $ map (prettyMsg ".") msgs)
      out = logStrLn $ renderStyle s doc
  case color of
    Nothing -> out
    Just c -> do
      liftIO $ setSGR [SetColor Foreground Vivid c]
      out
      liftIO $ setSGR [Reset]
  where
    s = Style { mode = PageMode, lineLength = 90, ribbonsPerLine = 1.0 }
    prefix = case lvl of
               Info -> empty
               Warn-> text "Warning:"
               Err -> text "Error:"

logSuccess :: MonadIO m => [String] -> m ()
logSuccess = log Info $ Just Green

logInfo :: MonadIO m => [String] -> m ()
logInfo = log Info Nothing

logWarn :: MonadIO m => [String] -> m ()
logWarn = log Warn $ Just Yellow

logError :: MonadIO m => [String] -> m ()
logError = log Err $ Just Red

logFatal :: MonadIO m => [String] -> m a
logFatal msgs = logError msgs >> liftIO exitFailure

logExit :: MonadIO m => [String] -> m a
logExit msgs = logInfo msgs >> liftIO exitSuccess

-- |Expand each string into a document of the string's lines joined together using vcat.
expandLines :: [String] -> [Doc]
expandLines = map $ vcat . map text . lines

-- |Ensure that a string is printable as a sentence by converting the first letter to upper case
-- and, unless it already ends with "standard" punctuation, appending the provided punctionation.
prettyMsg :: String -> String -> String
prettyMsg punctuation = \case
  "" -> ""
  (x:xs) -> let s = if null xs || last xs `elem` p
                    then xs
                    else xs ++ punctuation
            in (C.toUpper x) : s
  where p = ".,:;?!{}" :: String

logStr :: MonadIO m => String -> m ()
logStr = liftIO . hPutStr stderr

logStrLn :: MonadIO m => String -> m ()
logStrLn = liftIO . hPutStrLn stderr

-- |Ask the user to "confirm" on stdin and return the result.
askConfirmation :: Maybe String -> IO Bool
askConfirmation prompt = do
  putStr $ prettyMsg " [yN]: " $ fromMaybe defaultPrompt prompt
  input <- T.getLine
  return $ strip (toLower input) == "y"
  where defaultPrompt = "confirm"

-- |Standardized method of exiting the command because the transaction is cancelled.
exitTransactionCancelled :: IO ()
exitTransactionCancelled = logExit ["transaction cancelled"]

getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
  tz <- getCurrentTimeZone
  localTimeOfDay . utcToLocalTime tz <$> getCurrentTime

getCurrentTimeUnix :: IO TransactionExpiryTime
getCurrentTimeUnix = TransactionExpiryTime . round <$> getPOSIXTime

timeFromTransactionExpiryTime :: TransactionExpiryTime -> UTCTime
timeFromTransactionExpiryTime = posixSecondsToUTCTime . fromIntegral . expiry

data AccountInfoResult = AccountInfoResult
  { airAmount :: !Amount
  , airNonce :: !Nonce
  , airDelegation :: !(Maybe BakerId),
    airCredentials :: ![IDTypes.CredentialDeploymentValues]
  , airInstances :: ![ContractAddress]}
  deriving (Show)

instance AE.FromJSON AccountInfoResult where
  parseJSON = withObject "Account info" $ \v -> do
    airAmount <- v .: "accountAmount"
    airNonce <- v .: "accountNonce"
    airDelegation <- v .: "accountDelegation"
    airCredentials <- v .: "accountCredentials"
    airInstances <- v .: "accountInstances"
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
  , birTransactionEnergyCost :: Energy
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

data NextAccountNonce = NextAccountNonce
  { nanNonce :: Nonce
  , nanAllFinal :: Bool }

instance AE.FromJSON NextAccountNonce where
  parseJSON = withObject "next nonce" $ \v -> do
    nanNonce <- v .: "nonce"
    nanAllFinal <- v .: "allFinal"
    return NextAccountNonce {..}

data BakerKeys =
  BakerKeys
  { bkSigSignKey :: BlockSig.SignKey
  , bkSigVerifyKey :: BlockSig.VerifyKey
  , bkAggrSignKey :: Bls.SecretKey
  , bkAggrVerifyKey :: Bls.PublicKey
  , bkElectionSignKey :: VRF.SecretKey
  , bkElectionVerifyKey :: VRF.PublicKey }

instance AE.FromJSON BakerKeys where
  parseJSON = withObject "Baker keys" $ \v -> do
    bkAggrSignKey <- v .: "aggregationSignKey"
    bkAggrVerifyKey <- v .: "aggregationVerifyKey"
    bkElectionSignKey <- v .: "electionPrivateKey"
    bkElectionVerifyKey <- v .: "electionVerifyKey"
    bkSigSignKey <- v .: "signatureSignKey"
    bkSigVerifyKey <- v .: "signatureVerifyKey"
    return BakerKeys {..}

instance AE.ToJSON BakerKeys where
  toJSON v = object [ "aggregationSignKey" .= bkAggrSignKey v
                    , "aggregationVerifyKey" .= bkAggrVerifyKey v
                    , "electionPrivateKey" .= bkElectionSignKey v
                    , "electionVerifyKey" .= bkElectionVerifyKey v
                    , "signatureSignKey" .= bkSigSignKey v
                    , "signatureVerifyKey" .= bkSigVerifyKey v ]

-- |Information about a given account sufficient to sign transactions.
data AccountKeys =
  AccountKeys
  { akAddress :: AccountAddress
  , akKeys :: KeyMap
  , akThreshold :: IDTypes.SignatureThreshold }

instance AE.FromJSON AccountKeys where
  parseJSON = withObject "Account keys" $ \v -> do
    akAddress <- v .: "account"
    akKeys <- v .: "keys"
    let numKeys = Map.size akKeys
    unless (numKeys >= 1 && numKeys <= 255) $ fail $ "Too few or too many keys: " ++ show numKeys
    akThreshold <- v .:? "threshold" .!= fromIntegral numKeys
    return AccountKeys {..}

data NamedAddress = NamedAddress { naName :: Maybe Text, naAddr :: AccountAddress }
  deriving (Show, Eq)

-- |Hardcoded network ID.
defaultNetId :: Int
defaultNetId = 100

-- |If the string starts with @ we assume the remaining characters are a file name
-- and we try to read the contents of that file.
decodeJsonArg :: FromJSON a => Text -> Maybe (IO (Either String a))
decodeJsonArg v =
  Just $ do
    let bs = encodeUtf8 v
    res <- case BS.uncons bs of
             Just ('@', rest) ->
               AE.eitherDecodeFileStrict (BS.unpack rest)
             _ -> return $ AE.eitherDecodeStrict bs
    return $ case res of
               Left err -> Left $ printf "cannot parse '%s' as JSON: %s" v err
               Right r -> Right r

getExpiryArg :: String -> TransactionExpiryTime -> Maybe Text -> IO TransactionExpiryTime
getExpiryArg name now input =
  case input of
    Nothing -> return $ now + defaultExpiryDurationSecs
    Just v -> case parseExpiry now v of
                Left err -> logFatal [printf "invalid %s '%s': %s" name v err]
                Right e -> return e
  where defaultExpiryDurationSecs = 600 -- 10 mins

class (Monad m) => TransactionStatusQuery m where
  queryTransactionStatus :: TransactionHash -> m TransactionStatusResult
  wait :: Int -> m ()
