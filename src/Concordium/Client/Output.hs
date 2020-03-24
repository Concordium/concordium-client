{-# LANGUAGE LambdaCase #-}

module Concordium.Client.Output where

import qualified Concordium.Crypto.SignatureScheme as S
import Concordium.Client.Cli
import Concordium.Client.Commands (Verbose)
import Concordium.Client.Config
import Concordium.Client.Types.TransactionStatus
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.ID.Types as IDTypes

import Control.Monad.Writer
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Function
import Data.Functor
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Bool
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import Data.Time
import Text.Printf

-- PRINTER

type Printer = Writer [String] ()

runPrinter :: Printer -> IO ()
runPrinter p = mapM_ putStrLn $ execWriter p

-- TIME

showFormattedUtcTime :: UTCTime -> String
showFormattedUtcTime t = formatTime defaultTimeLocale rfc822DateFormat t

getFormattedLocalTimeOfDay :: IO String
getFormattedLocalTimeOfDay = do
  t <- getLocalTimeOfDay
  return $ showFormattedTimeOfDay t

getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
  tz <- getCurrentTimeZone
  t <- getCurrentTime
  return $ localTimeOfDay $ utcToLocalTime tz t

showFormattedTimeOfDay :: TimeOfDay -> String
showFormattedTimeOfDay = formatTime defaultTimeLocale "%T"

-- CONFIG

printBaseConfig :: BaseConfig -> Printer
printBaseConfig cfg = do
  tell [ printf "Base configuration:"
       , printf "- Verbose:            %s" (showYesNo $ bcVerbose cfg)
       , printf "- Account config dir: %s" (bcAccountCfgDir cfg) ]
  printAccountNameMap $ bcAccountNameMap cfg
  where printAccountNameMap m =
          if null m then
            tell [ "- Account name map:   " ++ showNone ]
          else do
            tell [ "- Account name map:"]
            printMap showEntry $ toSortedList m
        showEntry (n, a) = printf "    %s -> %s" n (show a)

printAccountConfig :: AccountConfig -> Printer
printAccountConfig cfg = do
  tell [ printf "Account configuration:"
       , printf "- Name:    %s" (fromMaybe (pack showNone) $ acName cfg)
       , printf "- Address: %s" (show $ acAddr cfg) ]
  printKeys $ acKeys cfg
  where printKeys m =
          if null m then
            tell [ "- Keys:    " ++ showNone ]
          else do
            tell [ "- Keys:" ]
            printMap showEntry $ toSortedList m
        showEntry (n, kp) =
          printf "    %s: %s" (show n) (showKeyPair kp)

printAccountConfigList :: [AccountConfig] -> Printer
printAccountConfigList cfgs =
  if null cfgs then
    tell [ "Account keys: " ++ showNone ]
  else do
    tell [ "Account keys:" ]
    forM_ cfgs $ \cfg -> do
      tell [ printf "- %s" (showNamedAddress cfg) ]
      printMap showEntry $ toSortedList $ acKeys cfg
  where showEntry (n, kp) =
          printf "    %s: %s" (show n) (showKeyPair kp)

-- ACCOUNT

showNone :: String
showNone = "none"

showRevealedAttributes :: M.Map IDTypes.AttributeTag IDTypes.AttributeValue -> String
showRevealedAttributes as =
  if null as then
    "none"
  else
    intercalate ", " $ map showAttr $ (M.toList as)
  where
    showTag t = case M.lookup t IDTypes.invMapping of
                  Nothing -> printf "<%s>" (show t)
                  Just k -> unpack k
    showAttr (t, IDTypes.AttributeValue v) = printf "%s=%s" (showTag t) (show v)

printAccountInfo :: Text -> AccountInfoResult -> Verbose -> Printer
printAccountInfo address a verbose = do
  tell [ printf "Address:    %s" address
       , printf "Amount:     %s" (showGtu $ airAmount a)
       , printf "Nonce:      %s" (show $ airNonce a)
       , printf "Delegation: %s" (maybe showNone show $ airDelegation a)
       , "" ]

  case airCredentials a of
      [] -> tell ["Credentials: " ++ showNone]
      creds -> do
        tell ["Credentials:"]
        if verbose then
          tell $ creds <&> (unpack . decodeUtf8 . BSL.toStrict . AE.encodePretty)
        else
          forM_ creds printCred

printCred :: IDTypes.CredentialDeploymentValues -> Printer
printCred c =
  tell [ printf "* %s:" (show $ IDTypes.cdvRegId c)
       , printf "  - Expiration: %s" expiry
       , printf "  - Revealed attributes: %s" (showRevealedAttributes attrs) ]
  where
    p = IDTypes.cdvPolicy c
    e = show $ IDTypes.pExpiry p
    attrs = IDTypes.pItems p
    expiry = case parseTimeM False defaultTimeLocale "%s" e of
               Nothing -> printf "invalid expiration time '%s'" e
               Just t -> showFormattedUtcTime t

printAccountList :: [Text] -> Printer
printAccountList addresses = tell $ map unpack addresses

showKeyPair :: S.KeyPair -> String
showKeyPair S.KeyPairEd25519 { S.signKey=sk, S.verifyKey=vk } =
  printf "sign=%s, verify=%s" (show sk) (show vk)

-- TRANSACTION

printTransactionStatus :: TransactionStatusResult -> Printer
printTransactionStatus status =
  case tsrState status of
    Received -> tell ["Transaction is pending."]
    Absent -> tell ["Transaction is absent."]
    Committed -> do
      case mapMaybe (\(k,v) -> maybe Nothing (\x -> Just (k, x)) v) $ sortBy (compare `on` fst) $ HM.toList (tsrResults status) of
        [] ->
          -- No blocks.
          tell ["Transaction is committed - no block information received (this should never happen!)."]
        [(hash, outcome)] ->
          -- Single block.
          tell [printf
                "Transaction is committed into block %s with %s."
                (show hash)
                (showOutcomeFragment outcome)]
        blocks ->
          -- Multiple blocks.
          case nub $ Prelude.map snd blocks of
            [outcome] -> do
              -- Single outcome.
              tell [printf
                     "Transaction is committed into the following %d blocks with %s:"
                     (length blocks)
                     (showOutcomeFragment outcome)]
              tell $ blocks <&> \(hash, _) ->
                                  printf "- %s" (show hash)
            _ -> do
              -- Multiple outcomes.
              tell [printf
                     "Transaction is committed into the following %d blocks:"
                     (length blocks)]
              tell $ blocks <&> \(hash, outcome) ->
                                  printf
                                    "- %s with %s."
                                    (show hash)
                                    (showOutcomeFragment outcome)
    Finalized -> do
      case mapMaybe (\(k,v) -> maybe Nothing (\x -> Just (k, x)) v) $ HM.toList (tsrResults status) of
        [] ->
          -- No blocks.
          tell ["Transaction is finalized - no block information received (this should never happen!)."]
        [(hash, outcome)] ->
          -- Single block.
          tell [printf
                 "Transaction is finalized into block %s with %s."
                 (show $ hash)
                 (showOutcomeFragment $ outcome)]
        _ ->
          -- Multiple blocks.
          tell ["Transaction is finalized into multiple blocks - this should never happen and may indicate a serious problem with the chain!"]
  where
    showCostFragment :: Types.Amount -> Types.Energy -> String
    showCostFragment gtu nrg = printf "%s (%s)" (showGtu gtu) (showNrg nrg)
    showOutcomeFragment :: Types.TransactionSummary -> String
    showOutcomeFragment outcome = printf
                                    "status \"%s\" and cost %s"
                                    (showOutcomeStatusFragment $ Types.tsResult outcome :: String)
                                    (showCostFragment (Types.tsCost outcome) (Types.tsEnergyCost outcome))
    showOutcomeStatusFragment = \case
      Types.TxSuccess _ -> "success"
      Types.TxReject _ -> "rejected"

-- CONSENSUS

printConsensusStatus :: ConsensusStatusResult -> Printer
printConsensusStatus r = do
  tell [ printf "Best block:                  %s" (show $ csrBestBlock r)
       , printf "Genesis block:               %s" (show $ csrGenesisBlock r)
       , printf "Last finalized block:        %s" (show $ csrLastFinalizedBlock r)
       , printf "Best block height:           %s" (show $ csrBestBlockHeight r)
       , printf "Last finalized block height: %s" (show $ csrLastFinalizedBlockHeight r)
       , printf "Blocks received count:       %s" (show $ csrBlocksReceivedCount r)
       , printf "Block last received time:    %s" (showMaybeUTC $ csrBlockLastReceivedTime r)
       , printf "Block receive latency:       %s" (showEmSeconds (csrBlockReceiveLatencyEMA r) (csrBlockReceiveLatencyEMSD r))
       , printf "Block receive period:        %s" (showMaybeEmSeconds (csrBlockReceivePeriodEMA r) (csrBlockReceivePeriodEMSD r))
       , printf "Blocks verified count:       %s" (show $ csrBlocksVerifiedCount r)
       , printf "Block last arrived time:     %s" (showMaybeUTC $ csrBlockLastArrivedTime r)
       , printf "Block arrive latency:        %s" (showEmSeconds (csrBlockArriveLatencyEMA r) (csrBlockArriveLatencyEMSD r))
       , printf "Block arrive period:         %s" (showMaybeEmSeconds (csrBlockArrivePeriodEMA r) (csrBlockArrivePeriodEMSD r))
       , printf "Transactions per block:      %s" (showEm (printf "%8.3f" $ csrTransactionsPerBlockEMA r) (printf "%8.3f" $ csrTransactionsPerBlockEMSD r))
       , printf "Finalization count:          %s" (show $ csrFinalizationCount r)
       , printf "Last finalized time:         %s" (showMaybeUTC $ csrLastFinalizedTime r)
       , printf "Finalization period:         %s" (showMaybeEmSeconds (csrFinalizationPeriodEMA r) (csrFinalizationPeriodEMSD r)) ]

printBirkParameters :: Bool -> BirkParametersResult -> Printer
printBirkParameters includeBakers r = do
  tell [ printf "Election nonce:      %s" (show $ bprElectionNonce r)
       , printf "Election difficulty: %s" (show $ bprElectionDifficulty r) ]
  when includeBakers $
    case bprBakers r of
      [] ->
         tell [ "Bakers:              " ++ showNone ]
      bs -> do
        tell [ "Bakers:"
             , printf "                             Account                       Lottery power"
             , printf "        ----------------------------------------------------------------" ]
        tell $ f <$> bs
  where f b = printf "%6s: %s    %.4f" (show $ bpbrId b) (show $ bpbrAccount b) (bpbrLotteryPower b)

-- BLOCK

printBlockInfo :: BlockInfoResult -> Printer
printBlockInfo b =
  tell [ printf "Hash:                       %s" (show $ birBlockHash b)
       , printf "Parent block:               %s" (show $ birBlockParent b)
       , printf "Last finalized block:       %s" (show $ birBlockLastFinalized b)
       , printf "Finalized:                  %s" (showYesNo $ birFinalized b)
       , printf "Receive time:               %s" (showFormattedUtcTime $ birBlockReceiveTime b)
       , printf "Arrive time:                %s" (showFormattedUtcTime $ birBlockArriveTime b)
       , printf "Slot:                       %s" (show $ birBlockSlot b)
       , printf "Slot time:                  %s" (showFormattedUtcTime $ birBlockSlotTime b)
       , printf "Baker:                      %s" (showMaybe show $ birBlockBaker b)
       , printf "Transaction count:          %d" (birTransactionCount b)
       , printf "Transaction energy cost:    %s" (showNrg $ birTransactionEnergyCost b)
       , printf "Transactions size:          %d" (birTransactionsSize b)
       , printf "Transaction execution cost: %s" (showGtu $ birExecutionCost b)
       , printf "Total amount:               %s" (showGtu $ birTotalAmount b)
       , printf "Total encrypted amount:     %s" (showGtu $ birTotalEncryptedAmount b)
       , printf "Central bank amount:        %s" (showGtu $ birCentralBankAmount b)
       , printf "Minted amount per slot:     %s" (showGtu $ birMintedAmountPerSlot b) ]

-- AMOUNT AND ENERGY

showGtu :: Types.Amount -> String
showGtu = printf "%.4f GTU" . (/amountPerGtu) . fromIntegral
  where amountPerGtu = 10000 :: Double

showNrg :: Types.Energy -> String
showNrg = printf "%s NRG" . show

-- UTIL

-- Produce a string fragment of the address and, if available, name of the account.
showNamedAddress :: AccountConfig -> String
showNamedAddress cfg =
  let addr = printf "'%s'" (show $ acAddr cfg)
  in case acName cfg of
    Nothing -> addr
    Just n -> printf "%s (%s)" addr n

showMaybe :: (a -> String) -> Maybe a -> String
showMaybe = maybe showNone

showMaybeUTC :: Maybe UTCTime -> String
showMaybeUTC = showMaybe showFormattedUtcTime

showEm :: String -> String -> String
showEm a d = printf "%s (EMA), %s (EMSD)" a d

showEmSeconds :: Double -> Double -> String
showEmSeconds a d = showEm (showSeconds a) (showSeconds d)

showMaybeEmSeconds :: Maybe Double -> Maybe Double -> String
showMaybeEmSeconds a d = case (a, d) of
                    (Just a', Just d') -> showEmSeconds a' d'
                    _ -> showNone

showSeconds :: Double -> String
showSeconds s = printf "%5d ms" (round $ 1000*s :: Int)

printMap :: ((k, v) -> String) -> [(k, v)] -> Printer
printMap s m = forM_ m $ \(k, v) -> tell [s (k, v)]

showYesNo :: Bool -> String
showYesNo = bool "no" "yes"

toSortedList :: Ord k => HM.HashMap k v -> [(k, v)]
toSortedList = sortOn fst . HM.toList
