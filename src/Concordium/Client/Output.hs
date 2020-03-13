{-# LANGUAGE LambdaCase #-}

module Concordium.Client.Output where

import qualified Concordium.Crypto.SignatureScheme as S
import Concordium.Client.Cli
import Concordium.Client.Commands (Verbose)
import Concordium.Client.Config
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
import Data.Text (Text, unpack)
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
       , printf "- verbose:            %s" (showYesNo $ bcVerbose cfg)
       , printf "- account config dir: %s" (bcAccountCfgDir cfg)
       , printf "- account name map:" ]
  printMap showEntry $ bcAccountNameMap cfg
  where showEntry (n, a) =
          printf "    %s -> %s" n (show a)

printAccountConfig :: AccountConfig -> Printer
printAccountConfig cfg = do
  tell [ printf "Account configuration:"
       , printf "- name:    %s" (fromMaybe "none" $ acName cfg)
       , printf "- address: %s" (show $ acAddr cfg)
       , printf "- keys:" ]
  printMap showEntry $ acKeys cfg
  where showEntry (IDTypes.KeyIndex n, kp) =
          printf "    %s: %s" (show n) (showKeyPair kp)

printAccountConfigList :: [AccountConfig] -> Printer
printAccountConfigList cfgs = do
  tell [ "Account keys:" ]
  forM_ cfgs $ \cfg -> do
    tell [ printf "- %s" (showNamedAddress cfg) ]
    printMap showEntry $ acKeys cfg
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
       , printf "Amount:     %s GTU" (show $ airAmount a)
       , printf "Nonce:      %s" (show $ airNonce a)
       , printf "Delegation: %s" (maybe showNone show $ airDelegation a)
       , "" ]

  case Prelude.map snd $ airCredentials a of
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
       , printf "  - expiration: %s" expiry
       , printf "  - revealed attributes: %s" (showRevealedAttributes attrs) ]
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

data TransactionOutcome = TransactionOutcome
                          { toStatus :: Text
                          , toGtuCost :: Types.Amount
                          , toNrgCost :: Types.Energy }
                        deriving (Eq)

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
    showCostFragment gtu nrg = printf "%s GTU (%s NRG)" (show gtu) (show nrg)
    showOutcomeFragment :: Types.TransactionSummary -> String
    showOutcomeFragment outcome = printf
                                    "status \"%s\" and cost %s"
                                    (showOutcomeStatusFragment $ Types.tsResult outcome :: String)
                                    (showCostFragment (Types.tsCost outcome) (Types.tsEnergyCost outcome))
    showOutcomeStatusFragment = \case
      Types.TxSuccess _ -> "success"
      Types.TxReject _ -> "rejected"

-- Produce a string fragment of the address and, if available, name of the account.
showNamedAddress :: AccountConfig -> String
showNamedAddress cfg =
  let addr = printf "'%s'" (show $ acAddr cfg)
  in case acName cfg of
    Nothing -> addr
    Just n -> printf "%s (%s)" addr n

printMap :: ((k, v) -> String) -> HM.HashMap k v -> Printer
printMap s m = forM_ (HM.toList m) $ \(k, v) -> tell [s (k, v)]

showYesNo :: Bool -> String
showYesNo = bool "no" "yes"
