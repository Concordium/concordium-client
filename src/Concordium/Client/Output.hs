{-# LANGUAGE RecordWildCards #-}

module Concordium.Client.Output where

import Concordium.Common.Version
import Concordium.Client.Cli
import Concordium.Client.Commands (Verbose)
import Concordium.Client.Config
import Concordium.Client.Parse
import Concordium.Client.Types.TransactionStatus
import Concordium.Client.Types.Account
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.ID.Types as IDTypes

import Control.Monad.Writer
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Functor
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Bool
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import Data.Time
import Text.Printf
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import Data.Word

-- PRINTER

-- |Specialized writer for producing a list of lines.
type Printer = Writer [String] ()

-- |Print the lines of a printer.
runPrinter :: (MonadIO m) => Printer -> m ()
runPrinter = liftIO . mapM_ putStrLn . execWriter

-- HELPERS

-- | Serialize to JSON and pretty-print.
showPrettyJSON :: AE.ToJSON a => a -> String
showPrettyJSON = unpack . decodeUtf8 . BSL.toStrict . AE.encodePretty

-- TIME

-- |Convert time to string using the provided formatting and "default" (American) locale.
-- Normally one of the functions below should be used instead of this one.
showTime :: String -> UTCTime -> String
showTime = formatTime defaultTimeLocale

-- |Convert time to string using the RFC822 date formatting and "default" (American) locale.
showTimeFormatted :: UTCTime -> String
showTimeFormatted = showTime rfc822DateFormat

-- |Convert time to string formatted as "<month (3 letters)> <year (4 digits)>".
-- This is the format used for credential expiration.
showTimeYearMonth :: UTCTime -> String
showTimeYearMonth = showTime "%b %0Y"

-- |Convert time of day to string formatted as "<hour>:<minute>:<second>" (all zero-padded).
-- This is the format used for timestamps in logging.
showTimeOfDay :: TimeOfDay -> String
showTimeOfDay = formatTime defaultTimeLocale "%T"

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
       , printf "- Name:    %s" (fromMaybe (pack showNone) $ naName $ acAddr cfg)
       , printf "- Address: %s" (show $ naAddr $ acAddr cfg) ]
  printKeys $ acKeys cfg
  where printKeys m =
          if null m then
            tell [ "- Keys:    " ++ showNone ]
          else do
            tell [ "- Keys:" ]
            printMap showEntry $ toSortedList m
        showEntry (n, kp) =
          printf "    %s: %s" (show n) (showAccountKeyPair kp)

printAccountConfigList :: [AccountConfig] -> Printer
printAccountConfigList cfgs =
  if null cfgs then
    tell [ "Account keys: " ++ showNone ]
  else do
    tell [ "Account keys:" ]
    forM_ cfgs $ \cfg -> do
      let keys = acKeys cfg
      if null keys then
        tell [ printf "- %s: %s" (showNamedAddress $ acAddr cfg) showNone]
      else do
        tell [ printf "- %s:" (showNamedAddress $ acAddr cfg)]
        -- NB: While we do not have proper account exports or dedicated commands to print
        -- the full account key map, this command should print the account key map in the
        -- JSON format that can be used for importing and "account add-keys".
        tell [ showPrettyJSON keys ]
        -- printMap showEntry $ toSortedList keys
  -- where showEntry (n, kp) =
  --         printf "    %s: %s" (show n) (showAccountKeyPair kp)

-- ACCOUNT

-- |Standardized method of displaying "no" information.
showNone :: String
showNone = "none"

showRevealedAttributes :: M.Map IDTypes.AttributeTag IDTypes.AttributeValue -> String
showRevealedAttributes as =
  if null as then
    "none"
  else
    intercalate ", " $ map showAttr $ M.toList as
  where
    showTag t = case M.lookup t IDTypes.invMapping of
                  Nothing -> printf "<%s>" (show t)
                  Just k -> unpack k
    showAttr (t, IDTypes.AttributeValue v) = printf "%s=%s" (showTag t) (show v)

printAccountInfo :: NamedAddress -> AccountInfoResult -> Verbose -> Printer
printAccountInfo addr a verbose = do
  tell [ printf "Local name: %s" (showMaybe unpack $ naName addr)
       , printf "Address:    %s" (show $ naAddr addr)
       , printf "Balance:    %s" (showGtu $ airAmount a)
       , printf "Nonce:      %s" (show $ airNonce a)
       , printf "Delegation: %s" (maybe showNone (printf "baker %s" . show) $ airDelegation a)
       , "" ]

  case airCredentials a of
      [] -> tell ["Credentials: " ++ showNone]
      creds -> do
        tell ["Credentials:"]
        if verbose then
          tell $ creds <&> showPrettyJSON
        else
          forM_ creds printVersionedCred

-- |Print a versioned credential. This only prints the credential value, and not the
-- associated version.
printVersionedCred :: (Versioned IDTypes.CredentialDeploymentValues) -> Printer
printVersionedCred vc = printCred (vValue vc)

-- |Print the registration id, expiry date, and revealed attributes of a credential.
printCred :: IDTypes.CredentialDeploymentValues -> Printer
printCred c =
  tell [ printf "* %s:" (show $ IDTypes.cdvRegId c)
       , printf "  - Expiration: %s" expiry
       , printf "  - Revealed attributes: %s" (showRevealedAttributes attrs) ]
  where
    p = IDTypes.cdvPolicy c
    e = show $ IDTypes.pValidTo p
    attrs = IDTypes.pItems p
    expiry = case parseCredExpiry e of
               Nothing -> printf "invalid expiration time '%s'" e
               Just t -> showTimeYearMonth t

printAccountList :: [Text] -> Printer
printAccountList = tell . map unpack

printModuleList :: [Text] -> Printer
printModuleList = printAccountList

showAccountKeyPair :: EncryptedAccountKeyPair -> String
-- TODO Make it respect indenting if this will be the final output format.
-- An alternative is not to print the encrypted key here, but rather have that
-- as part of an export command.
showAccountKeyPair = showPrettyJSON


-- TRANSACTION

data TransactionBlockResult
  = NoBlocks
  | SingleBlock Types.BlockHash Types.TransactionSummary
  | MultipleBlocksUnambiguous [Types.BlockHash] Types.TransactionSummary
  | MultipleBlocksAmbiguous [(Types.BlockHash, Types.TransactionSummary)]

parseTransactionBlockResult :: TransactionStatusResult -> TransactionBlockResult
parseTransactionBlockResult status =
  case sortOn fst $ HM.toList (tsrResults status) of
    [(hash, outcome)] -> SingleBlock hash outcome
    blocks -> case nub $ map snd blocks of
                [] -> NoBlocks
                [outcome] -> let hashes = map fst blocks
                             in MultipleBlocksUnambiguous hashes outcome
                _ -> MultipleBlocksAmbiguous blocks

printTransactionStatus :: TransactionStatusResult -> Printer
printTransactionStatus status =
  case tsrState status of
    Received -> tell ["Transaction is pending."]
    Absent -> tell ["Transaction is absent."]
    Committed ->
      case parseTransactionBlockResult status of
        NoBlocks ->
          tell ["Transaction is committed, but no block information was received - this should never happen!"]
        SingleBlock hash outcome -> do
          tell [printf
                 "Transaction is committed into block %s with %s."
                 (show hash)
                 (showOutcomeFragment outcome)]
          tell $ showOutcomeResult False $ Types.tsResult outcome
        MultipleBlocksUnambiguous hashes outcome -> do
          tell [printf
                 "Transaction is committed into %d blocks with %s:"
                 (length hashes)
                 (showOutcomeFragment outcome)]
          tell $ hashes <&> printf "- %s" . show
          tell $ showOutcomeResult False $ Types.tsResult outcome
        MultipleBlocksAmbiguous blocks -> do
          tell [printf
                 "Transaction is committed into %d blocks:"
                 (length blocks)]
          sequence_ $ blocks <&> \(hash, outcome) -> do
            tell [ printf "- %s with %s:"
                     (show hash)
                     (showOutcomeFragment outcome) ]
            tell $ (showOutcomeResult True $ Types.tsResult outcome) <&> ("  * " ++)
    Finalized ->
      case parseTransactionBlockResult status of
        NoBlocks ->
          tell ["Transaction is finalized, but no block information was received - this should never happen!"]
        SingleBlock hash outcome -> do
          tell [printf
                 "Transaction is finalized into block %s with %s."
                 (show hash)
                 (showOutcomeFragment outcome)]
          tell $ showOutcomeResult False $ Types.tsResult outcome
        MultipleBlocksUnambiguous _ _ ->
          tell ["Transaction is finalized into multiple blocks - this should never happen and may indicate a serious problem with the chain!"]
        MultipleBlocksAmbiguous _ ->
          tell ["Transaction is finalized into multiple blocks - this should never happen and may indicate a serious problem with the chain!"]
   where
     showOutcomeFragment :: Types.TransactionSummary -> String
     showOutcomeFragment outcome = printf
                                     "status \"%s\" and cost %s"
                                     (showOutcomeStatusFragment $ Types.tsResult outcome :: String)
                                     (showOutcomeCost outcome)
     showOutcomeStatusFragment = \case
       Types.TxSuccess _ -> "success"
       Types.TxReject _ -> "rejected"

showOutcomeCost :: Types.TransactionSummary -> String
showOutcomeCost outcome = showCost (Types.tsCost outcome) (Types.tsEnergyCost outcome)

showCost :: Types.Amount -> Types.Energy -> String
showCost gtu nrg = printf "%s (%s)" (showGtu gtu) (showNrg nrg)

showOutcomeResult :: Verbose -> Types.ValidResult -> [String]
showOutcomeResult verbose = \case
  Types.TxSuccess es -> mapMaybe (showEvent verbose) es
  Types.TxReject r ->
    if verbose
    then [showRejectReason True r]
    else [printf "Transaction rejected: %s." (showRejectReason False r)]

-- |Return string representation of outcome event if verbose or if the event includes
-- relevant information that wasn't part of the transaction request. Otherwise return Nothing.
-- If verbose is true, the string includes the details from the fields of the event.
-- Otherwise, only the fields that are not known from the transaction request are included.
-- Currently this is only the baker ID from AddBaker, which is computed by the backend.
-- The non-verbose version is used by the transaction commands (through tailTransaction)
-- where the input parameters have already been specified manually and repeated in a block
-- of text that they confirmed manually.
-- The verbose version is used by 'transaction status' and the non-trivial cases of the above
-- where there are multiple distinct outcomes.
showEvent :: Verbose -> Types.Event -> Maybe String
showEvent verbose = \case
  Types.ModuleDeployed ref->
    verboseOrNothing $ printf "module '%s' deployed" (show ref)
  Types.ContractInitialized{..} ->
    verboseOrNothing $ printf "initialized contract '%s' from module '%s' with name '%s' and '%s' tokens"
                              (show ecAddress) (show ecRef) (show ecName) (show ecAmount)
  Types.Updated{..} ->
    verboseOrNothing $ printf "sent message '%s' and '%s' tokens from %s to %s"
                              (show euMessage) (show euAmount) (showAccount euInstigator) (showAccount $ Types.AddressContract euAddress)
  Types.Transferred{..} ->
    verboseOrNothing $ printf "transferred %s tokens from %s to %s" (show etAmount) (showAccount etFrom) (showAccount etTo)
  Types.AccountCreated addr ->
    verboseOrNothing $ printf "account '%s' created" (show addr)
  Types.CredentialDeployed{..} ->
    verboseOrNothing $ printf "credential with registration '%s' deployed onto account '%s'" (show ecdRegId) (show ecdAccount)
  Types.BakerAdded bid ->
    Just $ printf "baker added with ID %s" (show bid)
  Types.BakerRemoved bid ->
    verboseOrNothing $ printf "baker '%s' removed" (show bid)
  Types.BakerAccountUpdated{..} ->
    verboseOrNothing $ printf "reward account of baker '%s' updated to '%s'" (show ebauBaker) (show ebauNewAccount)
  Types.BakerKeyUpdated{..} ->
    verboseOrNothing $ printf "signature key of baker '%s' updated to '%s'" (show ebkuBaker) (show ebkuNewKey)
  Types.BakerElectionKeyUpdated{..} ->
    verboseOrNothing $ printf "election key of baker '%s' updated to '%s'" (show ebekuBaker) (show ebekuNewKey)
  Types.BakerAggregationKeyUpdated{..} ->
    verboseOrNothing $ printf "aggregation key of baker '%s' updated to '%s'" (show ebakuBaker) (show ebakuNewKey)
  Types.StakeDelegated{..} ->
    verboseOrNothing $ printf "stake of account '%s' delegated to baker '%s'" (show esdAccount) (show esdBaker)
  Types.StakeUndelegated{..} ->
    verboseOrNothing $ case esuBaker of
                         Nothing -> printf "stake of account '%s' undelegated (was not previous delegated)" (show esuAccount)
                         Just bid -> printf "stake of account '%s' undelegated from baker %s" (show esuAccount) (show bid)
  Types.ElectionDifficultyUpdated{..} ->
    verboseOrNothing $ printf "election difficulty updated to %f" eeduDifficulty
  where
    verboseOrNothing :: String -> Maybe String
    verboseOrNothing msg = if verbose then Just msg else Nothing

    showAccount :: Types.Address -> String
    showAccount = \case
      Types.AddressAccount a -> printf "account '%s'" (show a)
      Types.AddressContract a -> printf "contract '%s'" (show a)

-- |Return string representation of reject reason.
-- If verbose is true, the string includes the details from the fields of the reason.
-- Otherwise, only the fields that are not known from the transaction request are included.
-- Currently this is only the baker address from NotFromBakerAccount.
-- The non-verbose version is used by the transaction commands (through tailTransaction)
-- where the input parameters have already been specified manually and repeated in a block
-- of text that they confirmed manually.
-- The verbose version is used by 'transaction status' and the non-trivial cases of the above
-- where there are multiple distinct outcomes.
showRejectReason :: Verbose -> Types.RejectReason -> String
showRejectReason verbose = \case
  Types.ModuleNotWF ->
    "typechecking error"
  Types.MissingImports ->
    "missing imports"
  Types.ModuleHashAlreadyExists m ->
    if verbose then
      printf "module '%s' already exists" (show m)
    else
      "module already exists"
  Types.MessageTypeError ->
    "message to the receive method is not of the correct type"
  Types.ParamsTypeError ->
    "parameters of the init method are not of the correct types"
  Types.InvalidAccountReference a ->
    if verbose then
      printf "account '%s' does not exist" (show a)
    else
      "account does not exist"
  Types.InvalidContractReference m t ->
    if verbose then
      printf "referencing nonexistent contract '%s', '%s'" (show m) (show t)
    else
      "referencing non-existing contract"
  Types.InvalidModuleReference m ->
    if verbose then
      printf "referencing non-existent module '%s'" (show m)
    else
      "referencing non-existing module"
  Types.InvalidContractAddress c ->
    if verbose then
      printf "contract instance '%s' does not exist" (show c)
    else
      "contract instance does not exist"
  Types.ReceiverAccountNoCredential a ->
    if verbose then
      printf "receiver account '%s' does not have a valid credential" (show a)
    else
      "receiver account does not have a valid credential"
  Types.ReceiverContractNoCredential a ->
    if verbose then
      printf "receiver contract '%s' does not have a valid credential" (show a)
    else
      "receiver contract does not have a valid credential"
  Types.AmountTooLarge a amount ->
    if verbose then
      printf "account or contract '%s' does not have enough funds to transfer %s tokens" (show a) (show amount)
    else
      "insufficient funds"
  Types.SerializationFailure ->
    "serialization failed"
  Types.OutOfEnergy ->
    "not enough energy"
  Types.Rejected ->
    "contract logic failure"
  Types.NonExistentRewardAccount a ->
    if verbose then
      printf "account '%s' does not exist (tried to set baker reward account)" (show a)
    else
      "account does not exist"
  Types.InvalidProof ->
    "proof that baker owns relevant private keys is not valid"
  Types.RemovingNonExistentBaker b ->
    if verbose then
      printf "baker '%s' does not exist (tried to remove baker)" (show b)
    else
      "baker does not exist"
  Types.InvalidBakerRemoveSource a ->
    if verbose then
      printf "invalid sender account '%s' (tried to remove baker)" (show a)
    else
      "invalid sender account"
  Types.UpdatingNonExistentBaker b ->
    if verbose then
      printf "baker '%s' does not exist (tried to update baker signature key)" (show b)
    else
      "baker does not exist"
  Types.InvalidStakeDelegationTarget b ->
    if verbose then
      printf "invalid baker ID '%s' (tried to delegate stake)" (show b)
    else
      "invalid baker ID"
  Types.DuplicateSignKey k ->
    if verbose then
      printf "duplicate sign key '%s'" (show k)
    else
      "duplicate sign key"
  Types.DuplicateAggregationKey k ->
    if verbose then
      printf "duplicate aggregation key '%s'" (show k)
    else
      "duplicate aggregation key"
  Types.NotFromBakerAccount fromAccount bakerAccount ->
    printf "sender '%s' is not the baker's account ('%s' is)" (show fromAccount) (show bakerAccount)
  Types.NotFromSpecialAccount ->
    "sender is not allowed to perform this special type of transaction"

-- CONSENSUS

printConsensusStatus :: ConsensusStatusResult -> Printer
printConsensusStatus r =
  tell [ printf "Best block:                  %s" (show $ csrBestBlock r)
       , printf "Genesis block:               %s" (show $ csrGenesisBlock r)
       , printf "Genesis time:                %s" (show $ csrGenesisTime r)
       , printf "Slot duration:               %s" (show $ csrSlotDuration r)
       , printf "Epoch duration:              %s" (show $ csrEpochDuration r)
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
       , printf "Election difficulty: %f" (bprElectionDifficulty r) ]
  when includeBakers $
    case bprBakers r of
      [] ->
         tell [ "Bakers:              " ++ showNone ]
      bs -> do
        tell [ "Bakers:"
             , printf "                             Account                       Lottery power"
             , printf "        ----------------------------------------------------------------" ]
        tell $ f <$> bs
  where f b = printf "%6s: %s  %s" (show $ bpbrId b) (show $ bpbrAccount b) (showLotteryPower $ bpbrLotteryPower b)
        showLotteryPower lp = if 0 < lp && lp < 0.000001
                              then " <0.0001 %" :: String
                              else printf "%8.4f %%" (lp*100)
-- BLOCK

printBlockInfo :: Maybe BlockInfoResult -> Printer
printBlockInfo Nothing = tell [ printf "Block not found." ]
printBlockInfo (Just b) =
  tell [ printf "Hash:                       %s" (show $ birBlockHash b)
       , printf "Parent block:               %s" (show $ birBlockParent b)
       , printf "Last finalized block:       %s" (show $ birBlockLastFinalized b)
       , printf "Finalized:                  %s" (showYesNo $ birFinalized b)
       , printf "Receive time:               %s" (showTimeFormatted $ birBlockReceiveTime b)
       , printf "Arrive time:                %s" (showTimeFormatted $ birBlockArriveTime b)
       , printf "Slot:                       %s" (show $ birBlockSlot b)
       , printf "Slot time:                  %s" (showTimeFormatted $ birBlockSlotTime b)
       , printf "Baker:                      %s" (showMaybe show $ birBlockBaker b)
       , printf "Transaction count:          %d" (birTransactionCount b)
       , printf "Transaction energy cost:    %s" (showNrg $ birTransactionEnergyCost b)
       , printf "Transactions size:          %d" (birTransactionsSize b) ]


-- ID LAYER

parseDescription :: AE.Value -> AE.Parser (String, String, String)
parseDescription = AE.withObject "Description" $ \obj -> do
  name <- obj AE..: "name"
  url <- obj AE..: "url"
  description <- obj AE..: "description"
  return (name, url, description)

printIdentityProviders :: [AE.Value] -> Printer
printIdentityProviders vals = do
  tell [ printf "Identity providers"
       , printf "------------------" ]
  tell $ concatMap printSingleIdentityProvider vals
 where parseResponse :: AE.Value -> AE.Parser (Word32, (String, String, String))
       parseResponse = AE.withObject "IpInfo" $ \obj -> do
         ipId <- obj AE..: "ipIdentity"
         descriptionVal <- obj AE..: "arDescription"
         description <- parseDescription descriptionVal
         return (ipId, description)
       printSingleIdentityProvider val =
         let mresult = AE.parse parseResponse val in
           case mresult of
             AE.Success (ident, (name, url, description)) ->
               [ printf "Identifier:     %s" ident
               , printf "Description:    NAME %s" name
               , printf "                URL %s" url
               , printf "                %s" description ]
             AE.Error e -> [ "Error encountered while parsing IpInfo: " ++ show e ]

printAnonymityRevokers :: [AE.Value] -> Printer
printAnonymityRevokers vals = do
  tell [ printf "Anonymity revokers"
       , printf "------------------" ]
  tell $ concatMap printSingleAnonymityRevoker vals
 where parseResponse :: AE.Value -> AE.Parser (Word32, (String, String, String))
       parseResponse = AE.withObject "IpInfo" $ \obj -> do
         ipId <- obj AE..: "arIdentity"
         descriptionVal <- obj AE..: "arDescription"
         description <- parseDescription descriptionVal
         return (ipId, description)
       printSingleAnonymityRevoker val =
         let mresult = AE.parse parseResponse val in
           case mresult of
             AE.Success (ident, (name, url, description)) ->
               [ printf "Identifier:     %s" ident
               , printf "Description:    NAME %s" name
               , printf "                URL %s" url
               , printf "                %s" description ]
             AE.Error e -> [ "Error encountered while parsing ArInfo: " ++ show e ]

-- AMOUNT AND ENERGY

-- |Standardized method of displaying an amount as GTU.
showGtu :: Types.Amount -> String
showGtu = printf "%.4f GTU" . (/amountPerGtu) . fromIntegral
  where amountPerGtu = 10000 :: Double

-- |Standardized method of displaying energy as NRG.
showNrg :: Types.Energy -> String
showNrg = printf "%s NRG" . show

-- UTIL

-- |Produce a string fragment of the address and, if available, name of the account.
showNamedAddress :: NamedAddress -> String
showNamedAddress NamedAddress { naName = name, naAddr = a } =
  let addr = printf "'%s'" (show a)
  in case name of
    Nothing -> addr
    Just n -> printf "%s (%s)" addr n

-- |Standardized method of displaying optional values.
showMaybe :: (a -> String) -> Maybe a -> String
showMaybe = maybe showNone

-- |Standardized method of displaying optional time values.
showMaybeUTC :: Maybe UTCTime -> String
showMaybeUTC = showMaybe showTimeFormatted

-- |Standardized method of displaying EMA/EMSD values.
showEm :: String -> String -> String
showEm = printf "%s (EMA), %s (EMSD)"

-- |Standardized method of displaying EMA/EMSD number of seconds.
showEmSeconds :: Double -> Double -> String
showEmSeconds a d = showEm (showSeconds a) (showSeconds d)

-- |Standardized method of displaying optional EMA/EMSD number of seconds.
showMaybeEmSeconds :: Maybe Double -> Maybe Double -> String
showMaybeEmSeconds a d = case (a, d) of
                    (Just a', Just d') -> showEmSeconds a' d'
                    _ -> showNone

-- |Standardized method of displaying a number of seconds.
showSeconds :: Double -> String
showSeconds s = printf "%5d ms" (round $ 1000*s :: Int)

-- |Print a line for each entry in the provided map using the provided print function.
printMap :: ((k, v) -> String) -> [(k, v)] -> Printer
printMap s m = forM_ m $ \(k, v) -> tell [s (k, v)]

-- |Standardized method of displaying a boolean as "yes" or "no"
-- (for True and False, respectively).
showYesNo :: Bool -> String
showYesNo = bool "no" "yes"

-- |Convert a map to an assoc list sorted on the key.
toSortedList :: Ord k => HM.HashMap k v -> [(k, v)]
toSortedList = sortOn fst . HM.toList
