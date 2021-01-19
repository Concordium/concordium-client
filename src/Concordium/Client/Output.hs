{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Output where

import Concordium.Client.Cli
import Concordium.Client.Commands (Verbose)
import Concordium.Client.Config
import Concordium.Client.Parse
import Concordium.Client.Types.Account
import Concordium.Client.Types.Contract.Info as CI
import Concordium.Client.Types.Contract.Schema as CS
import Concordium.Client.Types.TransactionStatus
import Concordium.Common.Version
import Concordium.ID.Parameters
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Crypto.EncryptedTransfers as Enc
import qualified Concordium.Wasm as Wasm

import Control.Monad.Writer
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import Data.Bool
import qualified Data.ByteString as BS
import Data.Functor
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Lens.Micro.Platform
import Text.Printf

-- PRINTER

-- |Specialized writer for producing a list of lines.
type Printer = Writer [String] ()

-- |Print the lines of a printer.
runPrinter :: (MonadIO m) => Printer -> m ()
runPrinter = liftIO . mapM_ putStrLn . execWriter

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
  tell [ "Base configuration:"
       , [i|- Verbose:            #{showYesNo $ bcVerbose cfg}|]
       , [i|- Account config dir: #{bcAccountCfgDir cfg}|]
       , [i|- Contract config dir: #{bcContractCfgDir cfg}|]]
  printNameMap "Account" show $ bcAccountNameMap cfg
  printNameMap "Contract" showCompactPrettyJSON $ bcContractNameMap cfg
  printNameMap "Module" show $ bcModuleNameMap cfg
  where
    printNameMap :: String -> (v -> String) -> NameMap v -> Printer
    printNameMap variantName showVal m =
      if null m then
        tell [[i|- #{variantName} name map:   #{showNone}|]]
      else do
        tell [[i|- #{variantName} name map:|]]
        printMap (showEntry showVal) $ toSortedList m
    showEntry :: (v -> String) -> (Text, v) -> String
    showEntry showVal (n, a) = [i|    #{n} -> #{a'}|] :: String
      where a' = showVal a

printAccountConfig :: AccountConfig -> Printer
printAccountConfig cfg = do
  tell [ [i|Account configuration:|]
       , [i|- Names:   #{nameListOrNone}|]
       , [i|- Address: #{naAddr $ acAddr cfg}|] ]
  printKeys $ acKeys cfg
  where printKeys m =
          if null m then
            tell [ "- Keys:    " ++ showNone ]
          else do
            tell [ "- Keys:" ]
            printMap showEntry $ toSortedList m
        showEntry (n, kp) =
          printf "    %s: %s" (show n) (showAccountKeyPair kp)

        nameListOrNone = case naNames $ acAddr cfg of
          [] -> showNone
          names -> showNameList names

printAccountConfigList :: [AccountConfig] -> Printer
printAccountConfigList cfgs =
  if null cfgs then
    tell [ "Account keys: " ++ showNone ]
  else do
    tell [ "Account keys:" ]
    forM_ cfgs $ \cfg -> do
      let keys = acKeys cfg
      if null keys then
        tell [ printf "- %s: %s" (namedAddress cfg) showNone]
      else do
        tell [ printf "- %s:" (namedAddress cfg)]
        -- NB: While we do not have proper account exports or dedicated commands to print
        -- the full account key map, this command should print the account key map in the
        -- JSON format that can be used for importing and "account add-keys".
        tell [ showPrettyJSON keys ]
    tell ["Encryption secret keys:" ]
    forM_ cfgs $ \cfg -> do
      tell [ printf "- %s: %s" (namedAddress cfg) (maybe showNone showPrettyJSON (acEncryptionKey cfg))]
  where namedAddress cfg = showNamedAddress $ acAddr cfg

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
                  Just k -> Text.unpack k
    showAttr (t, IDTypes.AttributeValue v) = printf "%s=%s" (showTag t) (show v)

printAccountInfo :: (Types.Epoch -> UTCTime) -> NamedAddress -> AccountInfoResult -> Verbose -> Bool -> Maybe (ElgamalSecretKey, GlobalContext) -> Printer
printAccountInfo epochsToUTC addr a verbose showEncrypted mEncKey= do
  tell ([ [i|Local names:            #{showNameList $ naNames addr}|]
        , [i|Address:                #{naAddr addr}|]
        , [i|Balance:                #{showGtu $ airAmount a}|]
        ] ++
       case totalRelease $ airReleaseSchedule a of
         0 -> []
         tot -> (printf "Release schedule:       total %s" (showGtu tot)) :
               (map (\ReleaseScheduleItem{..} -> printf "   %s:               %s scheduled by the transactions: %s."
                                                (showTimeFormatted (Types.timestampToUTCTime rsiTimestamp))
                                                (showGtu rsiAmount)
                                                (intercalate ", " $ map show rsiTransactions))
                 (releaseSchedule $ airReleaseSchedule a))
       ++ [ printf "Nonce:                  %s" (show $ airNonce a)
          , printf "Encryption public key:  %s" (show $ airEncryptionKey a)
          , "" ])

  if showEncrypted then
    let
      -- since encryption keys are quite long we only print 20 characters by default
      showEncryptedAmount = if verbose then show else \v -> take 20 (show v) ++ "..."
      showEncryptedBalance amms self = do
        let (_, balances) = foldl' (\(idx, strings) v -> (idx + 1, strings <> [printf "    %s: %s" (show idx) v]))
                                   (Types._startIndex $ airEncryptedAmount a, [])
                                   amms
        tell ["Encrypted balance:"]
        tell $ case balances of
                 [] -> ["  Incoming amounts: []"]
                 _ -> ["  Incoming amounts:"] <> balances
        tell [printf "  Self balance: %s" self]
        tell [ "" ]
    in
      case mEncKey of
        Nothing ->
          let incomingAmounts = showEncryptedAmount <$> Types.getIncomingAmountsList (airEncryptedAmount a)
              selfAmount = showEncryptedAmount $ airEncryptedAmount a ^. Types.selfAmount
          in showEncryptedBalance incomingAmounts selfAmount
        Just (encKey, globalContext) -> do
             let table = Enc.computeTable globalContext (2^(16::Int))
                 decoder = Enc.decryptAmount table encKey
                 printer x = let decoded = decoder x in "(" ++ showGtu decoded ++ ") " ++ showEncryptedAmount x
                 showableSelfDecryptedAmount = printer (Types._selfAmount $ airEncryptedAmount a)
                 incomingAmountsList = Types.getIncomingAmountsList $ airEncryptedAmount a
                 showableIncomingAmountsList =  printer <$>  incomingAmountsList
             showEncryptedBalance showableIncomingAmountsList showableSelfDecryptedAmount
    else return ()



  case airBaker a of
    Nothing -> tell ["Baker: none"]
    Just bk -> do
      let bkid = [i|Baker: ##{show . aibiIdentity . abirAccountBakerInfo $ bk}|]
          stkstr = [i| - Staked amount: #{showGtu . abirStakedAmount $ bk}|]
      case abirBakerPendingChange bk of
        NoChange -> tell [ bkid
                         , stkstr ]
        RemoveBaker e -> tell [ [i|#{bkid} to be removed at epoch #{e} (#{epochsToUTC e})|]
                              , stkstr ]
        ReduceStake n e -> tell [ bkid
                                , [i|#{stkstr} to be updated to #{showGtu n} at epoch #{e} (#{epochsToUTC e})|] ]
      tell [[i| - Restake earnings: #{showYesNo . abirStakeEarnings $ bk}|]]

  tell [ "" ]

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
printVersionedCred :: (Versioned IDTypes.AccountCredential) -> Printer
printVersionedCred vc = printCred (vValue vc)

-- |Print the registration id, expiry date, and revealed attributes of a credential.
printCred :: IDTypes.AccountCredential -> Printer
printCred c =
  tell [ printf "* %s:" (show $ IDTypes.regId c)
       , printf "  - Expiration: %s" expiry
       , printf "  - Type: %s" credType
       , printf "  - Revealed attributes: %s" (showRevealedAttributes attrs) ]
  where
    p = IDTypes.policy c
    e = show $ IDTypes.validTo c
    credType :: String =
      case IDTypes.credentialType c of
        IDTypes.Initial -> "initial"
        IDTypes.Normal -> "normal"
    attrs = IDTypes.pItems p
    expiry = case parseCredExpiry e of
               Nothing -> printf "invalid expiration time '%s'" e
               Just t -> showTimeYearMonth t

-- |Print a list of accounts along with optional names.
printAccountList :: AccountNameMap -> [IDTypes.AccountAddress] -> Printer
printAccountList nameMap accs = printNameList "Accounts" header format namedAccs
  where namedAccs = map (\addr -> NamedAddress {naAddr = addr, naNames = unwrapMaybeList $ HM.lookup addr nameMapInv}) accs
        nameMapInv = invertHashMapAndCombine nameMap
        header = [ "Accounts:"
                 , "                 Account Address                     Account Names"
                 , "--------------------------------------------------------------------" ]
        format NamedAddress{..} = [i|#{naAddr}   #{showNameList naNames}|]

-- |Print a list of modules along with optional names.
printModuleList :: ModuleNameMap -> [Types.ModuleRef] -> Printer
printModuleList nameMap refs = printNameList "Modules" header format namedModRefs
  where namedModRefs = map (\ref -> NamedModuleRef {nmrRef = ref, nmrNames = unwrapMaybeList $ HM.lookup ref nameMapInv}) refs
        nameMapInv = invertHashMapAndCombine nameMap
        header = [ "Modules:"
                 , "                        Module Reference                           Module Names"
                 , "---------------------------------------------------------------------------------" ]
        format NamedModuleRef{..} = [i|#{nmrRef}   #{showNameList nmrNames}|]

-- |Print a list of contracts along with optional names.
printContractList :: ContractNameMap -> [Types.ContractAddress] -> Printer
printContractList nameMap addrs = printNameList "Contracts" header format namedContrAddrs
  where namedContrAddrs = map (\addr -> NamedContractAddress {ncaAddr = addr, ncaNames = unwrapMaybeList $ HM.lookup addr nameMapInv}) addrs
        nameMapInv = invertHashMapAndCombine nameMap
        header = [ "Contracts:"
                 , "    Contract Address       Contract Names"
                 , "-------------------------------------------" ]
        format NamedContractAddress{..} = [i|#{addr}   #{showNameList ncaNames}|]
          where addr = showCompactPrettyJSON ncaAddr

-- |Print a header and a list of named items in the provided format.
printNameList :: String -> [String] -> (a -> String) -> [a] -> Printer
printNameList variantName header format xs =
  case xs of
    [] -> tell [[i|#{variantName}: #{showNone}|]]
    _  -> do
      -- TODO: Use a proper formatter tool.
      tell header
      tell $ map format xs

-- |Print contract info using a provided namedAddress and namedModRef.
-- Since ContractInfo comes directly from the node, the names are not included and must
-- be provided separately.
printContractInfo :: CI.ContractInfo -> NamedAddress -> NamedModuleRef -> Printer
printContractInfo CI.ContractInfo{..} namedOwner namedModRef = do
  tell [ [i|Contract:        #{contractName}|]
       , [i|Owner:           #{owner}|]
       , [i|ModuleReference: #{showNamedModuleRef namedModRef}|]
       , [i|Balance:         #{Types.amountToString ciAmount} GTU|]]
  tell state
  tell [ [i|Methods:|]]
  tellMethods
  where
    contractName = CI.contractNameFromInitName ciName
    owner = showNamedAddress namedOwner
    state = case ciState of
      CI.JustBytes bs -> ["State(raw):", [i|    #{BS.unpack bs}|]]
      CI.WithSchema _ (AE.Object obj) -> case HM.lookup "state" obj of
                                   Nothing -> stateErrorMsg
                                   Just state' -> ["State:", indentBy 4 $ showPrettyJSON state']
      CI.WithSchema _ _ -> stateErrorMsg
    stateErrorMsg = ["Could not display contract state."]
    tellMethods = case ciState of
      CI.JustBytes _ -> tell $ toDashedList methodNames
      CI.WithSchema CS.ModuleSchema{..} _ -> case M.lookup contractName contractSchemas of
        Nothing -> tell $ toDashedList methodNames
        Just CS.ContractSchema{receiveSigs=rcvSigs} -> tell . toDashedList . map (tryAppendSignature rcvSigs) $ methodNames
      where methodNames = map methodNameFromReceiveName ciMethods
            toDashedList = map (\x -> [i| - #{x}|])

            tryAppendSignature :: M.Map Text CS.SchemaType -> Text -> Text
            tryAppendSignature rcvSigs rcvName = case M.lookup rcvName rcvSigs of
              Nothing -> rcvName
              Just schemaType -> rcvName <> "\n" <> Text.pack (indentBy 4 $ showPrettyJSON schemaType)

    -- |Get a method name from a Receive name, i.e. extracting the text and removing the "<contractName>." prefix.
    -- If the receiveName does not have the prefix, it simply returns the extracted text.
    methodNameFromReceiveName :: Wasm.ReceiveName -> Text
    methodNameFromReceiveName rcvName = case Text.split (=='.') receiveNameText of
      [_contrName, methodName] -> methodName
      _ -> receiveNameText
      where receiveNameText = Wasm.receiveName rcvName

-- |Print module inspect info, i.e., the named moduleRef and its included contracts.
-- Signatures for the contracts' init functions are also printed, if available in the schema.
printModuleInspectInfo :: NamedModuleRef -> CS.ModuleSchema -> Printer
printModuleInspectInfo namedModRef CS.ModuleSchema{..} = do
  tell [ [i|Module:    #{showNamedModuleRef namedModRef}|]
       , [i|Contracts:|]]
  -- TODO: Should also print contracts without a schema. They can be found by parsing the Wasm module.
  tell contracts

  where
    contracts = map showContract $ M.toList contractSchemas
    showContract (contractName, CS.ContractSchema{..}) = case initSig of
      Nothing -> [i| - #{contractName}|]
      Just initSig' -> [i| - #{contractName}\n#{indentBy 4 $ showPrettyJSON initSig'}|]

-- |Indents each line in a string by the number of spaces specified.
indentBy :: Int -> String -> String
indentBy spaces = intercalate "\n" . map (replicate spaces ' ' <>) . lines

-- |Invert a hashmap and combine the new values in a list.
invertHashMapAndCombine :: (Eq v, Hashable v) => HM.HashMap k v -> HM.HashMap v [k]
invertHashMapAndCombine = HM.fromListWith (++) . map (\(k, v) -> (v, [k])) . HM.toList

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
-- The non-verbose version is used by the transaction commands (through tailTransaction_)
-- where the input parameters have already been specified manually and repeated in a block
-- of text that they confirmed manually.
-- The verbose version is used by 'transaction status' and the non-trivial cases of the above
-- where there are multiple distinct outcomes.
showEvent :: Verbose -> Types.Event -> Maybe String
showEvent verbose = \case
  Types.ModuleDeployed ref->
    verboseOrNothing $ printf "module '%s' deployed" (show ref)
  Types.ContractInitialized{..} ->
    verboseOrNothing $ printf "initialized contract '%s' using init function '%s' from module '%s' with '%s' tokens"
                              (show ecAddress) (show ecInitName) (show ecRef) (show ecAmount)
  Types.Updated{..} ->
    verboseOrNothing $ printf "sent message to function '%s' with '%s' and '%s' tokens from %s to %s"
                              (show euReceiveName) (show euMessage) (show euAmount) (showAddress euInstigator) (showAddress $ Types.AddressContract euAddress)
  Types.Transferred{..} ->
    verboseOrNothing $ printf "transferred %s tokens from %s to %s" (show etAmount) (showAddress etFrom) (showAddress etTo)
  Types.AccountCreated addr ->
    verboseOrNothing $ printf "account '%s' created" (show addr)
  Types.CredentialDeployed{..} ->
    verboseOrNothing $ printf "credential with registration '%s' deployed onto account '%s'" (show ecdRegId) (show ecdAccount)
  Types.BakerAdded{..} ->
    let restakeString :: String = if ebaRestakeEarnings then "Earnings are added to the stake." else "Earnings are not added to the stake."
    in Just $ printf "baker %s added, staking %s GTU. %s" (showBaker ebaBakerId ebaAccount) (Types.amountToString ebaStake) restakeString
  Types.BakerRemoved{..} ->
    verboseOrNothing $ printf "baker %s, removed" (showBaker ebrBakerId ebrAccount) (show ebrBakerId)
  Types.BakerStakeIncreased{..} ->
    Just $ printf "baker %s stake increased to %s" (showBaker ebsiBakerId ebsiAccount) (Types.amountToString ebsiNewStake)
  Types.BakerStakeDecreased{..} ->
    Just $ printf "baker %s stake decreased to %s" (showBaker ebsiBakerId ebsiAccount) (Types.amountToString ebsiNewStake)
  Types.BakerSetRestakeEarnings{..} ->
    verboseOrNothing $ printf "baker %s restake earnings %s" (showBaker ebsreBakerId ebsreAccount) (if ebsreRestakeEarnings then "set" :: String else "unset")
  Types.BakerKeysUpdated{..} ->
    verboseOrNothing $ printf "baker %s keys updated" (showBaker ebkuBakerId ebkuAccount)

  Types.AccountKeysUpdated -> verboseOrNothing $ "account keys updated"
  Types.AccountKeysAdded -> verboseOrNothing $ "account keys added"
  Types.AccountKeysRemoved -> verboseOrNothing $ "account keys removed"
  Types.AccountKeysSignThresholdUpdated -> verboseOrNothing $ "account signature threshold updated"
  Types.NewEncryptedAmount{..} -> verboseOrNothing $ printf "encrypted amount received on account '%s' with index '%s'" (show neaAccount) (show neaNewIndex)
  Types.EncryptedAmountsRemoved{..} -> verboseOrNothing $ printf "encrypted amounts removed on account '%s' up to index '%s' with a resulting self encrypted amount of '%s'" (show earAccount) (show earUpToIndex) (show earNewAmount)
  Types.AmountAddedByDecryption{..} -> verboseOrNothing $ printf "transferred '%s' tokens from the shielded balance to the public balance on account '%s'" (show aabdAmount) (show aabdAccount)
  Types.EncryptedSelfAmountAdded{..} -> verboseOrNothing $ printf "transferred '%s' tokens from the public balance to the shielded balance on account '%s' with a resulting self encrypted balance of '%s'" (show eaaAmount) (show eaaAccount) (show eaaNewAmount)
  Types.UpdateEnqueued{..} ->
    verboseOrNothing $ printf "Enqueued chain update, effective at %s:\n%s" (showTimeFormatted (timeFromTransactionExpiryTime ueEffectiveTime)) (show uePayload)
  Types.TransferredWithSchedule{..} ->
    verboseOrNothing $ printf "Sent transfer with schedule %s" (intercalate ", " . map (\(a, b) -> showTimeFormatted (Types.timestampToUTCTime a) ++ ": " ++ showGtu b) $ etwsAmount)
  where
    verboseOrNothing :: String -> Maybe String
    verboseOrNothing msg = if verbose then Just msg else Nothing

    showAddress :: Types.Address -> String
    showAddress = \case
      Types.AddressAccount a -> printf "account '%s'" (show a)
      Types.AddressContract a -> printf "contract '%s'" (show a)

    showBaker :: Types.BakerId -> Types.AccountAddress -> String
    showBaker bid addr = show addr ++ " (ID " ++ show bid ++ ")"


-- |Return string representation of reject reason.
-- If verbose is true, the string includes the details from the fields of the reason.
-- Otherwise, only the fields that are not known from the transaction request are included.
-- Currently this is only the baker address from NotFromBakerAccount.
-- The non-verbose version is used by the transaction commands (through tailTransaction_)
-- where the input parameters have already been specified manually and repeated in a block
-- of text that they confirmed manually.
-- The verbose version is used by 'transaction status' and the non-trivial cases of the above
-- where there are multiple distinct outcomes.
showRejectReason :: Verbose -> Types.RejectReason -> String
showRejectReason verbose = \case
  Types.ModuleNotWF ->
    "typechecking error"
  Types.ModuleHashAlreadyExists m ->
    if verbose then
      printf "module '%s' already exists" (show m)
    else
      "module already exists"
  Types.InvalidAccountReference a ->
    if verbose then
      printf "account '%s' does not exist" (show a)
    else
      "account does not exist"
  Types.InvalidInitMethod m name ->
    if verbose then
      printf "invalid init method '%s' of module '%s'" (show name) (show m)
    else
      "invalid init method"
  Types.InvalidReceiveMethod m name ->
    if verbose then
      printf "invalid receive method '%s' of module '%s'" (show name) (show m)
    else
      "invalid receive method"
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
  Types.RuntimeFailure ->
    "runtime failure"
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
  Types.DuplicateAggregationKey k ->
    if verbose then
      printf "duplicate aggregation key '%s'" (show k)
    else
      "duplicate aggregation key"
  Types.NonExistentAccountKey ->
    "encountered key index to which no key belongs"
  Types.KeyIndexAlreadyInUse ->
    "encountered a key index that is already in use"
  Types.InvalidAccountKeySignThreshold ->
    "signature threshold exceeds the number of keys"
  Types.InvalidEncryptedAmountTransferProof ->
    "the proof for the encrypted transfer doesn't validate"
  Types.EncryptedAmountSelfTransfer acc ->
    printf "attempted to make an encrypted transfer to the same account '%s'" (show acc)
  Types.InvalidTransferToPublicProof ->
    "the proof for the secret to public transfer doesn't validate"
  Types.InvalidIndexOnEncryptedTransfer ->
    "the provided index is below the start index or above `startIndex + length incomingAmounts`"
  Types.ZeroScheduledAmount -> "the total amount or some of the releases scheduled would be equal to zero"
  Types.NonIncreasingSchedule -> "the releases were not sorted on the timestamp"
  Types.FirstScheduledReleaseExpired -> "the first release has already expired"
  Types.ScheduledSelfTransfer acc ->
    printf "attempted to make an scheduled transfer to the same account '%s'" (show acc)
  Types.AlreadyABaker bid -> printf "already a registered baker with ID %s" (show bid)
  Types.NotABaker addr -> printf "attempt to remove a baker account %s that is not a baker" (show addr)
  Types.InsufficientBalanceForBakerStake -> "the balance on the account is insufficient to cover the desired stake"
  Types.BakerInCooldown -> "change could not be completed because the baker is in the cooldown period"


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

printBirkParameters :: Bool -> BirkParametersResult -> HM.HashMap IDTypes.AccountAddress Text -> Printer
printBirkParameters includeBakers r addrmap = do
  tell [ printf "Election nonce:      %s" (show $ bprElectionNonce r)
      ] --, printf "Election difficulty: %f" (Types.electionDifficulty $ bprElectionDifficulty r) ]
  when includeBakers $
    case bprBakers r of
      [] ->
         tell [ "Bakers:              " ++ showNone ]
      bakers -> do
        tell [ "Bakers:"
             , printf "                             Account                       Lottery power  Account Name"
             , printf "        ------------------------------------------------------------------------------" ]
        tell (map f bakers)
        where
          f b' = printf "%6s: %s  %s  %s" (show $ bpbrId b') (show $ bpbrAccount b') (showLotteryPower $ bpbrLotteryPower b') (accountName $ bpbrAccount b')
          showLotteryPower lp = if 0 < lp && lp < 0.000001
                                then " <0.0001 %" :: String
                                else printf "%8.4f %%" (lp*100)
          accountName bkr = fromMaybe " " $ HM.lookup bkr addrmap


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
       , printf "Height:                     %s" (show $ birBlockHeight b)
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
 where parseResponse :: AE.Value -> AE.Parser (IDTypes.IdentityProviderIdentity, (String, String, String))
       parseResponse = AE.withObject "IpInfo" $ \obj -> do
         ipId <- obj AE..: "ipIdentity"
         descriptionVal <- obj AE..: "ipDescription"
         description <- parseDescription descriptionVal
         return (ipId, description)
       printSingleIdentityProvider val =
         let mresult = AE.parse parseResponse val in
           case mresult of
             AE.Success (ident, (name, url, description)) ->
               [ printf "Identifier:     %s" $ show ident
               , printf "Description:    NAME %s" name
               , printf "                URL %s" url
               , printf "                %s" description ]
             AE.Error e -> [ "Error encountered while parsing IpInfo: " ++ show e ]

printAnonymityRevokers :: [AE.Value] -> Printer
printAnonymityRevokers vals = do
  tell [ printf "Anonymity revokers"
       , printf "------------------" ]
  tell $ concatMap printSingleAnonymityRevoker vals
 where parseResponse :: AE.Value -> AE.Parser (IDTypes.ArIdentity, (String, String, String))
       parseResponse = AE.withObject "IpInfo" $ \obj -> do
         ipId <- obj AE..: "arIdentity"
         descriptionVal <- obj AE..: "arDescription"
         description <- parseDescription descriptionVal
         return (ipId, description)
       printSingleAnonymityRevoker val =
         let mresult = AE.parse parseResponse val in
           case mresult of
             AE.Success (ident, (name, url, description)) ->
               [ printf "Identifier:     %s" $ show ident
               , printf "Description:    NAME %s" name
               , printf "                URL %s" url
               , printf "                %s" description ]
             AE.Error e -> [ "Error encountered while parsing ArInfo: " ++ show e ]

-- AMOUNT AND ENERGY

-- |Standardized method of displaying an amount as GTU.
showGtu :: Types.Amount -> String
showGtu = printf "%s GTU" . Types.amountToString

-- |Standardized method of displaying energy as NRG.
showNrg :: Types.Energy -> String
showNrg = printf "%s NRG" . show

-- UTIL

-- |Produce a string fragment of the account address and, if available, a list of names for it.
showNamedAddress :: NamedAddress -> String
showNamedAddress NamedAddress {..} =
  case naNames of
    [] -> [i|'#{naAddr}'|]
    names -> [i|'#{naAddr}' (#{showNameList names})|]

-- |Produce a string fragment of the contract address and, if available, a list of names for it.
showNamedContractAddress :: NamedContractAddress -> String
showNamedContractAddress NamedContractAddress{..} =
  case ncaNames of
    [] -> ncaAddr'
    names -> [i|#{ncaAddr'} (#{showNameList names})|]
    where ncaAddr' = showCompactPrettyJSON ncaAddr

-- |Produce a string fragment of the moduleRef and, if available, a list of names for it.
showNamedModuleRef :: NamedModuleRef -> String
showNamedModuleRef NamedModuleRef {..} =
  case nmrNames of
    [] -> [i|'#{nmrRef}'|]
    names -> [i|'#{nmrRef}' (#{showNameList names})|]

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

-- |Unwrap a list from within `Maybe`. `Nothing` becomes an empty list.
unwrapMaybeList :: Maybe [a] -> [a]
unwrapMaybeList = concat
