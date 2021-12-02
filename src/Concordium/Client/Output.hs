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
import Concordium.Client.Utils(durationToText)
import Concordium.Client.Types.TransactionStatus
import Concordium.Common.Version
import Concordium.ID.Parameters
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Crypto.EncryptedTransfers as Enc
import qualified Concordium.Wasm as Wasm
import qualified Concordium.Common.Time as Time
import qualified Concordium.Types.Queries as Queries

import Control.Monad.Writer
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import Data.Bool
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Lazy as BSL
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.List (foldl', intercalate, nub, sortOn)
import Data.Maybe
import Data.Word (Word64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Lens.Micro.Platform
import Text.Printf
import Codec.CBOR.Read
import Codec.CBOR.JSON
import Codec.CBOR.Decoding (decodeString)

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
        printMap (showEntry showVal) $ Map.toAscList m
    showEntry :: (v -> String) -> (Text, v) -> String
    showEntry showVal (n, a) = [i|    #{n} -> #{a'}|] :: String
      where a' = showVal a

printSelectedKeyConfig :: EncryptedSigningData -> Printer
printSelectedKeyConfig encSignData = do
  tell [ [i|Account configuration:|]
       , [i|- Names:   #{nameListOrNone}|]
       , [i|- Address: #{naAddr $ esdAddress encSignData}|] ]
  printKeys $ esdKeys encSignData
  where printKeys m =
          if null m then
            tell [ "- Credentials keys:    " ++ showNone ]
          else do
            tell [ "- Credentials keys:" ]
            forM_ (Map.toList m) $ (\(cidx, km) -> do
              tell [ "   - Keys for credential with index " ++ show cidx]
              printMap showEntry $ Map.toAscList km)
        showEntry (n, kp) =
          printf "      %s: %s" (show n) (showAccountKeyPair kp)

        nameListOrNone = case naNames $ esdAddress encSignData of
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

showRevealedAttributes :: Map.Map IDTypes.AttributeTag IDTypes.AttributeValue -> String
showRevealedAttributes as =
  if null as then
    "none"
  else
    intercalate ", " $ map showAttr $ Map.toList as
  where
    showTag t = case Map.lookup t IDTypes.invMapping of
                  Nothing -> printf "<%s>" (show t)
                  Just k -> Text.unpack k
    showAttr (t, IDTypes.AttributeValue v) = printf "%s=%s" (showTag t) (show v)

printAccountInfo :: (Types.Epoch -> UTCTime) -> NamedAddress -> AccountInfoResult -> Verbose -> Bool -> Maybe (ElgamalSecretKey, GlobalContext) -> Printer
printAccountInfo epochsToUTC addr a verbose showEncrypted mEncKey= do
  tell ([ [i|Local names:            #{showNameList $ naNames addr}|]
        , [i|Address:                #{naAddr addr}|]
        , [i|Balance:                #{showCcd $ airAmount a}|]
        ] ++
       case totalRelease $ airReleaseSchedule a of
         0 -> []
         tot -> (printf "Release schedule:       total %s" (showCcd tot)) :
               (map (\ReleaseScheduleItem{..} -> printf "   %s:               %s scheduled by the transactions: %s."
                                                (showTimeFormatted (Time.timestampToUTCTime rsiTimestamp))
                                                (showCcd rsiAmount)
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
                 printer x = let decoded = decoder x in "(" ++ showCcd decoded ++ ") " ++ showEncryptedAmount x
                 showableSelfDecryptedAmount = printer (Types._selfAmount $ airEncryptedAmount a)
                 incomingAmountsList = Types.getIncomingAmountsList $ airEncryptedAmount a
                 showableIncomingAmountsList =  printer <$>  incomingAmountsList
             showEncryptedBalance showableIncomingAmountsList showableSelfDecryptedAmount
    else return ()



  case airBaker a of
    Nothing -> tell ["Baker: none"]
    Just bk -> do
      let bkid = [i|Baker: \##{show . aibiIdentity . abirAccountBakerInfo $ bk}|]
          stkstr = [i| - Staked amount: #{showCcd . abirStakedAmount $ bk}|]
      case abirBakerPendingChange bk of
        NoChange -> tell [ bkid
                         , stkstr ]
        RemoveBaker e -> tell [ [i|#{bkid} to be removed at epoch #{e} (#{epochsToUTC e})|]
                              , stkstr ]
        ReduceStake n e -> tell [ bkid
                                , [i|#{stkstr} to be updated to #{showCcd n} at epoch #{e} (#{epochsToUTC e})|] ]
      tell [[i| - Restake earnings: #{showYesNo . abirStakeEarnings $ bk}|]]

  tell [ "" ]

  if Map.null $ airCredentials a then
    tell ["Credentials: " ++ showNone]
  else do
    tell ["Credentials:"]
    if verbose then
      tell $ [showPrettyJSON (airCredentials a)]
    else
      forM_ (Map.toList (airCredentials a)) printVersionedCred

-- |Print a versioned credential. This only prints the credential value, and not the
-- associated version.
printVersionedCred :: (IDTypes.CredentialIndex, (Versioned IDTypes.AccountCredential)) -> Printer
printVersionedCred (ci, vc) = printCred ci (vValue vc)

-- |Print the registration id, expiry date, and revealed attributes of a credential.
printCred :: IDTypes.CredentialIndex -> IDTypes.AccountCredential -> Printer
printCred ci c =
  tell [ printf "* %s:" (show $ IDTypes.credId c)
       , printf "  - Index: %s" (show ci)
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
  where namedAccs = map (\addr -> NamedAddress {naAddr = addr, naNames = unwrapMaybeList $ Map.lookup addr nameMapInv}) accs
        nameMapInv = invertHashMapAndCombine nameMap
        header = [ "Accounts:"
                 , "                 Account Address                     Account Names"
                 , "--------------------------------------------------------------------" ]
        format NamedAddress{..} = [i|#{naAddr}   #{showNameList naNames}|]

-- |Print a list of modules along with optional names.
printModuleList :: ModuleNameMap -> [Types.ModuleRef] -> Printer
printModuleList nameMap refs = printNameList "Modules" header format namedModRefs
  where namedModRefs = map (\ref -> NamedModuleRef {nmrRef = ref, nmrNames = unwrapMaybeList $ Map.lookup ref nameMapInv}) refs
        nameMapInv = invertHashMapAndCombine nameMap
        header = [ "Modules:"
                 , "                        Module Reference                           Module Names"
                 , "---------------------------------------------------------------------------------" ]
        format NamedModuleRef{..} = [i|#{nmrRef}   #{showNameList nmrNames}|]

-- |Print a list of contracts along with optional names.
printContractList :: ContractNameMap -> [Types.ContractAddress] -> Printer
printContractList nameMap addrs = printNameList "Contracts" header format namedContrAddrs
  where namedContrAddrs = map (\addr -> NamedContractAddress {ncaAddr = addr, ncaNames = unwrapMaybeList $ Map.lookup addr nameMapInv}) addrs
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
       , [i|Balance:         #{showCcd ciAmount}|]
       , [i|State size:      #{ciSize} bytes|]]
  tell state
  tell [ [i|Methods:|]]
  tellMethods
  where
    contractName = CI.contractNameFromInitName ciName
    owner = showNamedAddress namedOwner
    state = case ciState of
      CI.JustBytes bs -> ["State(raw):", [i|    #{BS.unpack bs}|]]
      CI.WithSchema _ Nothing bs -> ["No schema type was found for the state.\nState(raw):", [i|    #{BS.unpack bs}|]]
      CI.WithSchema _ (Just state') _ -> ["State:", indentBy 4 $ showPrettyJSON state']
    tellMethods = case ciState of
      CI.JustBytes _ -> tell $ toDashedList methodNames
      CI.WithSchema CS.ModuleSchema{..} _ _ -> case Map.lookup contractName contractSchemas of
        Nothing -> tell $ toDashedList methodNames
        Just CS.ContractSchema{receiveSigs=rcvSigs} -> tell . toDashedList . map (tryAppendSignature rcvSigs) $ methodNames
      where methodNames = map methodNameFromReceiveName ciMethods
            toDashedList = map (\x -> [i| - #{x}|])

            tryAppendSignature :: Map.Map Text CS.SchemaType -> Text -> Text
            tryAppendSignature rcvSigs rcvName = case Map.lookup rcvName rcvSigs of
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
-- If the init or receive signatures for a contract exist in the schema, they are also printed.
-- Otherwise, it just prints the method names.
-- If the schema contains signatures for init or receive methods not in the module, a warning is displayed.
printModuleInspectInfo :: NamedModuleRef
                        -> Maybe CS.ModuleSchema
                        -> [Text] -- ^ Exported function names.
                        -> Printer
printModuleInspectInfo namedModRef moduleSchema exportedFuncNames = do
  tell [ [i|Module:    #{showNamedModuleRef namedModRef}|]
       , [i|Contracts:|]]
  tell $ showMap mapWithSchemas
  tell $ showWarnings invalidSchemas

  where

    -- Flow of the function:
    --  Create a map of contractNames -> ContractSigs from the exported function names.
    --  Insert receiveNames from the exported functions into the aforementioned map.
    --  Insert the schemas into the map.
    --    If extraneous schemas are found, these are collected.
    -- Display the map and possibly a warning with the extraneous schemas.

    (mapWithSchemas, invalidSchemas) = case moduleSchema of
      Nothing -> (mapFromExports, [])
      Just CS.ModuleSchema{..} -> addSchemas mapFromExports contractSchemas

    mapFromExports :: Map.Map Text ContractSigs
    mapFromExports = insertReceiveNames funcNames $ mkMapFromInits funcNames
      where funcNames = toFuncNames exportedFuncNames
            toFuncNames :: [Text] -> [FuncName]
            toFuncNames [] = []
            toFuncNames (name : remaining)
             | Wasm.isValidInitName name = (InitFuncName . Text.drop 5 $ name) : toFuncNames remaining
             | Wasm.isValidReceiveName name = case Text.findIndex (== '.') name of
                                            Nothing -> toFuncNames remaining -- Cannot happen, as a valid receive name always has a dot.
                                            Just idx -> let (cname, fname) = Text.splitAt idx name
                                                            fnameWithoutDot = Text.tail fname
                                                        in ReceiveFuncName cname fnameWithoutDot : toFuncNames remaining
             | otherwise = toFuncNames remaining -- Ignore other types of exported functions.

            mkMapFromInits :: [FuncName] -> Map.Map Text ContractSigs
            mkMapFromInits = Map.fromList . foldr mkInitTuples []
              where mkInitTuples x xs = case x of
                      InitFuncName name -> (name, ContractSigs { csInitSig = Nothing, csReceiveSigs = Map.empty }) : xs
                      ReceiveFuncName _ _ -> xs

            insertReceiveNames :: [FuncName] -> Map.Map Text ContractSigs -> Map.Map Text ContractSigs
            insertReceiveNames [] sigMap = sigMap
            insertReceiveNames (InitFuncName _:remaining) sigMap = insertReceiveNames remaining sigMap
            insertReceiveNames (ReceiveFuncName cname fname:remaining) sigMap = case Map.lookup cname sigMap of
              Nothing -> insertReceiveNames remaining sigMap -- This should never happen, as we validate modules before they are put on chain.
              Just ContractSigs {..} ->
                let updatedCsReceiveSigs = Map.insert fname Nothing csReceiveSigs
                    sigMap' = Map.insert cname (ContractSigs {csInitSig = csInitSig, csReceiveSigs = updatedCsReceiveSigs}) sigMap
                in insertReceiveNames remaining sigMap'

    addSchemas :: Map.Map Text ContractSigs -> Map.Map Text ContractSchema -> (Map.Map Text ContractSigs, [FuncName])
    addSchemas mSigs mSchema = go mSigs [] (Map.toList mSchema)
      where
            go :: Map.Map Text ContractSigs -> [FuncName] -> [(Text, ContractSchema)] -> (Map.Map Text ContractSigs, [FuncName])
            go sigMap errors [] = (sigMap, errors)
            go sigMap errors ((cname, ContractSchema {..}):remaining) =
              case Map.lookup cname sigMap of
                Nothing -> let receiveErrors = map (ReceiveFuncName cname) . Map.keys $ receiveSigs
                               errors' = InitFuncName cname : receiveErrors ++ errors
                           in go sigMap errors' remaining -- Schema has init signature for a contract not in the module.
                Just ContractSigs {..} ->
                  let (updatedCsReceiveSigs, receiveErrors) = updateReceiveSigs cname csReceiveSigs [] (Map.toList receiveSigs)
                      sigMap' = Map.insert cname (ContractSigs {csInitSig = initSig, csReceiveSigs = updatedCsReceiveSigs}) sigMap
                  in go sigMap' (receiveErrors ++ errors) remaining

            updateReceiveSigs :: Text -> Map.Map Text (Maybe SchemaType) -> [FuncName]
                              -> [(Text, SchemaType)] -> (Map.Map Text (Maybe SchemaType), [FuncName])
            updateReceiveSigs _ sigMap errors [] = (sigMap, errors)
            updateReceiveSigs cname sigMap errors ((fname, schema):remaining) =
              if Map.member fname sigMap
              then updateReceiveSigs cname (Map.insert fname (Just schema) sigMap) errors remaining
              else -- Schema has signature for method not in the module.
                updateReceiveSigs cname sigMap (ReceiveFuncName cname fname:errors) remaining

    -- |Show all the contract init and receive functions including optional signatures from the schema.
    -- Example:
    --  - contract-no-methods-no-schema
    --  - contract-no-methods
    --     {
    --         "this-is": "<String>",
    --         "the-init-sig": "<UInt32>"
    --     }
    --  - contract
    --     ["<String>", "<Int32>"]
    --     - func
    --         ["<AccountAddress>"]
    --     - func-no-schema
    showMap :: Map.Map Text ContractSigs -> [String]
    showMap = go . sortOn fst . Map.toList
      where go [] = []
            go ((cname, ContractSigs{..}):remaining) = [[i| - #{cname}|]]
                                                        ++ showInit csInitSig
                                                        ++ showReceives (sortOn fst . Map.toList $ csReceiveSigs)
                                                        ++ go remaining

            showInit :: Maybe SchemaType -> [String]
            showInit Nothing = []
            showInit (Just initSig) = [indentBy 4 $ showPrettyJSON initSig]

            showReceives :: [(Text, Maybe SchemaType)] -> [String]
            showReceives [] = []
            showReceives ((fname, mSchema):remaining) =
              case mSchema of
                Nothing -> indentBy 4 [i| - #{fname}|] : showReceives remaining
                Just schema -> [indentBy 4 [i| - #{fname}|], indentBy 8 $ showPrettyJSON schema] ++ showReceives remaining

    showWarnings :: [FuncName] -> [String]
    showWarnings [] = []
    showWarnings xs =
      "\nWarning: The schema contained signatures for the following methods that do not exist in the module:" : map showFuncName xs
      where
        showFuncName = \case
                InitFuncName cname -> [i| - init_#{cname}|]
                ReceiveFuncName cname fname -> [i| - #{cname}.#{fname}|]

-- |Used for inspecting smart contract modules.
-- Is similar to ContractSchema, but has a Maybe Value in the receiveSigs and does not have a state field.
data ContractSigs = ContractSigs
  { csInitSig :: Maybe SchemaType
  , csReceiveSigs :: Map.Map Text (Maybe SchemaType)
  }

-- |Indents each line in a string by the number of spaces specified.
indentBy :: Int -> String -> String
indentBy spaces = intercalate "\n" . map (replicate spaces ' ' <>) . lines

-- |Invert a map and combine the new values in a list.
invertHashMapAndCombine :: (Ord v) => Map.Map k v -> Map.Map v [k]
invertHashMapAndCombine = Map.fromListWith (++) . map (\(k, v) -> (v, [k])) . Map.toList

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
  case Map.toAscList (tsrResults status) of
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
showCost gtu nrg = printf "%s (%s)" (showCcd gtu) (showNrg nrg)

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
    in Just $ printf "baker %s added, staking %s CCD. %s" (showBaker ebaBakerId ebaAccount) (Types.amountToString ebaStake) restakeString
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
  Types.CredentialsUpdated{..} ->
    verboseOrNothing $ [i|credentials on account #{cuAccount} have been updated.\nCredentials #{cuRemovedCredIds} have been removed, and credentials #{cuNewCredIds} have been added.\nThe new account threshold is #{cuNewThreshold}.|]

  Types.CredentialKeysUpdated cid -> verboseOrNothing $ printf "credential keys updated for credential with credId %s" (show cid)
  Types.NewEncryptedAmount{..} -> verboseOrNothing $ printf "shielded amount received on account '%s' with index '%s'" (show neaAccount) (show neaNewIndex)
  Types.EncryptedAmountsRemoved{..} -> verboseOrNothing $ printf "shielded amounts removed on account '%s' up to index '%s' with a resulting self shielded amount of '%s'" (show earAccount) (show earUpToIndex) (show earNewAmount)
  Types.AmountAddedByDecryption{..} -> verboseOrNothing $ printf "transferred '%s' tokens from the shielded balance to the public balance on account '%s'" (show aabdAmount) (show aabdAccount)
  Types.EncryptedSelfAmountAdded{..} -> verboseOrNothing $ printf "transferred '%s' tokens from the public balance to the shielded balance on account '%s' with a resulting self shielded balance of '%s'" (show eaaAmount) (show eaaAccount) (show eaaNewAmount)
  Types.UpdateEnqueued{..} ->
    verboseOrNothing $ printf "Enqueued chain update, effective at %s:\n%s" (showTimeFormatted (timeFromTransactionExpiryTime ueEffectiveTime)) (show uePayload)
  Types.TransferredWithSchedule{..} ->
    verboseOrNothing $ printf "Sent transfer with schedule %s" (intercalate ", " . map (\(a, b) -> showTimeFormatted (Time.timestampToUTCTime a) ++ ": " ++ showCcd b) $ etwsAmount)
  Types.DataRegistered{ } ->
    verboseOrNothing [i|Registered data on chain.|]
  Types.TransferMemo{..} ->
    let (Types.Memo bss) = tmMemo
        invalidCBOR = printf "Could not decode memo as valid CBOR. The hex value of the memo is %s." $ show tmMemo
        bsl = BSL.fromStrict $ BSS.fromShort bss
        str = case deserialiseFromBytes decodeString bsl of -- Try to decode the memo as a CBOR string
          Left _ -> json -- if not possible, try to decode as JSON
          Right (rest, x) -> if rest == BSL.empty then
                               Text.unpack x
                             else
                               invalidCBOR
        json = case deserialiseFromBytes (decodeValue False) bsl of
          Left _ -> invalidCBOR -- if not possible, the memo is not written in valid CBOR
          Right (rest, x) -> if rest == BSL.empty then
                               showPrettyJSON x
                             else 
                               invalidCBOR
    in Just $ printf "Transfer memo:\n%s" str
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
  Types.AmountTooLarge a amount ->
    if verbose then
      printf "account or contract '%s' does not have enough funds to transfer %s tokens" (show a) (show amount)
    else
      "insufficient funds"
  Types.SerializationFailure ->
    "serialization failed"
  Types.OutOfEnergy ->
    "not enough energy"
  Types.RejectedInit{..} ->
    [i|contract init logic failed with code #{rejectReason}|]
  Types.RejectedReceive{..} ->
    let (contractName, funcName) = Wasm.contractAndFunctionName receiveName
    in [i|'#{funcName}' in '#{contractName}' at #{showCompactPrettyJSON contractAddress} failed with code #{rejectReason}|]
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
  Types.KeyIndexAlreadyInUse ->
    "encountered a key index that is already in use"
  Types.InvalidCredentialKeySignThreshold ->
    "signature threshold exceeds the number of keys of the credential"
  Types.InvalidAccountThreshold ->
    "account threshold exceeds the number of credentials"
  Types.InvalidEncryptedAmountTransferProof ->
    "the proof for the shielded transfer doesn't validate"
  Types.EncryptedAmountSelfTransfer acc ->
    printf "attempted to make a shielded transfer to the same account '%s'" (show acc)
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
  Types.NonExistentCredentialID -> "credential ID does not exist on the account"
  Types.InvalidCredentials -> "one or more of the credentials is not valid"
  Types.DuplicateCredIDs cids -> [i|credential registration ids #{cids} are duplicate|]
  Types.NonExistentCredIDs cids -> [i|credential registration ids #{cids} do not exist|]
  Types.RemoveFirstCredential -> [i|attempt to remove the first credential of the account|]
  Types.CredentialHolderDidNotSign -> [i|credential holder did not sign the credential key update|]
  Types.StakeUnderMinimumThresholdForBaking -> "the desired stake is under the minimum threshold for baking"
  Types.NotAllowedMultipleCredentials -> "the account is not allowed to have multiple credentials"
  Types.NotAllowedToReceiveEncrypted -> "the account is not allowed to receive shielded transfers"
  Types.NotAllowedToHandleEncrypted -> "the account is not allowed handle shielded amounts"

-- CONSENSUS

printConsensusStatus :: Queries.ConsensusStatus -> Printer
printConsensusStatus r =
  tell [ printf "Best block:                  %s" (show $ Queries.csBestBlock r)
       , printf "Genesis block:               %s" (show $ Queries.csGenesisBlock r)
       , printf "Genesis time:                %s" (show $ Queries.csGenesisTime r)
       , printf "Slot duration:               %s" (showDuration $ Time.durationMillis $ Queries.csSlotDuration r)
       , printf "Epoch duration:              %s" (showDuration $ Time.durationMillis $ Queries.csEpochDuration r)
       , printf "Last finalized block:        %s" (show $ Queries.csLastFinalizedBlock r)
       , printf "Best block height:           %s" (show $ Queries.csBestBlockHeight r)
       , printf "Last finalized block height: %s" (show $ Queries.csLastFinalizedBlockHeight r)
       , printf "Blocks received count:       %s" (show $ Queries.csBlocksReceivedCount r)
       , printf "Block last received time:    %s" (showMaybeUTC $ Queries.csBlockLastReceivedTime r)
       , printf "Block receive latency:       %s" (showEmSeconds (Queries.csBlockReceiveLatencyEMA r) (Queries.csBlockReceiveLatencyEMSD r))
       , printf "Block receive period:        %s" (showMaybeEmSeconds (Queries.csBlockReceivePeriodEMA r) (Queries.csBlockReceivePeriodEMSD r))
       , printf "Blocks verified count:       %s" (show $ Queries.csBlocksVerifiedCount r)
       , printf "Block last arrived time:     %s" (showMaybeUTC $ Queries.csBlockLastArrivedTime r)
       , printf "Block arrive latency:        %s" (showEmSeconds (Queries.csBlockArriveLatencyEMA r) (Queries.csBlockArriveLatencyEMSD r))
       , printf "Block arrive period:         %s" (showMaybeEmSeconds (Queries.csBlockArrivePeriodEMA r) (Queries.csBlockArrivePeriodEMSD r))
       , printf "Transactions per block:      %s" (showEm (printf "%8.3f" $ Queries.csTransactionsPerBlockEMA r) (printf "%8.3f" $ Queries.csTransactionsPerBlockEMSD r))
       , printf "Finalization count:          %s" (show $ Queries.csFinalizationCount r)
       , printf "Last finalized time:         %s" (showMaybeUTC $ Queries.csLastFinalizedTime r)
       , printf "Finalization period:         %s" (showMaybeEmSeconds (Queries.csFinalizationPeriodEMA r) (Queries.csFinalizationPeriodEMSD r))
       , printf "Protocol version:            %s" (show $ Queries.csProtocolVersion r)
       , printf "Genesis index:               %s" (show $ Queries.csGenesisIndex r)
       , printf "Current era genesis block:   %s" (show $ Queries.csCurrentEraGenesisBlock r)
       , printf "Current era genesis time:    %s" (show $ Queries.csCurrentEraGenesisTime r)]

printBirkParameters :: Bool -> BirkParametersResult -> Map.Map IDTypes.AccountAddress Text -> Printer
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
          accountName bkr = fromMaybe " " $ Map.lookup bkr addrmap


-- BLOCK

printBlockInfo :: Maybe Queries.BlockInfo -> Printer
printBlockInfo Nothing = tell [ printf "Block not found." ]
printBlockInfo (Just b) =
  tell [ printf "Hash:                       %s" (show $ Queries.biBlockHash b)
       , printf "Parent block:               %s" (show $ Queries.biBlockParent b)
       , printf "Last finalized block:       %s" (show $ Queries.biBlockLastFinalized b)
       , printf "Finalized:                  %s" (showYesNo $ Queries.biFinalized b)
       , printf "Receive time:               %s" (showTimeFormatted $ Queries.biBlockReceiveTime b)
       , printf "Arrive time:                %s" (showTimeFormatted $ Queries.biBlockArriveTime b)
       , printf "Slot:                       %s" (show $ Queries.biBlockSlot b)
       , printf "Slot time:                  %s" (showTimeFormatted $ Queries.biBlockSlotTime b)
       , printf "Height:                     %s" (show $ Queries.biBlockHeight b)
       , printf "Height since last genesis:  %s" (show $ Queries.biEraBlockHeight b)
       , printf "Genesis index:              %s" (show $ Queries.biGenesisIndex b)
       , printf "Baker:                      %s" (showMaybe show $ Queries.biBlockBaker b)
       , printf "Transaction count:          %d" (Queries.biTransactionCount b)
       , printf "Transaction energy cost:    %s" (showNrg $ Queries.biTransactionEnergyCost b)
       , printf "Transactions size:          %d" (Queries.biTransactionsSize b) ]


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

-- |Standardized method of displaying an amount as CCD.
showCcd :: Types.Amount -> String
showCcd = printf "%s CCD" . Types.amountToString

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

-- |Standardized method of displaying a number of milliseconds in a nice way, e.g. "2h 15m 3s".
showDuration :: Word64 -> String
showDuration = Text.unpack . durationToText

-- |Print a line for each entry in the provided map using the provided print function.
printMap :: ((k, v) -> String) -> [(k, v)] -> Printer
printMap s m = forM_ m $ \(k, v) -> tell [s (k, v)]

-- |Standardized method of displaying a boolean as "yes" or "no"
-- (for True and False, respectively).
showYesNo :: Bool -> String
showYesNo = bool "no" "yes"

-- |Unwrap a list from within `Maybe`. `Nothing` becomes an empty list.
unwrapMaybeList :: Maybe [a] -> [a]
unwrapMaybeList = concat
