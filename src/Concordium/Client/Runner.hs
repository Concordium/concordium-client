{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Concordium.Client.Runner
  ( process
  , getAccountNonce
  , getBestBlockHash
  , getLastFinalBlockHash
  , PeerData(..)
  , getPeerData
  , getNodeInfo
  , sendTransactionToBaker
  , getConsensusStatus
  , generateBakerKeys
  , getAccountInfo
  , getAccountInfoOrDie
  , getModuleSet
  , getStatusOfPeers
  , StatusOfPeers(..)
  , ClientMonad(..)
  , withClient
  , EnvData(..)
  , GrpcConfig
  , processTransaction_
  , awaitState
  -- * For importing support
  , decodeGenesisFormattedAccountExport
  , accountCfgsFromWalletExportAccounts
  , decodeMobileFormattedAccountExport
  -- * For adding baker support
  , startTransaction
  , encodeAndSignTransaction
  , tailTransaction_
  , getBlockItemHash
  , withClientJson
  , TransactionConfig(..)
  , getFromJson
  , getFromJson'
  -- other auxiliary functions
  , getParseCryptographicParameters
  ) where

import           Concordium.Client.Utils
import           Concordium.Client.Cli
import           Concordium.Client.Config
import           Concordium.Client.Commands          as COM
import           Concordium.Client.Export
import           Concordium.Client.GRPC
import           Concordium.Client.GRPC2
import           Concordium.Client.Output
import           Concordium.Client.Parse
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Account
import qualified Concordium.Client.Types.Contract.Info as CI
import qualified Concordium.Client.Types.Contract.Parameter as CP
import qualified Concordium.Client.Types.Contract.Schema as CS
import           Concordium.Client.Types.Transaction as CT
import           Concordium.Client.Types.TransactionStatus
import           Concordium.Common.Version
import qualified Concordium.Common.Time as Time
import qualified Concordium.Crypto.EncryptedTransfers as Enc
import qualified Concordium.Crypto.BlockSignature    as BlockSig
import qualified Concordium.Crypto.BlsSignature      as Bls
import qualified Concordium.Crypto.Proofs            as Proofs
import qualified Concordium.Crypto.SignatureScheme   as SigScheme
import qualified Concordium.Crypto.VRF               as VRF

import           Concordium.GRPC2
import qualified Concordium.Types.InvokeContract     as InvokeContract
import           Concordium.Types.UpdateQueues       as Types
import qualified Concordium.Types.Queries            as Queries
import qualified Concordium.Types.Updates            as Updates
import qualified Concordium.Types.Transactions       as Types
import qualified Concordium.Types.Accounts           as Types
import qualified Concordium.Types.Block              as Types
import           Concordium.Types.HashableTo
import           Concordium.Types.Parameters
import qualified Concordium.Cost as Cost
import qualified Concordium.Types.Execution          as Types
import qualified Concordium.Types                    as Types
import qualified Concordium.ID.Types                 as ID
import           Concordium.ID.Parameters
import qualified Concordium.Utils.Encryption         as Password
import qualified Concordium.Wasm                     as Wasm
import           Proto.ConcordiumP2pRpc
import qualified Proto.ConcordiumP2pRpc_Fields       as CF
import qualified Data.Char as Char

import           Control.Exception
import           Control.Monad.Fail
import           Control.Monad.Except
import           Control.Monad.Reader                hiding (fail)
import           Control.Monad.State.Strict
import           GHC.Generics
import           Data.IORef
import           Data.Foldable
import           Data.Aeson                          as AE
import qualified Data.Aeson.Encode.Pretty            as AE
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.ByteString.Lazy.Char8          as BSL8
import qualified Data.ByteString.Short               as BS (toShort)
import qualified Data.ByteString.Short               as BSS
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import qualified Data.List                           as L
import qualified Data.Serialize                      as S
import qualified Data.Set                            as Set
import           Data.String
import           Data.String.Interpolate (i, iii)
import           Data.Text(Text)
import qualified Data.Tuple                          as Tuple
import qualified Data.Text                           as Text
import qualified Data.Text.Encoding                  as Text
import qualified Data.Vector                         as Vec
import           Data.Word
import           Lens.Micro.Platform
import           Network.GRPC.Client.Helpers
import           Prelude                             hiding (log, fail, unlines)
import           System.IO
import           System.Exit
import           System.Directory
import           System.FilePath
import qualified System.Console.ANSI                 as ANSI
import           Text.Printf
import           Text.Read (readMaybe, readEither)
import Data.Time.Clock (addUTCTime, getCurrentTime, UTCTime)
import Data.Ratio
import Codec.CBOR.Write
import Codec.CBOR.Encoding
import Codec.CBOR.JSON
import Control.Arrow (Arrow(second))
import Concordium.Client.Types.Contract.Info (instanceInfoToContractInfo)
import Network.GRPC.HTTP2.Types (GRPCStatusCode(NOT_FOUND))

-- |Establish a new connection to the backend and run the provided computation.
-- Close a connection after completion of the computation. Establishing a
-- connection is expensive, and thus if multiple RPC calls are going to be made
-- they should be made in the context of the same 'withClient' so they reuse it.
withClient :: Backend -> ClientMonad IO a -> IO a
withClient bkend comp = do
  let config = GrpcConfig (COM.grpcHost bkend) (COM.grpcPort bkend) (COM.grpcAuthenticationToken bkend) (COM.grpcTarget bkend) (COM.grpcRetryNum bkend) Nothing (COM.grpcUseTls bkend)
  runExceptT (mkGrpcClient config Nothing) >>= \case
      Left err -> logFatal ["Cannot establish connection to the node: " ++ show err]
      Right client -> do
        let body = evalStateT (runExceptT ((runReaderT . _runClientMonad) comp $! client)) Map.empty >>= \case
              Left err -> fail (show err)
              Right x -> return x
        let closeOrFail Nothing = return ()
            closeOrFail (Just ref) =
              runExceptT (close ref) >>= \case
                Left err -> logFatal ["Error closing connection: " ++ show err]
                Right () -> return ()
        let closeConnection = closeOrFail =<< (fmap (fmap snd) . readIORef $ grpc client)
        finally body closeConnection

withClientJson :: (FromJSON a) => Backend -> ClientMonad IO (Either String Value) -> IO a
withClientJson b c = withClient b c >>= getFromJson

-- |Helper function for parsing JSON Value or fail if the value is missing or cannot be converted correctly.
-- The parameter has the same type as the one returned by e.g. eitherDecode or processJSON,
-- which many of the GRPC commands use.
getFromJson :: (MonadIO m, FromJSON a) => Either String Value -> m a
getFromJson = getFromJsonAndHandleError onError
  where onError val err = logFatal ["cannot convert '" ++ show val ++ "': " ++ err]

getFromJson' :: (MonadIO m, FromJSON a) => GRPCResult Value -> m a
getFromJson' = getFromJson . fmap grpcResponseVal

-- |Helper function for parsing JSON Value, logFatal if the Either is Left,
-- and use the provided function to handle Error in fromJSON.
getFromJsonAndHandleError :: (MonadIO m, FromJSON a) =>
                         (Value -> String -> m a) -- ^ Takes the JSON being converted and the err string from (Error err) if fromJSON fails.
                         -> Either String Value
                         -> m a
getFromJsonAndHandleError handleError r = do
  s <- case r of
         Left err -> logFatal [printf "I/O error: %s" err]
         Right v -> return v
  case fromJSON s of
    Error err -> handleError s err
    Success v -> return v

-- |Look up account from the provided name or address.
-- Fail if the address cannot be found.
getAccountAddressArg :: AccountNameMap -> Text -> IO NamedAddress
getAccountAddressArg m account = do
  case getAccountAddress m account of
    Left err -> logFatal [err]
    Right v -> return v

-- |Process CLI command.
process :: COM.Options -> IO ()
process Options{optsCmd = command, optsBackend = backend, optsConfigDir = cfgDir, optsVerbose = verbose} = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering

  case command of
    LegacyCmd c -> processLegacyCmd c backend
    ConfigCmd c -> processConfigCmd c cfgDir verbose
    TransactionCmd c -> processTransactionCmd c cfgDir verbose backend
    AccountCmd c -> processAccountCmd c cfgDir verbose backend
    ModuleCmd c -> processModuleCmd c cfgDir verbose backend
    ContractCmd c -> processContractCmd c cfgDir verbose backend
    ConsensusCmd c -> processConsensusCmd c cfgDir verbose backend
    BlockCmd c -> processBlockCmd c verbose backend
    BakerCmd c -> processBakerCmd c cfgDir verbose backend
    DelegatorCmd c -> processDelegatorCmd c cfgDir verbose backend
    IdentityCmd c -> processIdentityCmd c backend

-- |Process a 'config ...' command.
processConfigCmd :: ConfigCmd -> Maybe FilePath -> Verbose ->  IO ()
processConfigCmd action baseCfgDir verbose =
  case action of
    ConfigInit -> void $ initBaseConfig baseCfgDir verbose
    ConfigShow -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      runPrinter $ printBaseConfig baseCfg
      putStrLn ""
      accCfgs <- getAllAccountConfigs baseCfg
      runPrinter $ printAccountConfigList accCfgs
    ConfigBackupExport fileName -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      pwd <- askPassword "Enter password for encryption of backup (leave blank for no encryption): "
      allAccounts <- getAllAccountConfigs baseCfg
      let configBackup = ConfigBackup { cbAccounts = allAccounts
                                      , cbContractNameMap = bcContractNameMap baseCfg
                                      , cbModuleNameMap = bcModuleNameMap baseCfg
                                      }
      exportedConfigBackup <- case Password.getPassword pwd of
        "" -> configExport configBackup Nothing
        _ -> configExport configBackup (Just pwd)
      void $ handleWriteFile BS.writeFile PromptBeforeOverwrite verbose fileName exportedConfigBackup

    ConfigBackupImport fileName skipExistingAccounts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      ciphertext <- handleReadFile BS.readFile fileName
      configBackup <- configImport ciphertext (askPassword "The backup file is password protected. Enter password: ")
      case configBackup of
        Right ConfigBackup{..} -> do
          void $ importConfigBackup verbose baseCfg skipExistingAccounts (cbAccounts, cbContractNameMap, cbModuleNameMap)
        Left err -> logFatal [[i|Failed to import Config Backup, #{err}|]]

    ConfigAccountCmd c -> case c of
      ConfigAccountName addr name -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        checkedAddr <-
          case ID.addressFromText addr of
            Left err -> logFatal [[i|cannot parse #{addr} as an address: #{err}|]]
            Right a -> return a
        nameAdded <- liftIO $ addAccountNameAndWrite verbose baseCfg name checkedAddr
        logSuccess [[i|Account reference #{addr} was successfully named '#{nameAdded}'|]]

      ConfigAccountRemove account -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        -- look up the name/address and check if account is initialized:
        (_, accountConfig) <- getAccountConfig (Just account) baseCfg Nothing Nothing Nothing AssumeInitialized
        let accAddr = naAddr . acAddr $ accountConfig
        let accNames = findAllNamesFor (bcAccountNameMap baseCfg) accAddr
        let nameAddr = NamedAddress {naAddr = accAddr, naNames = accNames}

        let descriptor = [i|the account #{showNamedAddress nameAddr}|]

        logWarn [descriptor ++ " will be removed and can NOT be recovered"]

        updateConfirmed <- askConfirmation $ Just "confirm that you want to remove the account"

        when updateConfirmed $ do
          logInfo[ descriptor ++ " will be removed"]
          void $ removeAccountConfig baseCfg nameAddr

      ConfigAccountImport file name importFormat skipExisting -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        fileExists <- doesFileExist file
        unless fileExists $ logFatal [printf "the given file '%s' does not exist" file]

        -- NB: This also checks whether the name is valid if provided.
        accCfgs <- loadAccountImportFile importFormat file name

        -- fold over all accounts adding them to the config, starting from the
        -- base config. Checks for duplicates in the names mapping and prompts
        -- the user to give a new name
        (_, skipped) <- foldM addAccountToBaseConfigWithNamePrompts (baseCfg, 0::Int) accCfgs

        when (skipExisting && (skipped > 0)) $ logInfo [printf "`%d` account[s] automatically skipped. To overwrite these keys, re-import file without the skip-existing flag, or import them individually with --name" skipped ]

        where
          -- Adds an account to the BaseConfig, prompting the user for a new
          -- non-colliding name if the account is named and the name already
          -- exists in the name map.
          -- If skip-existing flag is set, count how many accounts were skipped
          addAccountToBaseConfigWithNamePrompts (baseCfg, skipped) accCfg = do
            (bcfg, _, t) <- initAccountConfig baseCfg (acAddr accCfg) True skipExisting
            when t $ writeAccountKeys bcfg accCfg verbose
            if skipExisting && (not t) then
              return (bcfg, skipped + 1)
            else
              return (bcfg, skipped)

      ConfigAccountAddKeys addr keysFile -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        keyMapInput <- getKeyMapInput keysFile
        (baseCfg', accCfg) <- getAccountConfigFromAddr addr baseCfg

        let keyMapCurrent = acKeys accCfg

        let keyMapNew = flip Map.mapWithKey keyMapInput $ \cidx km -> case Map.lookup cidx keyMapCurrent of
              Nothing -> km
              Just kmCurrent -> Map.difference km kmCurrent
        -- let keyMapNew = Map.difference keyMapInput keyMapCurrent
        let credDuplicates = Map.intersection keyMapCurrent keyMapInput

        let keyDuplicates = flip Map.mapWithKey credDuplicates $ \cidx _ -> case (Map.lookup cidx keyMapCurrent, Map.lookup cidx keyMapInput) of
              (Just kmCurrent, Just kmInput) -> Map.intersection kmCurrent kmInput
              _ -> Map.empty -- will never happen

        unless (Map.null credDuplicates) $ forM_ (Map.toList keyDuplicates) $ \(cidx, km) -> do
          unless (Map.null km) $ logWarn [ "the keys for credential "++ show cidx ++" with indices "
                  ++ showMapIdxs km
                  ++ " cannot be added because they already exist",
                  "Use 'concordium-client config account update-keys' if you want to update them."]

        -- Only write account keys if any non-duplicated keys are added
        case Map.null keyMapNew of
          True -> logInfo ["no keys were added"]
          False -> do
            forM_ (Map.toList keyMapNew) $ \(cidx, km) -> do
              unless (Map.null km) $ logInfo ["the keys for credential "++ show cidx ++" with indices "
                      ++ showMapIdxs km
                      ++ " will be added to account " ++ Text.unpack addr]
            let accCfg' = accCfg { acKeys = keyMapNew }
            writeAccountKeys baseCfg' accCfg' verbose

      ConfigAccountUpdateKeys addr keysFile -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        keyMapInput <- getKeyMapInput keysFile
        (baseCfg', accCfg) <- getAccountConfigFromAddr addr baseCfg

        let keyMapCurrent = acKeys accCfg
        -- let keyMapNew = Map.difference keyMapInput keyMapCurrent
        -- let keyMapDuplicates = Map.intersection keyMapCurrent keyMapInput
        let keyMapNew = flip Map.mapWithKey keyMapInput $ \cidx km -> case Map.lookup cidx keyMapCurrent of
              Nothing -> km
              Just kmCurrent -> Map.difference km kmCurrent
        let credDuplicates = Map.intersection keyMapCurrent keyMapInput

        let keyDuplicates = let f cidx _ =
                                 case (Map.lookup cidx keyMapCurrent, Map.lookup cidx keyMapInput) of
                                   (Just kmCurrent, Just kmInput) -> Map.intersection kmInput kmCurrent
                                   _ -> Map.empty -- if the credential exists in one but not the other then it won't be updated.
                            in Map.filter (not . Map.null) . Map.mapWithKey f $ credDuplicates

        unless (Map.null keyMapNew) $ forM_ (Map.toList keyMapNew) $ \(cidx, km) -> do
          unless (Map.null km) $ logWarn
                              [ "the keys for credential "++ show cidx ++" with indices "
                                ++ showMapIdxs km
                                ++ " can not be updated because they do not match existing keys",
                                "Use 'concordium-client config account add-keys' if you want to add them."]

        case Map.null keyDuplicates of
          True -> logInfo ["no keys were updated"]
          False -> do
            forM_ (Map.toList keyDuplicates) $ \(cidx, km) -> do
              unless (Map.null km) $ logWarn [ "the keys for credential "++ show cidx ++" with indices "
                      ++ showMapIdxs km
                      ++ " will be updated and can NOT be recovered"]

            updateConfirmed <- askConfirmation $ Just "confirm that you want to update the keys"

            when updateConfirmed $ do
              forM_ (Map.toList keyDuplicates) $ \(cidx, km) -> do
                unless (Map.null km) $ logInfo [ "the keys for credential "++ show cidx ++" with indices "
                          ++ showMapIdxs km
                          ++ " will be updated on account " ++ Text.unpack addr]
              let accCfg' = accCfg { acKeys = keyDuplicates }
              writeAccountKeys baseCfg' accCfg' verbose

      ConfigAccountChangeKeyPassword name cidx keyIndex -> do
        logWarn [printf "Re-encrypting keys under a new password permanently overwrites the previous encrypted keys"
                , "This is a destructive operation and cannot be undone"]

        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        (baseCfg', accountCfg) <- getAccountConfig (Just name) baseCfg Nothing Nothing Nothing AssumeInitialized
        let keyMap = acKeys accountCfg
        let ckeys = Map.lookup cidx keyMap
        case ckeys of
          Nothing -> logError [printf [i|No Credential found at Credential index #{cidx} for account '#{name}'|]]
          Just enckeys -> do
            let encKey = Map.lookup keyIndex enckeys
            case encKey of
              Nothing -> logError [printf [i|No key found with index #{keyIndex} for Credential index #{cidx} for account '#{name}'|]]
              Just encKey' -> do
                decPwd <- askPassword "Enter current encrypted key password"
                plain <- runExceptT (decryptAccountKeyPair decPwd keyIndex encKey')
                case plain of
                  Left err -> logError [printf err]
                  Right plainKey -> do
                    createpwdresult <- createPasswordInteractive (Just "re-encrypt key under")
                    case createpwdresult of
                      Left err -> logError [printf err]
                      Right encPwd -> do
                        encKey2 <- encryptAccountKeyPair encPwd plainKey
                        let newKeys = Map.insert cidx (Map.insert keyIndex encKey2 enckeys) keyMap
                        let accCfg' = accountCfg { acKeys = newKeys }
                        writeAccountKeys baseCfg' accCfg' verbose
                        logInfo [printf [i|Key with Credential Index #{cidx}, and key index #{keyIndex} for account '#{name}' has been re-encrypted under the new password|]]

      ConfigAccountRemoveKeys addr cidx idxs -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        (_, accCfg) <- getAccountConfigFromAddr addr baseCfg

        let idxsInput = Set.fromList idxs
        let cKeys = Map.lookup cidx $ acKeys accCfg

        case cKeys of
          Nothing -> do logWarn
                              ["No credential found with index  "
                               ++ show cidx]
          Just keys -> do
            let idxsCurrent = Map.keysSet keys

            let idxsToRemove = Set.intersection idxsCurrent idxsInput
            let idxsNotFound = Set.difference idxsInput idxsToRemove

            unless (Set.null idxsNotFound) $ logWarn
                                  ["keys (for credential "++show cidx++") with indices "
                                  ++ showSetIdxs idxsNotFound
                                  ++ " do not exist and can therefore not be removed"]

            case Set.null idxsToRemove of
              True -> logInfo ["no keys were removed"]
              False -> do
                logWarn [ "the keys (for credential "++show cidx++") with indices "
                          ++ showSetIdxs idxsToRemove
                          ++ " will be removed and can NOT be recovered"]

                updateConfirmed <- askConfirmation $ Just "confirm that you want to remove the keys"

                when updateConfirmed $ do
                  logInfo [ "the keys (for credential "++show cidx++") with indices "
                            ++ showSetIdxs idxsToRemove
                            ++ " will be removed from account " ++ Text.unpack addr]
                  removeAccountKeys baseCfg accCfg cidx (Set.toList idxsToRemove) verbose

      ConfigAccountRemoveName name -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        let nameMap = bcAccountNameMap baseCfg
        case Map.lookup name nameMap of
          Nothing -> logFatal [[i|the name '#{name}' is not in use|]]
          Just currentAddr -> do
            logInfo [[i|removing mapping from '#{name}' to account address '#{currentAddr}'|]]
            void $ removeAccountNameAndWrite baseCfg name verbose

  where showMapIdxs = showIdxs . Map.keys
        showSetIdxs = showIdxs . Set.toList
        showIdxs = L.intercalate ", " . map show . L.sort

        getKeyMapInput :: FilePath -> IO EncryptedAccountKeyMap
        getKeyMapInput keysFile = AE.eitherDecodeFileStrict keysFile `withLogFatalIO` ("cannot decode keys: " ++)

        getAccountConfigFromAddr :: Text -> BaseConfig -> IO (BaseConfig, AccountConfig)
        getAccountConfigFromAddr addr baseCfg = getAccountConfig (Just addr) baseCfg Nothing Nothing Nothing AutoInit

-- |Read and parse a file exported from either genesis data or mobile wallet.
-- The format specifier tells which format to expect.
-- If the format is "mobile", the user is prompted for a password which is used to decrypt
-- the exported data. This may result in multiple named accounts. If a name is provided,
-- only the account with that name is being selected for import.
-- The "genesis" format is not encrypted and only contains a single account which is not named.
-- If a name is provided in this case, this will become the account name.
loadAccountImportFile :: AccountExportFormat -> FilePath -> Maybe Text -> IO [AccountConfig]
loadAccountImportFile format file name = do
  contents <- handleReadFile BS.readFile file
  case format of
    FormatMobile -> do
      let pwd = askPassword "Enter encryption password: "
      (accCfgs, environment) <- decodeMobileFormattedAccountExport contents name pwd `withLogFatalIO` ("cannot import accounts: " ++)
      let accountsMessage :: Text = if length accCfgs >= 2 then "accounts" else "account"
      logInfo [[i|loaded the following #{accountsMessage} from the #{environment} chain:|]]
      forM_ accCfgs $ \AccountConfig{acAddr=NamedAddress{..}} -> logInfo [[i|- #{naAddr} #{showNameList naNames}|]]
      logInfo ["all signing keys have been encrypted with the password used for this import."
              ,"Note that accounts are not transferable between different chains, e.g., from testnet to mainnet or vice-versa."
              ]
      putStrLn ""

      return accCfgs
    FormatGenesis -> do
     pwd <- createPasswordInteractive (Just "encrypt all signing keys with") `withLogFatalIO` id
     accCfg <- decodeGenesisFormattedAccountExport contents name pwd `withLogFatalIO` ("cannot import account: " ++)
     return [accCfg]

-- Read a transaction memo as either
-- - a CBOR encoded string,
-- - CBOR encoded json (from a file), or
-- - raw bytes (from a file). 
-- Warn user if protocol version is 1.
checkAndGetMemo :: MemoInput -> Types.ProtocolVersion -> IO Types.Memo
checkAndGetMemo memo pv = do
  when (pv == Types.P1) $ logWarn ["Protocol version 1 does not support transaction memos."]
  bss <- BS.toShort <$> case memo of
    MemoString memoString -> do
      return $ toStrictByteString $ encodeString memoString
    MemoJSON memoFile -> do
      source <- handleReadFile BSL.readFile memoFile
      case AE.eitherDecode source of
        Left err -> fail $ "Error decoding JSON: " ++ err
        Right value -> do 
          return $ toStrictByteString $ encodeValue value
    MemoRaw memoFile -> handleReadFile BS.readFile memoFile
  case Types.memoFromBSS bss of
    Left err -> do
      logError [[i|Failed parsing memo: #{err} Transaction will fail.|]]
      override <- askConfirmation (Just "Proceed anyway")
      unless override exitFailure
      return $ Types.Memo bss
    Right m -> return m


-- | Returns contract info with schemas associated with an event.
-- Returns @Nothing@ if the event provided is not one of
--   - Concordium.Types.ContractInitialized
--   - Concordium.Types.Updated
--   - Concordium.Types.Interrupted
-- or fails if the contract could not be retrieved.
-- Optionally takes a path to a schema file to be parsed and returned.
-- Optionally takes a blockhash of the block in which the contract
-- contract module containing the schema resides, or defaults to using
-- the best blockhash. The schema from the file will take precedence
-- over an embedded schema in the module.
getContractInfoWithSchemas :: (MonadIO m)
  => Maybe FilePath -- ^ Path pointing to a schema file.
  -> BlockHashInput -- ^ The block to retrieve the contract info from.
  -> Types.Event -- ^ The event for which the contract info will be retrieved.
  -> ClientMonad m (Maybe CI.ContractInfo)
getContractInfoWithSchemas schemaFile blockHash ev = do
  -- Get contract address.
  let cAM = case ev of
              Types.ContractInitialized{..} -> Just ecAddress
              Types.Updated{..} -> Just euAddress
              Types.Interrupted{..} -> Just iAddress
              _ -> Nothing
  -- Get module schema.
  case cAM of
    Just ca -> do
      contrInfo <- getContractInfo (NamedContractAddress ca []) blockHash
      let namedModRef = NamedModuleRef { nmrRef = CI.ciSourceModule contrInfo, nmrNames = [] }
      schemaM <- getSchemaFromFileOrModule schemaFile namedModRef blockHash
      case schemaM of
        Nothing -> return $ Just contrInfo
        Just schema -> CI.addSchemaData contrInfo schema
    _ -> return Nothing

-- |Get `ContractInfo` for all events in all blocks in which a transaction is present.
-- Returns a map from blockhashes of blocks in which the transaction is present to the
-- events of the transaction in that block. Each event appears in a pair with an optional
-- `ContractInfo` value containing contract schema info associated with the event of the
-- transaction in that block, if present.
-- Optionally takes a path to a schema file to be parsed. If a schema is contained in the
-- file, it will take precedence over any schemas that may be embedded in the module and
-- will therefore be present in the `ContractInfo` for all events.
getTxContractInfoWithSchemas :: (MonadIO m)
  => Maybe FilePath -- ^ Path pointing to a schema file.
  -> TransactionStatusResult -- ^ The transaction result for which the contract info will be retrieved.
  -> ClientMonad m (Map.Map Types.BlockHash [(Types.Event, Maybe CI.ContractInfo)])
getTxContractInfoWithSchemas schemaFile status = do
  -- Which blocks should be used in the ContractInfo queries?
  let bhEvents = [ (bh, evsE) | (bh, Right evsE) <- extractFromTsr' getEvents status ]
  -- Get event schemas for all blocks and events.
  bhToEv <- forM bhEvents $ \(bh, evs) -> do
    evToSt <- forM evs $ \ev -> do
      st <- getContractInfoWithSchemas schemaFile (Given bh) ev
      return (ev, st)
    return (bh, evToSt)
  return $ Map.fromList bhToEv
  where
    getEvents :: Types.TransactionSummary -> Either String [Types.Event]
    getEvents tSum = case Types.tsResult tSum of
            Types.TxSuccess {..} -> Right vrEvents
            Types.TxReject {..}  -> Left $ showRejectReason True vrRejectReason
    
    -- A different variant of extractFromTsr' which also takes into
    -- account MultipleBlocksUnambiguous and MultipleBlocksAmbiguous.
    extractFromTsr' :: (Types.TransactionSummary -> a) -> TransactionStatusResult -> [(Types.BlockHash, a)]
    extractFromTsr' f tsr =
      case parseTransactionBlockResult tsr of
          NoBlocks -> []
          SingleBlock bh ts -> [(bh, f ts)] 
          MultipleBlocksUnambiguous bhs ts -> map (, f ts) bhs
          MultipleBlocksAmbiguous bhts -> map (second f) bhts

-- |Process a 'transaction ...' command.
processTransactionCmd :: TransactionCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processTransactionCmd action baseCfgDir verbose backend =
  case action of
    TransactionSubmit fname intOpts -> do
      -- TODO Ensure that the "nonce" field is optional in the payload.
      source <- handleReadFile BSL.readFile fname

      -- TODO Print transaction details and ask for confirmation if (ioConfirm intOpts)

      withClient backend $ do
        tx <- processTransaction source defaultNetId
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction_ verbose hash
--          logSuccess [ "transaction successfully completed" ]

    TransactionDeployCredential fname intOpts -> do
      source <- handleReadFile BSL.readFile fname
      withClient backend $ do
        tx <- processCredential source defaultNetId
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction_ verbose hash
--          logSuccess [ "credential deployed successfully" ]

    TransactionStatus h schemaFile -> do
      hash <- case parseTransactionHash h of
        Nothing -> logFatal [printf "invalid transaction hash '%s'" h]
        Just hash -> return hash
      withClient backend $ do
        status <- queryTransactionStatus hash
        contrInfoWithSchemas <- getTxContractInfoWithSchemas schemaFile status 
        runPrinter $ printTransactionStatus status True $ Just contrInfoWithSchemas

      -- TODO This works but output doesn't make sense if transaction is already committed/finalized.
      --      It should skip already completed steps.
      -- withClient backend $ tailTransaction_ hash
    TransactionSendCcd receiver amount maybeMemo txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      receiverAddress <- getAccountAddressArg (bcAccountNameMap baseCfg) receiver

      withClient backend $ do
        cs <- getResponseValueOrDie =<< getConsensusInfoV2
        pl <- liftIO $ do
              res <- case maybeMemo of 
                Nothing -> return $ Types.Transfer (naAddr receiverAddress) amount
                Just memoInput -> do
                  memo <- checkAndGetMemo memoInput $ Queries.csProtocolVersion cs
                  return $ Types.TransferWithMemo (naAddr receiverAddress) memo amount 
              return $ Types.encodePayload res
        let nrgCost _ = return $ Just $ simpleTransferEnergyCost $ Types.payloadSize pl
        txCfg <- liftIO $ getTransactionCfg baseCfg txOpts nrgCost

        let ttxCfg = TransferTransactionConfig
                        { ttcTransactionCfg = txCfg
                        , ttcReceiver = receiverAddress
                        , ttcAmount = amount }
        when verbose $ liftIO $ do
          runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData txCfg
          putStrLn ""

        let intOpts = toInteractionOpts txOpts
        liftIO $ transferTransactionConfirm ttxCfg (ioConfirm intOpts)
        sendAndTailTransaction_ verbose txCfg pl intOpts

    TransactionSendWithSchedule receiver schedule maybeMemo txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      let realSchedule = case schedule of
                       Right val -> val
                       Left (total, interval, numIntervals, start) ->
                         let chunks = total `div` fromIntegral numIntervals
                             lastChunk = total `mod` fromIntegral numIntervals
                             diff = case interval of
                               COM.Minute -> 60 * 1000
                               COM.Hour -> 60 * 60 * 1000
                               COM.Day -> 24 * 60 * 60 * 1000
                               COM.Week -> 7 * 24 * 60 * 60 * 1000
                               COM.Month -> 30 * 24 * 60 * 60 * 1000
                               COM.Year -> 365 * 24 * 60 * 60 * 1000
                         in
                           zip (iterate (+ diff) start) (replicate (numIntervals - 1) chunks ++ [chunks + lastChunk])
      receiverAddress <- getAccountAddressArg (bcAccountNameMap baseCfg) receiver
      withClient backend $ do
        cs <- getResponseValueOrDie =<< getConsensusInfoV2
        pl <- liftIO $ do
              res <- case maybeMemo of 
                Nothing -> return $ Types.TransferWithSchedule (naAddr receiverAddress) realSchedule
                Just memoInput -> do
                  memo <- checkAndGetMemo memoInput $ Queries.csProtocolVersion cs
                  return $ Types.TransferWithScheduleAndMemo (naAddr receiverAddress) memo realSchedule
              return $ Types.encodePayload res
        let nrgCost _ = return $ Just $ transferWithScheduleEnergyCost (Types.payloadSize pl) (length realSchedule)
        txCfg <- liftIO $ getTransactionCfg baseCfg txOpts nrgCost
        let ttxCfg = TransferWithScheduleTransactionConfig
                        { twstcTransactionCfg = txCfg
                        , twstcReceiver = receiverAddress
                        , twstcSchedule = realSchedule }

        when verbose $ liftIO $ do
          runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData txCfg
          putStrLn ""
        -- Check that sending and receiving accounts are not the same
        let fromAddr = naAddr $ esdAddress ( tcEncryptedSigningData txCfg)
        let toAddr = naAddr $ twstcReceiver ttxCfg
        if fromAddr == toAddr
          then liftIO $ do 
            logWarn ["Scheduled transfers from an account to itself are not allowed."]
            logWarn ["Transaction Cancelled"]
          else do
            let intOpts = toInteractionOpts txOpts
            liftIO $ transferWithScheduleTransactionConfirm ttxCfg (ioConfirm intOpts)
            sendAndTailTransaction_ verbose txCfg pl intOpts

    TransactionEncryptedTransfer txOpts receiver amount index maybeMemo -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      accCfg <- getAccountCfgFromTxOpts baseCfg txOpts
      let senderAddr = esdAddress accCfg

      encryptedSecretKey <-
          case esdEncryptionKey accCfg of
            Nothing ->
              logFatal ["Missing account encryption secret key for account: " ++ show senderAddr]
            Just x -> return x
      secretKey <- decryptAccountEncryptionSecretKeyInteractive encryptedSecretKey `withLogFatalIO` ("Couldn't decrypt account encryption secret key: " ++)

      receiverAcc <- getAccountAddressArg (bcAccountNameMap baseCfg) receiver

      withClient backend $ do
        transferData <- getEncryptedAmountTransferData (naAddr senderAddr) receiverAcc amount index secretKey
        cs <- getResponseValueOrDie =<< getConsensusInfoV2
        payload <- liftIO $ do
                res <- case maybeMemo of 
                  Nothing -> return $ Types.EncryptedAmountTransfer (naAddr receiverAcc) transferData
                  Just memoInput -> do
                    memo <- checkAndGetMemo memoInput $ Queries.csProtocolVersion cs
                    return $ Types.EncryptedAmountTransferWithMemo (naAddr receiverAcc) memo transferData
                return $ Types.encodePayload res
        let nrgCost _ = return $ Just $ encryptedTransferEnergyCost $ Types.payloadSize payload
        txCfg <- liftIO (getTransactionCfg baseCfg txOpts nrgCost)

        let ettCfg = EncryptedTransferTransactionConfig{
          ettTransferData = transferData,
          ettTransactionCfg = txCfg,
          ettReceiver = receiverAcc,
          ettAmount = amount
        }
        liftIO $ when verbose $ do
          runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData txCfg
          putStrLn ""

        let intOpts = toInteractionOpts txOpts
        encryptedTransferTransactionConfirm ettCfg (ioConfirm intOpts)
        sendAndTailTransaction_ verbose txCfg payload intOpts

    TransactionRegisterData file txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      rdCfg <- getRegisterDataTransactionCfg baseCfg txOpts file
      let txCfg = rdtcTransactionCfg rdCfg
      let nrg = tcEnergy txCfg

      logInfo [[i|Register data. Allowing up to #{showNrg nrg} to be spent as transaction fee.|]]

      when (ioConfirm . toInteractionOpts $ txOpts) $ do
         confirmed <- askConfirmation Nothing
         unless confirmed exitTransactionCancelled

      logInfo ["Registering data..."]

      let intOpts = toInteractionOpts txOpts
      let pl = registerDataTransactionPayload rdCfg

      withClient backend $ do
        mTsr <- sendAndTailTransaction verbose txCfg (Types.encodePayload pl) intOpts
        let extractDataRegistered = extractFromTsr $ \case Types.DataRegistered rd -> Just rd
                                                           _ -> Nothing
        case extractDataRegistered mTsr of
          Nothing -> return ()
          Just (Left err) -> logFatal ["Registering data failed:", err]
          Just (Right _) -> logSuccess ["Data succesfully registered."]

-- |Construct a transaction config for registering data.
--  The data is read from the 'FilePath' provided.
--  Fails if the data can't be read or it violates the size limit checked by 'Types.registeredDataFromBSS'.
getRegisterDataTransactionCfg :: BaseConfig -> TransactionOpts (Maybe Types.Energy) -> RegisterDataInput -> IO RegisterDataTransactionCfg
getRegisterDataTransactionCfg baseCfg txOpts dataInput = do
  bss <- BS.toShort <$> case dataInput of
    RegisterString string -> do
      return $ toStrictByteString $ encodeString string
    RegisterJSON file -> do
      source <- handleReadFile BSL.readFile file
      case AE.eitherDecode source of
        Left err -> fail $ "Error decoding JSON: " ++ err
        Right value -> do 
          return $ toStrictByteString $ encodeValue value
    RegisterRaw file -> handleReadFile BS.readFile file

  rdtcData <- case Types.registeredDataFromBSS bss of
    Left err -> do
      logError [[i|Failed parsing data: #{err} Transaction will fail.|]]
      override <- askConfirmation (Just "Proceed anyway")
      unless override exitFailure
      return $ Types.RegisteredData bss
    Right rd -> return rd
  rdtcTransactionCfg <- getTransactionCfg baseCfg txOpts $ registerDataEnergyCost rdtcData
  return RegisterDataTransactionCfg {..}
  where
    -- Calculate the energy cost for registering data.
    registerDataEnergyCost :: Types.RegisteredData -> EncryptedSigningData -> IO (Maybe (Int -> Types.Energy))
    registerDataEnergyCost rd encSignData = pure . Just . const $
      Cost.registerDataCost + minimumCost payloadSize signatureCount
      where
        signatureCount = mapNumKeys (esdKeys encSignData)
        payloadSize = Types.payloadSize . Types.encodePayload . Types.RegisterData $ rd

registerDataTransactionPayload :: RegisterDataTransactionCfg -> Types.Payload
registerDataTransactionPayload RegisterDataTransactionCfg {..} = Types.RegisterData rdtcData

-- |Transaction config for registering data.
data RegisterDataTransactionCfg =
  RegisterDataTransactionCfg
  { -- |Configuration for the transaction.
    rdtcTransactionCfg :: !TransactionConfig
    -- |The data to register.
  , rdtcData :: !Types.RegisteredData }

-- |Poll the transaction state continuously until it is "at least" the provided one.
-- Note that the "absent" state is considered the "highest" state,
-- so the loop will break if, for instance, the transaction state goes from "committed" to "absent".
awaitState :: (TransactionStatusQuery m) => Int -> TransactionState -> Types.TransactionHash -> m TransactionStatusResult
awaitState t s hash = do
  status <- queryTransactionStatus hash
  if tsrState status >= s then
    return status
  else do
    wait t
    awaitState t s hash

-- |Function type for computing the transaction energy cost for a given number of keys.
-- Returns Nothing if the cost cannot be computed.
type ComputeEnergyCost = Int -> Types.Energy

-- |Function for computing a cost function based on the resolved account config.
type GetComputeEnergyCost = EncryptedSigningData -> IO (Maybe ComputeEnergyCost)

-- |Resolved configuration common to all transaction types.
data TransactionConfig =
  TransactionConfig
  { tcEncryptedSigningData :: EncryptedSigningData
  , tcNonce :: Maybe Types.Nonce
  , tcEnergy :: Types.Energy
  , tcExpiry :: Types.TransactionExpiryTime
  , tcAlias :: Maybe Word }

-- |Resolve transaction config based on persisted config and CLI flags.
-- If an energy cost function is provided and it returns a value which
-- is different from the specified energy allocation, a warning is logged.
-- If the energy allocation is too low, the user is prompted to increase it.
getTransactionCfg :: BaseConfig -> TransactionOpts (Maybe Types.Energy) -> GetComputeEnergyCost -> IO TransactionConfig
getTransactionCfg baseCfg txOpts getEnergyCostFunc = do
  encSignData <- getAccountCfgFromTxOpts baseCfg txOpts
  energyCostFunc <- getEnergyCostFunc encSignData
  let computedCost = case energyCostFunc of
                       Nothing -> Nothing
                       Just ec -> Just $ ec (mapNumKeys (esdKeys encSignData))
  energy <- case (toMaxEnergyAmount txOpts, computedCost) of
              (Nothing, Nothing) -> logFatal ["energy amount not specified"]
              (Nothing, Just c) -> do
                logInfo [printf "using default energy amount of %s" (showNrg c)]
                return c
              (Just maxCost, Nothing) -> return maxCost
              (Just maxCost, Just c) -> promptEnergyUpdate maxCost c

  now <- getCurrentTimeUnix
  expiry <- getExpiryArg "expiry" now $ toExpiration txOpts
  warnSuspiciousExpiry expiry now

  return TransactionConfig
    { tcEncryptedSigningData = encSignData
    , tcNonce = toNonce txOpts
    , tcEnergy = energy
    , tcExpiry = expiry
    , tcAlias = toAlias txOpts }
  where
    promptEnergyUpdate energy actualFee
      | energy < actualFee = do
          logWarn [ "insufficient energy allocated to the transaction"
                  , printf "transaction fee will be %s, but only %s has been allocated" (showNrg actualFee) (showNrg energy) ]
          confirmed <- askConfirmation $ Just [i|Do you want to increase the energy allocation to #{showNrg actualFee}? Confirm|]
          return $ if confirmed then actualFee else energy
      | actualFee < energy = do
          logInfo [printf "%s allocated to the transaction, but only %s is needed" (showNrg energy) (showNrg actualFee)]
          return energy
      | otherwise = return energy

-- |Resolve transaction config based on persisted config and CLI flags.
-- Used for transactions where a specification of maxEnergy is required.
getRequiredEnergyTransactionCfg :: BaseConfig -> TransactionOpts Types.Energy -> IO TransactionConfig
getRequiredEnergyTransactionCfg baseCfg txOpts = do
  encSignData <- getAccountCfgFromTxOpts baseCfg txOpts
  let energy = toMaxEnergyAmount txOpts
  now <- getCurrentTimeUnix
  expiry <- getExpiryArg "expiry" now $ toExpiration txOpts
  warnSuspiciousExpiry expiry now

  return TransactionConfig
    { tcEncryptedSigningData = encSignData
    , tcNonce = toNonce txOpts
    , tcEnergy = energy
    , tcExpiry = expiry
    , tcAlias = toAlias txOpts }

-- |Warn if expiry is in the past or very near or distant future.
-- As the timestamps are unsigned, taking the simple difference might cause underflow.
warnSuspiciousExpiry :: Types.TransactionExpiryTime -> Types.TransactionExpiryTime -> IO ()
warnSuspiciousExpiry expiryArg now
  | expiryArg < now =
    logWarn [ "expiration time is in the past"
            , "the transaction will not be committed" ]
  | expiryArg < now + 30 =
    logWarn [ printf "expiration time is in just %s seconds" (show $ expiryArg - now)
            , "this may not be enough time for the transaction to be committed" ]
  | expiryArg > now + 3600 =
    logWarn [ "expiration time is in more than one hour" ]
  | otherwise = return ()

-- |Get accountCfg from the config folder and return EncryptedSigningData or logFatal if the keys are not provided in txOpts.
getAccountCfgFromTxOpts :: BaseConfig -> TransactionOpts energyOrMaybe -> IO EncryptedSigningData
getAccountCfgFromTxOpts baseCfg txOpts = do
  keysArg <- case toKeys txOpts of
               Nothing -> return Nothing
               Just filePath -> AE.eitherDecodeFileStrict filePath `withLogFatalIO` ("cannot decode keys: " ++)
  let chosenKeysText = toSigners txOpts
  let chosenKeysMaybe :: Maybe (Map.Map ID.CredentialIndex [ID.KeyIndex]) = case chosenKeysText of
        Nothing -> Nothing
        Just t -> Just $ let insertKey c k acc = case Map.lookup c acc of
                              Nothing -> Map.insert c [k] acc
                              Just x -> Map.insert c ([k]++x) acc
                            in foldl' (\acc (c, k) -> insertKey c k acc) Map.empty $ fmap ((\(p1, p2) -> (read . Text.unpack $ p1, read . Text.unpack $ Text.drop 1 p2)) . Text.breakOn ":") $ Text.split (==',') t
  accCfg <- snd <$> getAccountConfig (toSender txOpts) baseCfg Nothing keysArg Nothing AssumeInitialized
  let keys = acKeys accCfg
  case chosenKeysMaybe of
    Nothing -> return EncryptedSigningData{esdKeys=keys, esdAddress = acAddr accCfg, esdEncryptionKey = acEncryptionKey accCfg}
    Just chosenKeys -> do
      let newKeys = Map.intersection keys chosenKeys
      let filteredKeys = Map.mapWithKey (\c m -> Map.filterWithKey (\k _ -> case Map.lookup c chosenKeys of
                                                                        Nothing -> False
                                                                        Just keyList -> elem k keyList) m) newKeys
      _ <- Map.traverseWithKey (\c keyIndices -> case Map.lookup c newKeys of
                                              Nothing -> logFatal [ "No credential holder with index " ++ (show c) ++ "."]
                                              Just credHolderMap -> do
                                                                  let warnIfMissingKey keyIndex = case Map.lookup keyIndex credHolderMap of
                                                                          Nothing -> logFatal [ "No key with index " ++ (show keyIndex) ++ " for credential holder " ++ (show c) ++ "."]
                                                                          Just _ -> return() -- Key found, do nothing. 
                                                                            -- We could add the key to a map in this case, replacing the intersection and mapWithKey steps above.
                                                                  mapM_ warnIfMissingKey keyIndices) chosenKeys
      return EncryptedSigningData{esdKeys=filteredKeys, esdAddress = acAddr accCfg, esdEncryptionKey = acEncryptionKey accCfg}


-- |Resolved configuration for a transfer transaction.
data TransferTransactionConfig =
  TransferTransactionConfig
  { ttcTransactionCfg :: TransactionConfig
  , ttcReceiver :: NamedAddress
  , ttcAmount :: Types.Amount }

-- |Resolved configuration for a transfer transaction.
data TransferWithScheduleTransactionConfig =
  TransferWithScheduleTransactionConfig
  { twstcTransactionCfg :: TransactionConfig
  , twstcReceiver :: NamedAddress
  , twstcSchedule :: [(Time.Timestamp, Types.Amount)]}

data EncryptedTransferTransactionConfig =
  EncryptedTransferTransactionConfig
  { ettTransactionCfg :: TransactionConfig
  , ettReceiver :: NamedAddress
  , ettAmount :: Types.Amount
  , ettTransferData :: !Enc.EncryptedAmountTransferData }

getEncryptedAmountTransferData :: ID.AccountAddress -> NamedAddress -> Types.Amount -> Maybe Int -> ElgamalSecretKey -> ClientMonad IO Enc.EncryptedAmountTransferData
getEncryptedAmountTransferData senderAddr ettReceiver ettAmount idx secretKey = do
  -- get encrypted amounts for the sender
  bbHash <- extractResponseValueOrDie Queries.biBlockHash =<< getBlockInfoV2 Best
  Types.AccountInfo{aiAccountEncryptedAmount=a@Types.AccountEncryptedAmount{..}} <-
    getResponseValueOrDie =<< getAccountInfoV2 (Types.AccAddress senderAddr) (Given bbHash)
  let listOfEncryptedAmounts = Types.getIncomingAmountsList a
  taker <- case idx of
            Nothing -> return id
            Just v ->
              if v < fromIntegral _startIndex
                  || v > fromIntegral _startIndex + length listOfEncryptedAmounts
              then logFatal ["The index provided must be at least the index of the first incoming amount on the account and at most `start index + number of incoming amounts`"]
              else return $ take (v - fromIntegral _startIndex)
  -- get receiver's public encryption key
  air <- getResponseValueOrDie =<< getAccountInfoV2 (Types.AccAddress $ naAddr ettReceiver) (Given bbHash)
  globalContext <- getResponseValueOrDie =<< getCryptographicParametersV2 (Given bbHash)
  let receiverPk = ID._elgamalPublicKey $ Types.aiAccountEncryptionKey air
  -- precomputed table for speeding up decryption
  let table = Enc.computeTable globalContext (2^(16::Int))
      decoder = Enc.decryptAmount table secretKey
      selfDecrypted = decoder _selfAmount
  -- aggregation of idx encrypted amounts
      inputEncAmounts = taker listOfEncryptedAmounts
      aggAmounts = foldl' (<>) _selfAmount inputEncAmounts
      totalEncryptedAmount = foldl' (+) selfDecrypted $ fmap decoder inputEncAmounts
  unless (totalEncryptedAmount >= ettAmount) $
    logFatal [printf "The requested transfer (%s) is more than the total encrypted balance (%s)." (Types.amountToString ettAmount) (Types.amountToString totalEncryptedAmount)]
  -- index indicating which encrypted amounts we used as input
  let aggIndex = case idx of
        Nothing -> Enc.EncryptedAmountAggIndex (Enc.theAggIndex _startIndex + fromIntegral (length listOfEncryptedAmounts))
        Just idx' -> Enc.EncryptedAmountAggIndex (fromIntegral idx')
        -- we use the supplied index if given. We already checked above that it is within bounds.
      aggAmount = Enc.makeAggregatedDecryptedAmount aggAmounts totalEncryptedAmount aggIndex
  liftIO $ Enc.makeEncryptedAmountTransferData globalContext receiverPk secretKey aggAmount ettAmount >>= \case
    Nothing -> logFatal ["Could not create transfer. Likely the provided secret key is incorrect."]
    Just ettTransferData -> return ettTransferData

-- |Returns the UTCTime date when the baker cooldown on reducing stake/removing a baker will end, using on chain parameters
getBakerCooldown :: Queries.EChainParametersAndKeys -> ClientMonad IO UTCTime
getBakerCooldown (Queries.EChainParametersAndKeys (ecpParams :: ChainParameters' cpv) _) = do
  cooldownTime <- case Types.chainParametersVersion @cpv of
    Types.SCPV0 -> do
        cs <- getResponseValueOrFail =<< getConsensusInfoV2
        let epochTime = toInteger (Time.durationMillis $ Queries.csEpochDuration cs) % 1000
        return . fromRational $ epochTime * ((cooldownEpochsV0 ecpParams + 2) % 1)
    Types.SCPV1 -> return .fromIntegral . Types.durationSeconds $
        ecpParams ^. cpCooldownParameters . cpPoolOwnerCooldown
  currTime <- liftIO getCurrentTime
  let cooldownDate = addUTCTime cooldownTime currTime
  return cooldownDate
  where
    cooldownEpochsV0 ups =
        toInteger $ ups ^. cpCooldownParameters . cpBakerExtraCooldownEpochs

-- |Returns the UTCTime date when the delegator cooldown on reducing stake/removing delegation will end, using on chain parameters
getDelegatorCooldown :: Queries.EChainParametersAndKeys -> IO (Maybe UTCTime)
getDelegatorCooldown (Queries.EChainParametersAndKeys (ecpParams :: ChainParameters' cpv) _) = do
  case Types.chainParametersVersion @cpv of
      Types.SCPV0 -> do
        return Nothing
      Types.SCPV1 -> do
        currTime <- liftIO getCurrentTime
        let cooldownTime = fromIntegral . Types.durationSeconds $ ecpParams ^. cpCooldownParameters . cpDelegatorCooldown
        return $ Just $ addUTCTime cooldownTime currTime

-- |Query the chain for the given account. Fail if either the chain cannot be reached, or
-- if the account does not exist.
getAccountInfoOrDie :: (MonadIO m) => ID.AccountAddress -> BlockHashInput -> ClientMonad m Types.AccountInfo
getAccountInfoOrDie senderAddr bhInput = do
  res <- getAccountInfoV2 (Types.AccAddress senderAddr) bhInput
  case res of
    StatusOk resp -> case grpcResponseVal resp of
      Left err -> logFatal ["Cannot decode account info response from the node: " <> err]
      Right v -> return v
    StatusNotOk (NOT_FOUND, _) -> logFatal [[i|Account #{senderAddr} does not exist on the chain.|]] 
    StatusNotOk (status, err) -> logFatal [[i|GRPC response with status '#{status}': #{err}|]] 
    StatusInvalid -> logFatal ["GRPC response contained an invalid status code."]
    RequestFailed err -> logFatal [[i|I/O error: #{err}|]]

-- |Query the chain for the given pool. Fail if either the chain cannot be reached, or
-- if the baker pool does not exist.
getPoolStatusOrDie :: Maybe Types.BakerId ->  ClientMonad IO Queries.PoolStatus
getPoolStatusOrDie mbid = do
  getPoolInfoV2 Best (fromMaybe 0 mbid) >>= getResponseValueOrFail

data CredentialUpdateKeysTransactionCfg =
  CredentialUpdateKeysTransactionCfg
  { cuktcTransactionCfg :: TransactionConfig
  , cuktcKeys :: ID.CredentialPublicKeys
  , cuktcCredId :: ID.CredentialRegistrationID }

data AccountUpdateCredentialsTransactionCfg =
  AccountUpdateCredentialsTransactionCfg
  { auctcTransactionCfg :: TransactionConfig
  , auctcNewCredInfos :: Map.Map ID.CredentialIndex ID.CredentialDeploymentInformation
  , auctcRemoveCredIds :: [ID.CredentialRegistrationID]
  , auctcNewThreshold :: ID.AccountThreshold }

-- |Resolved configuration for transferring from public to encrypted balance.
data AccountEncryptTransactionConfig =
  AccountEncryptTransactionConfig
  { aeTransactionCfg :: TransactionConfig,
    aeAmount :: Types.Amount
  }

-- |Resolved configuration for transferring from encrypted to public balance.
data AccountDecryptTransactionConfig =
  AccountDecryptTransactionConfig
  { adTransactionCfg :: TransactionConfig,
    adTransferData :: Enc.SecToPubAmountTransferData
  }

-- |Resolve configuration for transferring an amount from public to encrypted
-- balance of an account.
getAccountEncryptTransactionCfg :: BaseConfig -> TransactionOpts (Maybe Types.Energy) -> Types.Amount -> Types.PayloadSize -> IO AccountEncryptTransactionConfig
getAccountEncryptTransactionCfg baseCfg txOpts aeAmount payloadSize = do
  aeTransactionCfg <- getTransactionCfg baseCfg txOpts nrgCost
  return AccountEncryptTransactionConfig{..}
  where nrgCost _ = return $ Just $ accountEncryptEnergyCost payloadSize

-- |Resolve configuration for transferring an amount from encrypted to public
-- balance of an account.
getAccountDecryptTransferData :: ID.AccountAddress -> Types.Amount -> ElgamalSecretKey -> Maybe Int -> ClientMonad IO Enc.SecToPubAmountTransferData
getAccountDecryptTransferData senderAddr adAmount secretKey idx = do
  bbHash <- extractResponseValueOrDie Queries.biBlockHash =<< getBlockInfoV2 Best
  Types.AccountInfo{aiAccountEncryptedAmount=a@Types.AccountEncryptedAmount{..}} <-
    getResponseValueOrDie =<< getAccountInfoV2 (Types.AccAddress senderAddr) (Given bbHash)
  globalContext <- getResponseValueOrDie =<< getCryptographicParametersV2 (Given bbHash)
  let listOfEncryptedAmounts = Types.getIncomingAmountsList a
  taker <- case idx of
    Nothing -> return id
    Just v -> if v < fromIntegral _startIndex
                || v > fromIntegral _startIndex + length listOfEncryptedAmounts
              then logFatal ["The index provided must be at least the index of the first incoming amount on the account and at most `start index + number of incoming amounts`"]
              else return $ take (v - fromIntegral _startIndex)
  -- precomputed table for speeding up decryption
  let table = Enc.computeTable globalContext (2^(16::Int))
      decoder = Enc.decryptAmount table secretKey
      selfDecrypted = decoder _selfAmount
  -- aggregation of all encrypted amounts
      inputEncAmounts = taker listOfEncryptedAmounts
      aggAmounts = foldl' (<>) _selfAmount inputEncAmounts
      totalEncryptedAmount = foldl' (+) selfDecrypted $ fmap decoder inputEncAmounts
  unless (totalEncryptedAmount >= adAmount) $
    logFatal [printf "The requested transfer (%s) is more than the total encrypted balance (%s)." (Types.amountToString adAmount) (Types.amountToString totalEncryptedAmount)]
  -- index indicating which encrypted amounts we used as input
  let aggIndex = case idx of
        Nothing -> Enc.EncryptedAmountAggIndex (Enc.theAggIndex _startIndex + fromIntegral (length listOfEncryptedAmounts))
        Just idx' -> Enc.EncryptedAmountAggIndex (fromIntegral idx')
      aggAmount = Enc.makeAggregatedDecryptedAmount aggAmounts totalEncryptedAmount aggIndex
  liftIO $ Enc.makeSecToPubAmountTransferData globalContext secretKey aggAmount adAmount >>= \case
    Nothing -> logFatal ["Could not create transfer. Likely the provided secret key is incorrect."]
    Just adTransferData -> return adTransferData

-- |Get the cryptographic parameters in a given block, and attempt to parse them.
getParseCryptographicParameters :: Text -> ClientMonad IO (Either String GlobalContext)
getParseCryptographicParameters bHash = runExceptT $ do
  vglobalContext <- liftEither =<< (lift . getCryptographicParameters $ bHash)
  case AE.fromJSON $ grpcResponseVal vglobalContext of
    AE.Error err -> throwError err
    AE.Success Nothing -> throwError "The given block does not exist."
    AE.Success (Just Versioned{..}) -> do
      unless (vVersion == 0) $ throwError "Received unsupported version of cryptographic parameters."
      return vValue

-- |Convert transfer transaction config into a valid payload,
-- optionally asking the user for confirmation.
transferTransactionConfirm :: TransferTransactionConfig -> Bool -> IO ()
transferTransactionConfirm ttxCfg confirm = do
  let TransferTransactionConfig
        { ttcTransactionCfg = TransactionConfig
                              { tcEnergy = energy
                              , tcExpiry = expiryTs
                              , tcEncryptedSigningData = EncryptedSigningData { esdAddress = fromAddress } }
        , ttcAmount = amount
        , ttcReceiver = toAddress }
        = ttxCfg

  logSuccess [ printf "sending %s from %s to %s" (showCcd amount) (showNamedAddress fromAddress) (showNamedAddress toAddress)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , printf "transaction expires on %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiryTs) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

-- |Convert transfer transaction config into a valid payload,
-- optionally asking the user for confirmation.
transferWithScheduleTransactionConfirm :: TransferWithScheduleTransactionConfig -> Bool -> IO ()
transferWithScheduleTransactionConfirm ttxCfg confirm = do
  let TransferWithScheduleTransactionConfig
        { twstcTransactionCfg = TransactionConfig
                              { tcEnergy = energy
                              , tcExpiry = expiryTs
                              , tcEncryptedSigningData = EncryptedSigningData { esdAddress = fromAddress } }
        , twstcSchedule = schedule
        , twstcReceiver = toAddress }
        = ttxCfg

  logSuccess [ printf "sending CCDs from %s to %s" (showNamedAddress fromAddress) (showNamedAddress toAddress)
             , printf "with the following release schedule:\n%swith a total amount of %s" (unlines $ map (\(a, b) -> showTimeFormatted (Time.timestampToUTCTime a) ++ ": " ++ showCcd b) schedule) (showCcd $ foldl' (\acc (_, x) -> acc + x) 0 schedule)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , printf "transaction expires on %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiryTs) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

encryptedTransferTransactionConfirm :: MonadIO m => EncryptedTransferTransactionConfig -> Bool -> m ()
encryptedTransferTransactionConfirm EncryptedTransferTransactionConfig{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcEncryptedSigningData = EncryptedSigningData { esdAddress = addr } }
        = ettTransactionCfg

  logInfo
    [ printf "transferring %s CCD from shielded balance of account %s to %s" (Types.amountToString ettAmount) (showNamedAddress addr) (showNamedAddress ettReceiver)
    , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires on %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

-- |Query the chain for the minimum baker stake threshold. Fail if the chain cannot be reached.
getBakerStakeThresholdOrDie :: ClientMonad IO Types.Amount
getBakerStakeThresholdOrDie = do
  blockSummary <- getFromJson' =<< withLastFinalBlockHash Nothing getBlockSummary
  case blockSummary of
    Nothing -> do
      logFatal ["Could not reach the node to retrieve the baker stake threshold."]
    Just bs -> return $ Queries.bsWithUpdates bs $ \spv ups ->
        case Types.chainParametersVersionFor spv of
            Types.SCPV0 -> ups ^. Types.currentParameters ^. cpPoolParameters ^. ppBakerStakeThreshold
            Types.SCPV1 -> ups ^. Types.currentParameters ^. cpPoolParameters ^. ppMinimumEquityCapital

getAccountUpdateCredentialsTransactionData ::
  Maybe FilePath -- ^ A file with new credentials.
  -> Maybe FilePath -- ^ A file with an array of credential registration ids to remove.
  -> ID.AccountThreshold -- ^ New threshold.
  -> IO (Types.EncodedPayload, [Int], Map.Map ID.CredentialIndex ID.CredentialDeploymentInformation, [ID.CredentialRegistrationID])
  -- ^ Return the payload to send, and the number of keys in all the new credentials, the new credentials, and the list of removed credentials.
getAccountUpdateCredentialsTransactionData f1 f2 ucNewThreshold = do
  ucNewCredInfos <- case f1 of
    Nothing -> return Map.empty
    Just file -> getFromJson =<< eitherDecodeFileStrict file
  ucRemoveCredIds <- case f2 of
    Nothing -> return []
    Just file -> getFromJson =<< eitherDecodeFileStrict file
  let pload = Types.encodePayload Types.UpdateCredentials{..}
  let numKeysList = map (ID.credNumKeys . ID.credPubKeys) $ Map.elems ucNewCredInfos
  return (pload, numKeysList, ucNewCredInfos, ucRemoveCredIds)

credentialUpdateKeysTransactionConfirm :: CredentialUpdateKeysTransactionCfg -> Bool -> IO ()
credentialUpdateKeysTransactionConfirm CredentialUpdateKeysTransactionCfg{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcEncryptedSigningData = EncryptedSigningData { esdAddress = addr } }
        = cuktcTransactionCfg

  let logNewKeys = Map.foldrWithKey (\idx (SigScheme.VerifyKeyEd25519 key) l -> (printf "\t%s: %s" (show idx) (show key)) : l) [] (ID.credKeys cuktcKeys)

  logInfo $
    [ [i|"setting the following keys for credential #{show cuktcCredId} on account #{showNamedAddress addr}:" |] ] ++
    logNewKeys ++
    [ printf "with threshold %s" (show (ID.credThreshold cuktcKeys))] ++
    [ printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires on %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

accountUpdateCredentialsTransactionConfirm :: AccountUpdateCredentialsTransactionCfg -> Bool -> IO ()
accountUpdateCredentialsTransactionConfirm AccountUpdateCredentialsTransactionCfg{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcEncryptedSigningData = EncryptedSigningData { esdAddress = addr } }
        = auctcTransactionCfg

  let logNewCids = Map.foldrWithKey (\idx cdi l -> (printf "\t%s: %s" (show idx) (show $ ID.credId $ cdi)) : l) [] auctcNewCredInfos

  logInfo $
    [ printf "adding credentials to account %s with the following credential registration ids on account:" (showNamedAddress addr) ] ++
    logNewCids ++
    [ printf "setting new account threshold %s" (show (auctcNewThreshold))] ++
    [ printf "removing credentials with credential registration ids %s" (show (auctcRemoveCredIds))] ++
    [ printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires on %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled


accountEncryptTransactionConfirm :: AccountEncryptTransactionConfig -> Bool -> IO ()
accountEncryptTransactionConfirm AccountEncryptTransactionConfig{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcEncryptedSigningData = EncryptedSigningData { esdAddress = addr } }
        = aeTransactionCfg


  logInfo $
    [ printf "transferring %s CCD from public to shielded balance of account %s" (Types.amountToString aeAmount) (showNamedAddress addr)
    , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires on %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled


accountDecryptTransactionConfirm :: MonadIO m => AccountDecryptTransactionConfig -> Bool -> m ()
accountDecryptTransactionConfirm AccountDecryptTransactionConfig{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcEncryptedSigningData = EncryptedSigningData { esdAddress = addr } }
        = adTransactionCfg

  logInfo $
    [ printf "transferring %s CCD from shielded to public balance of account %s" (Types.amountToString (Enc.stpatdTransferAmount adTransferData)) (showNamedAddress addr)
    , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires on %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled


-- |Encode, sign, and send transaction off to the baker.
-- If confirmNonce is set, the user is asked to confirm using the next nonce
-- if there are pending transactions.
startTransaction :: (MonadFail m, MonadIO m)
  => TransactionConfig
  -> Types.EncodedPayload
  -> Bool
  -> Maybe AccountKeyMap -- ^ The decrypted account signing keys. If not provided, the encrypted keys
                         -- from the 'TransactionConfig' will be used, and for each the password will be queried.
  -> ClientMonad m Types.BareBlockItem
startTransaction txCfg pl confirmNonce maybeAccKeys = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcNonce = n
        , tcEncryptedSigningData = EncryptedSigningData { esdAddress = NamedAddress{..}, .. }
        , ..
        } = txCfg
  nonce <- getNonce naAddr n confirmNonce
  accountKeyMap <- case maybeAccKeys of
                     Just acKeys' -> return acKeys'
                     Nothing -> liftIO $ failOnError $ decryptAccountKeyMapInteractive esdKeys Nothing Nothing
  let sender = applyAlias tcAlias naAddr
  let tx = signEncodedTransaction pl sender energy nonce expiry accountKeyMap
  when (isJust tcAlias) $
      logInfo [[i|Using the alias #{sender} as the sender of the transaction instead of #{naAddr}.|]]
  sbiRes <- sendBlockItemV2 tx
  let res = case sbiRes of
        StatusOk resp -> Right resp
        StatusNotOk (status, err) -> Left [[i|GRPC response with status '#{status}': #{err}|]] 
        StatusInvalid -> Left ["GRPC response contained an invalid status code."]
        RequestFailed err -> Left [[i|I/O error: #{err}|]]
  case res of
    Left err -> logFatal $ [[i|Transaction not accepted by the baker:|]] <> err
    Right _ -> return tx

-- |Fetch next nonces relative to the account's most recently committed and
-- pending transactions, respectively.
-- If they match, the nonce is returned.
-- If they don't match, optionally ask the user to confirm proceeding with the latter nonce.
-- If rejected, the process is cancelled (exit with code 0).
getNonce :: (MonadFail m, MonadIO m) => Types.AccountAddress -> Maybe Types.Nonce -> Bool -> ClientMonad m Types.Nonce
getNonce sender nonce confirm =
  case nonce of
    Nothing -> do
      currentNonce <- extractResponseValueOrDie Types.aiAccountNonce =<< getAccountInfoV2 (Types.AccAddress sender) Best
      nextNonce <- fmap Queries.nanNonce . getResponseValueOrDie =<< getNextSequenceNumberV2 sender
      liftIO $ when (currentNonce /= nextNonce) $ do
        logWarn [ printf "there is a pending transaction with nonce %s, but last committed one has %s" (show $ nextNonce-1) (show $ currentNonce-1)
                , printf "this transaction will have nonce %s and might hang if the pending transaction fails" (show nextNonce) ]
        when confirm $ do
          putStrLn "Proceed if you're confident that all currently pending transactions are valid."
          confirmed <- askConfirmation $ Just "proceed"
          unless confirmed exitTransactionCancelled
      return nextNonce
    Just v -> return v

-- |Send a transaction and optionally tail it (see 'tailTransaction' below).
sendAndTailTransaction_ :: (MonadIO m, MonadFail m)
    => Bool -- ^ Whether the output should be verbose
    -> TransactionConfig -- ^ Information about the sender, and the context of transaction
    -> Types.EncodedPayload -- ^ Payload of the transaction to send
    -> InteractionOpts -- ^ How interactive should sending and tailing be
    -> ClientMonad m ()
sendAndTailTransaction_ verbose txCfg pl intOpts = void $ sendAndTailTransaction verbose txCfg pl intOpts

-- |Send a transaction and optionally tail it (see 'tailTransaction' below).
-- If tailed, it returns the TransactionStatusResult of the finalized status,
-- otherwise the return value is `Nothing`.
sendAndTailTransaction :: (MonadIO m, MonadFail m)
    => Bool -- ^ Whether the output should be verbose
    -> TransactionConfig -- ^ Information about the sender, and the context of transaction
    -> Types.EncodedPayload -- ^ Payload of the transaction to send
    -> InteractionOpts -- ^ How interactive should sending and tailing be
    -> ClientMonad m (Maybe TransactionStatusResult)
sendAndTailTransaction verbose txCfg pl intOpts = do
  tx <- startTransaction txCfg pl (ioConfirm intOpts) Nothing
  let hash = getBlockItemHash tx
  logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
  if ioTail intOpts
  then Just <$> tailTransaction verbose hash
  else return Nothing

-- |Continuously query and display transaction status until the transaction is finalized.
tailTransaction_ :: (MonadIO m) => Bool -> Types.TransactionHash -> ClientMonad m ()
tailTransaction_ verbose hash = void $ tailTransaction verbose hash

-- |Continuously query and display transaction status until the transaction is finalized.
-- Returns the TransactionStatusResult of the finalized status.
tailTransaction :: (MonadIO m) => Bool -> Types.TransactionHash-> ClientMonad m TransactionStatusResult
tailTransaction verbose hash = do
  logInfo [ "waiting for the transaction to be committed and finalized"
          , "you may skip this step by interrupting the command using Ctrl-C (pass flag '--no-wait' to do this by default)"
          , printf "the transaction will still get processed and may be queried using\n  'concordium-client transaction status %s'" (show hash) ]

  liftIO $ printf "[%s] Waiting for the transaction to be committed..." =<< getLocalTimeOfDayFormatted
  committedStatus <- awaitState 2 Committed hash
  liftIO $ putStrLn ""

  when (tsrState committedStatus == Absent) $
    logFatal [ "transaction failed before it got committed"
             , "most likely because it was invalid" ]

  committedContrInfoWithSchemas <- getTxContractInfoWithSchemas Nothing committedStatus
  runPrinter $ printTransactionStatus committedStatus verbose $ Just committedContrInfoWithSchemas

  -- If the transaction goes back to pending state after being committed
  -- to a branch which gets dropped later on, the command will currently
  -- keep presenting the transaction as committed and awaiting finalization.
  -- As the transaction is still expected to go back to being committed or
  -- (if for instance it expires) become absent, this seems acceptable
  -- from a UX perspective. Also, this is expected to be a very uncommon case.
  
  status <- if tsrState committedStatus == Finalized then -- if transaction is already finalized, there is no reason to print "Waiting for the transaction to be finalized..."
    return committedStatus
  else do
    liftIO $ printf "[%s] Waiting for the transaction to be finalized..." =<< getLocalTimeOfDayFormatted
    finalizedStatus <- awaitState 5 Finalized hash
    liftIO $ putStrLn ""

    when (tsrState finalizedStatus == Absent) $
      logFatal [ "transaction failed after it was committed"
              , "response:\n" ++ showPrettyJSON committedStatus ]

    -- Print out finalized status.
    finalizedContrInfoWithSchemas <- getTxContractInfoWithSchemas Nothing finalizedStatus
    runPrinter $ printTransactionStatus finalizedStatus verbose $ Just finalizedContrInfoWithSchemas

    return finalizedStatus
  liftIO $ printf "[%s] Transaction finalized.\n" =<< getLocalTimeOfDayFormatted
  return status

  where
    getLocalTimeOfDayFormatted = showTimeOfDay <$> getLocalTimeOfDay

-- |`read` input or fail if the input could not be `read`.
readOrFail :: (MonadIO m, Read a) => Text -> m a
readOrFail t =
  case readEither s of
    Left err -> logFatal [[i|Unable to parse '#{s}': #{err}|]]
    Right v -> return v
  where s = Text.unpack t

-- |Reads a blockhash wrapped in a @Maybe@.
-- If the provided value is @Nothing@, a default value provided in the first parameter 
-- is returned. If the provided value is @Just s@, @readOrFail s@ is returned. Fails if
-- @s@ is not a valid blockhash.
readBlockHashOrDefault :: (MonadIO m) => BlockHashInput -> Maybe Text -> m BlockHashInput
readBlockHashOrDefault  d Nothing = return d
readBlockHashOrDefault  _ (Just s) = readOrFail s >>= return . Given

-- |Process an 'account ...' command.
processAccountCmd :: AccountCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processAccountCmd action baseCfgDir verbose backend =
  case action of
    AccountShow inputMaybe block showEncrypted showDecrypted -> do
      baseCfg <- getBaseConfig baseCfgDir verbose

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      input <- case inputMaybe of
        Nothing -> do
          case Map.lookup defaultAccountName (bcAccountNameMap baseCfg) of
            Nothing -> do
              logFatal [[i|The ACCOUNT argument was not provided; so the default account name '#{defaultAccountName}' was used, but no account with that name exists.|]]
            Just acc -> do
              logInfo [[i|The ACCOUNT argument was not provided; so the default account name '#{defaultAccountName}' was used.|]]
              return . Text.pack $ show acc
        Just acc -> return acc

      accountIdentifier <-
        case Types.decodeAccountIdentifier (Text.encodeUtf8 input) of
          Just v -> return v -- input is a wellformed account identifier
          Nothing -> logFatal [[i|The identifier '#{input}' is neither a credential registration ID, an account index, the address nor the name of an account|]]

      (accInfo, na, dec) <- withClient backend $ do
        -- query account
        bhInput <- readBlockHashOrDefault Best block
        accInfo <- do
          res <- getAccountInfoV2 accountIdentifier bhInput
          case res of
            StatusOk resp -> case grpcResponseVal resp of
              Left err -> logFatal [[i|Cannot decode account info response from the node: #{err}|]] 
              Right v -> return v
            StatusNotOk (NOT_FOUND, _) -> logFatal [[i|Account does not exist on the chain.|]] 
            StatusNotOk (status, err) -> logFatal [[i|GRPC response with status '#{status}': #{err}|]] 
            StatusInvalid -> logFatal ["GRPC response contained an invalid status code."]
            RequestFailed err -> logFatal [[i|I/O error: #{err}|]]
        -- derive the address of the account from the the initial credential
        resolvedAddress <-
          case Map.lookup (ID.CredentialIndex 0) (Types.aiAccountCredentials accInfo) of
            Nothing -> logFatal [[i|No initial credential found for the account identified by '#{accountIdentifier}'|]]
            Just v -> return $ ID.addressFromRegIdRaw $ ID.credId $ vValue v
        -- reverse lookup local account names
        let na = NamedAddress {naNames = findAllNamesFor (bcAccountNameMap baseCfg) resolvedAddress, naAddr = resolvedAddress}
        
        encKey <- liftIO $
          if showDecrypted
          then do
            encKeys <- (\l -> [ acEncryptionKey v | v <- l, naAddr (acAddr v) == resolvedAddress, isJust (acEncryptionKey v) ] ) <$> getAllAccountConfigs baseCfg
            case encKeys of
              [Just enc] -> do
                decrypted <- decryptAccountEncryptionSecretKeyInteractive enc
                case decrypted of
                  Right v -> return (Just v)
                  _ -> logFatal [printf "Couldn't decrypt encryption key for account '%s' with the provided password" (show $ naAddr na)]
              _ -> logFatal [printf "Tried to decrypt balance of account '%s' but this account is not present in the local store" (show $ naAddr na)]
          else return Nothing
        case encKey of
          Nothing ->
            return (accInfo, na, Nothing)
          Just k -> do
            gc <- getResponseValueOrDie =<< getCryptographicParametersV2 Best
            return (accInfo, na, Just (k, gc))

      runPrinter $ printAccountInfo na accInfo verbose (showEncrypted || showDecrypted) dec

    AccountList block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      bhInput <- readBlockHashOrDefault Best block
      accs <- withClient backend $
        getAccountListV2 bhInput >>=
        getResponseValueOrDie
      runPrinter $ printAccountList (bcAccountNameMap baseCfg) (toList accs)

    AccountUpdateKeys f cid txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      accCfg <- liftIO $ getAccountCfgFromTxOpts baseCfg txOpts
      let senderAddress = naAddr $ esdAddress accCfg

      withClient backend $ do
        keys <- liftIO $ getFromJson =<< eitherDecodeFileStrict f
        let pl = Types.encodePayload $ Types.UpdateCredentialKeys cid keys

        accInfo <- getAccountInfoOrDie senderAddress Best
        let numCredentials = Map.size $ Types.aiAccountCredentials accInfo
        let numKeys = length $ ID.credKeys keys
        let nrgCost _ = return $ Just $ accountUpdateKeysEnergyCost (Types.payloadSize pl) numCredentials numKeys

        txCfg <- liftIO $ getTransactionCfg baseCfg txOpts nrgCost

        let aukCfg = CredentialUpdateKeysTransactionCfg txCfg keys cid

        -- TODO: Check that the credential exists on the chain before making the update.

        when verbose $ liftIO $ do
          runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData txCfg
          putStrLn ""

        let intOpts = toInteractionOpts txOpts
        liftIO $ credentialUpdateKeysTransactionConfirm aukCfg (ioConfirm intOpts)
        sendAndTailTransaction_ verbose txCfg pl intOpts

    AccountUpdateCredentials cdisFile removeCidsFile newThreshold txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      accCfg <- liftIO $ getAccountCfgFromTxOpts baseCfg txOpts
      let senderAddress = naAddr $ esdAddress accCfg

      withClient backend $ do
        accInfo <- getAccountInfoOrDie senderAddress Best
        (epayload, numKeys, newCredentials, removedCredentials) <- liftIO $ getAccountUpdateCredentialsTransactionData cdisFile removeCidsFile newThreshold
        let numExistingCredentials =  Map.size (Types.aiAccountCredentials accInfo)
        let nrgCost _ = return $ Just $ accountUpdateCredentialsEnergyCost (Types.payloadSize epayload) numExistingCredentials numKeys
        txCfg <- liftIO $ getTransactionCfg baseCfg txOpts nrgCost
        when verbose $ liftIO $ do
          runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData txCfg
          putStrLn ""

        let intOpts = toInteractionOpts txOpts
        let aucCfg = AccountUpdateCredentialsTransactionCfg
                { auctcTransactionCfg = txCfg
                , auctcNewCredInfos = newCredentials
                , auctcRemoveCredIds = removedCredentials
                , auctcNewThreshold = newThreshold }
        liftIO $ accountUpdateCredentialsTransactionConfirm aucCfg (ioConfirm intOpts)
        sendAndTailTransaction_ verbose txCfg epayload intOpts

    AccountEncrypt{..} -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      let pl = Types.encodePayload $ Types.TransferToEncrypted aeAmount

      aetxCfg <- getAccountEncryptTransactionCfg baseCfg aeTransactionOpts aeAmount (Types.payloadSize pl)
      let txCfg = aeTransactionCfg aetxCfg
      when verbose $ do
        runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData txCfg
        putStrLn ""

      let intOpts = toInteractionOpts aeTransactionOpts
      accountEncryptTransactionConfirm aetxCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction_ verbose txCfg pl intOpts

    AccountDecrypt{..} -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      accCfg <- getAccountCfgFromTxOpts baseCfg adTransactionOpts
      let senderAddr = esdAddress accCfg

      encryptedSecretKey <- maybe (logFatal ["Missing account encryption secret key for account: " ++ show senderAddr]) return (esdEncryptionKey accCfg)
      secretKey <- either (\e -> logFatal ["Couldn't decrypt account encryption secret key: " ++ e]) return =<< decryptAccountEncryptionSecretKeyInteractive encryptedSecretKey

      withClient backend $ do
        transferData <- getAccountDecryptTransferData (naAddr senderAddr) adAmount secretKey adIndex
        let pl = Types.encodePayload $ Types.TransferToPublic transferData

        let nrgCost _ = return $ Just $ accountDecryptEnergyCost $ Types.payloadSize pl
        txCfg <- liftIO (getTransactionCfg baseCfg adTransactionOpts nrgCost)


        let adtxCfg = AccountDecryptTransactionConfig{
          adTransactionCfg = txCfg,
          adTransferData = transferData
        }

        when verbose $ do
          runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData txCfg
          liftIO $ putStrLn ""

        let intOpts = toInteractionOpts adTransactionOpts
        accountDecryptTransactionConfirm adtxCfg (ioConfirm intOpts)
        sendAndTailTransaction_ verbose txCfg pl intOpts

    AccountShowAlias addrOrName alias -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      case getAccountAddress (bcAccountNameMap baseCfg) addrOrName of
        Left err -> logFatal [err]
        Right namedAddr ->
          putStrLn [i|The requested alias for address #{naAddr namedAddr} is #{Types.createAlias (naAddr namedAddr) alias}|]

-- |Process a 'module ...' command.
processModuleCmd :: ModuleCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processModuleCmd action baseCfgDir verbose backend =
  case action of
    ModuleDeploy modFile modName mWasmVersion txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose

      mdCfg <- getModuleDeployTransactionCfg baseCfg txOpts modFile mWasmVersion
      let txCfg = mdtcTransactionCfg mdCfg

      let nrg = tcEnergy txCfg

      let msgIntro = case modName of
            Nothing -> [i|deploy the module '#{modFile}'|]
            Just modName' -> [i|deploy the module '#{modFile}' and name it '#{modName'}'|]
      logInfo [ msgIntro
              , [i|allowing up to #{showNrg nrg} to be spent as transaction fee|]]

      when (ioConfirm . toInteractionOpts $ txOpts) $ do
         confirmed <- askConfirmation Nothing
         unless confirmed exitTransactionCancelled

      logInfo ["deploying module..."]

      let intOpts = toInteractionOpts txOpts
      let pl = moduleDeployTransactionPayload mdCfg

      withClient backend $ do
        mTsr <- sendAndTailTransaction verbose txCfg (Types.encodePayload pl) intOpts
        case extractModRef mTsr of
          Nothing -> return ()
          Just (Left err) -> logFatal ["module deployment failed:", err]
          Just (Right modRef) -> do
            logSuccess [[i|module successfully deployed with reference: '#{modRef}'|]]
            case modName of
              Nothing -> return ()
              Just modName' -> do
                nameAdded <- liftIO $ addModuleNameAndWrite verbose baseCfg modName' modRef
                logSuccess [[i|module reference #{modRef} was successfully named '#{nameAdded}'|]]

    ModuleList block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      bhInput <- readBlockHashOrDefault Best block
      ms <- withClient backend $
        getModuleListV2 bhInput >>=
          getResponseValueOrFail
      runPrinter $ printModuleList (bcModuleNameMap baseCfg) (toList ms)

    ModuleShow modRefOrName outFile block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      namedModRef <- getNamedModuleRef (bcModuleNameMap baseCfg) modRefOrName
      wasmModule <- withClient backend $ getWasmModule namedModRef =<< readBlockHashOrDefault Best block
      logInfo [[i|WASM Version of module: #{Wasm.wasmVersion wasmModule}|]]
      let wasmModuleBytes = S.encode wasmModule
      case outFile of
        -- Write to stdout
        "-" -> BS.putStr wasmModuleBytes
        -- Write to file
        _   -> do
          success <- handleWriteFile BS.writeFile PromptBeforeOverwrite verbose outFile wasmModuleBytes
          when success $ logSuccess [[i|wrote module source to the file '#{outFile}'|]]

    ModuleInspect modRefOrName schemaFile block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      namedModRef <- getNamedModuleRef (bcModuleNameMap baseCfg) modRefOrName
      wasmModule <- withClient backend $ getWasmModule namedModRef =<< readBlockHashOrDefault Best block
      let wasmVersion = Wasm.wasmVersion wasmModule
      (schema, exports) <- getSchemaAndExports schemaFile wasmModule
      let moduleInspectInfo = CI.constructModuleInspectInfo namedModRef wasmVersion schema exports
      runPrinter $ printModuleInspectInfo moduleInspectInfo

    ModuleName modRefOrFile modName mWasmVersion -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      modRef <- getModuleRefFromRefOrFile modRefOrFile mWasmVersion
      nameAdded <- liftIO $ addModuleNameAndWrite verbose baseCfg modName modRef
      logSuccess [[i|module reference #{modRef} was successfully named '#{nameAdded}'|]]

    ModuleRemoveName name -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        let nameMap = bcModuleNameMap baseCfg
        case Map.lookup name nameMap of
          Nothing -> logFatal [[i|the name '#{name}' is not in use|]]
          Just currentAddr -> do
            logInfo [[i|removing mapping from '#{name}' to contract address '#{currentAddr}'|]]
            void $ removeModuleNameAndWrite baseCfg name verbose

  where extractModRef = extractFromTsr (\case
                                           Types.ModuleDeployed modRef -> Just modRef
                                           _ -> Nothing)

getModuleDeployTransactionCfg :: BaseConfig -> TransactionOpts (Maybe Types.Energy) -> FilePath -> Maybe Wasm.WasmVersion -> IO ModuleDeployTransactionCfg
getModuleDeployTransactionCfg baseCfg txOpts moduleFile mWasmVersion = do
  wasmModule <- getWasmModuleFromFile moduleFile mWasmVersion
  txCfg <- getTransactionCfg baseCfg txOpts $ moduleDeployEnergyCost wasmModule
  return $ ModuleDeployTransactionCfg txCfg wasmModule

-- |Calculate the energy cost of deploying a module.
moduleDeployEnergyCost :: Wasm.WasmModule -> EncryptedSigningData -> IO (Maybe (Int -> Types.Energy))
moduleDeployEnergyCost wasmMod encSignData = pure . Just . const $
  Cost.deployModuleCost (fromIntegral payloadSize) + minimumCost payloadSize signatureCount
  where
        signatureCount = mapNumKeys (esdKeys encSignData)
        payloadSize = Types.payloadSize . Types.encodePayload . Types.DeployModule $ wasmMod

data ModuleDeployTransactionCfg =
  ModuleDeployTransactionCfg
  { -- |Configuration for the transaction.
    mdtcTransactionCfg :: !TransactionConfig
    -- |The WASM module to deploy.
  , mdtcModule :: !Wasm.WasmModule }

moduleDeployTransactionPayload :: ModuleDeployTransactionCfg -> Types.Payload
moduleDeployTransactionPayload ModuleDeployTransactionCfg {..} = Types.DeployModule mdtcModule

-- |Checks if the given receive name is valid and if so, returns it back
-- or otherwise a fallback receive name for v1 contracts.
checkAndGetContractReceiveName :: CI.ContractInfo -> Text -> IO Text
checkAndGetContractReceiveName contrInfo receiveName = do
  if CI.hasReceiveMethod receiveName contrInfo
    then return receiveName
    else
      if not $ CI.hasFallbackReceiveSupport contrInfo
        then confirmAbortOrContinue False
        else let (_, fallbackReceiveName) = Wasm.contractAndFunctionName $ Wasm.makeFallbackReceiveName $ Wasm.ReceiveName (CI.getContractName contrInfo <> receiveName) in
          if CI.hasReceiveMethod fallbackReceiveName contrInfo
          then do
            logInfo [[i|The contract does not have an entrypoint named '#{receiveName}'. The fallback entrypoint will be used instead.|]]
            return fallbackReceiveName
          else confirmAbortOrContinue True
  where
    confirmAbortOrContinue hasFallbackReceiveSupport = do
      let fallback_msg :: String = if hasFallbackReceiveSupport
                then " or a fallback entrypoint"
                else ""
      logWarn [[i|The contract does not have an entrypoint named '#{receiveName}'#{fallback_msg}.|]]
      confirmed <- askConfirmation $ Just "Do you still want to continue? "
      unless confirmed $ logFatal ["aborting..."]
      return receiveName

-- |Process a 'contract ...' command.
processContractCmd :: ContractCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processContractCmd action baseCfgDir verbose backend =
  case action of
    ContractList block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      res <- withClient backend $
        readBlockHashOrDefault Best block >>=
          getInstanceListV2 >>=
            getResponseValueOrFail
      runPrinter $ printContractList (bcContractNameMap baseCfg) (toList res)

    ContractShow indexOrName subindex schemaFile block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      namedContrAddr <- getNamedContractAddress (bcContractNameMap baseCfg) indexOrName subindex
      withClient backend $ do
        blockHash <-
          readBlockHashOrDefault Best block >>=
            getBlockInfoV2 >>=
              getResponseValueOrFail' Queries.biBlockHash
        contrInfo <- getContractInfo namedContrAddr (Given blockHash)
        let namedModRef = NamedModuleRef {nmrRef = CI.ciSourceModule contrInfo, nmrNames = findAllNamesFor (bcModuleNameMap baseCfg) (CI.ciSourceModule contrInfo)}
        schema <- getSchemaFromFileOrModule schemaFile namedModRef (Given blockHash)
        let namedOwner = NamedAddress {naAddr = CI.ciOwner contrInfo, naNames = findAllNamesFor (bcAccountNameMap baseCfg) (CI.ciOwner contrInfo)}
        displayContractInfo schema contrInfo namedOwner namedModRef

    ContractInit modTBD contrName paramsFile schemaFile contrAlias isPath mWasmVersion amount txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      ciCfg <- getContractInitTransactionCfg backend baseCfg txOpts modTBD isPath mWasmVersion contrName
                paramsFile schemaFile amount
      let txCfg = citcTransactionCfg ciCfg
      let energy = tcEnergy txCfg
      let expiryTs = tcExpiry txCfg

      let minEnergy = contractInitMinimumEnergy ciCfg (tcEncryptedSigningData txCfg)
      when (energy < minEnergy) $ logFatal [ "insufficient energy provided"
                                           , [iii|to verify the transaction signature #{showNrg minEnergy} is needed,
                                                  and additional energy is needed to complete the initialization|]]

      logInfo [ [i|initialize contract '#{contrName}' from module '#{citcModuleRef ciCfg}' with |]
                  ++ paramsMsg paramsFile ++ [i| Sending #{Types.amountToString $ citcAmount ciCfg} CCD.|]
              , [i|allowing up to #{showNrg energy} to be spent as transaction fee|]
              , [i|transaction expires on #{showTimeFormatted $ timeFromTransactionExpiryTime expiryTs}|]]

      when (ioConfirm . toInteractionOpts $ txOpts) $ do
         confirmed <- askConfirmation Nothing
         unless confirmed exitTransactionCancelled

      let intOpts = toInteractionOpts txOpts
      let pl = contractInitTransactionPayload ciCfg
      withClient backend $ do
        mTsr <- sendAndTailTransaction verbose txCfg (Types.encodePayload pl) intOpts
        case extractContractAddress mTsr of
          Nothing -> return ()
          Just (Left err) -> logFatal ["contract initialisation failed:", err]
          Just (Right contrAddr) -> do
            logSuccess [[i|contract successfully initialized with address: #{showCompactPrettyJSON contrAddr}|]]
            case contrAlias of
              Nothing -> return ()
              Just contrAlias' -> do
                nameAdded <- liftIO $ addContractNameAndWrite verbose baseCfg contrAlias' contrAddr
                logSuccess [[i|contract address #{showCompactPrettyJSON contrAddr} was successfully named '#{nameAdded}'|]]

    ContractUpdate indexOrName subindex receiveName paramsFile schemaFile amount txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      cuCfg <- getContractUpdateTransactionCfg backend baseCfg txOpts indexOrName subindex
                receiveName paramsFile schemaFile amount
      let txCfg = cutcTransactionCfg cuCfg
      let energy = tcEnergy txCfg
      let expiryTs = tcExpiry txCfg

      let minEnergy = contractUpdateMinimumEnergy cuCfg (tcEncryptedSigningData txCfg)
      when (energy < minEnergy) $ logFatal [ "insufficient energy provided"
                                           , [iii|to verify the transaction signature #{showNrg minEnergy} is needed,
                                                  and additional energy is needed to complete the update|]]

      logInfo [ [i|update contract '#{cutcContrName cuCfg}' using the function '#{receiveName}' with |]
                  ++ paramsMsg paramsFile ++ [i| Sending #{Types.amountToString $ cutcAmount cuCfg} CCD.|]
              , [i|allowing up to #{showNrg energy} to be spent as transaction fee|]
              , [i|transaction expires on #{showTimeFormatted $ timeFromTransactionExpiryTime expiryTs}|]]

      when (ioConfirm . toInteractionOpts $ txOpts) $ do
         confirmed <- askConfirmation Nothing
         unless confirmed exitTransactionCancelled

      let intOpts = toInteractionOpts txOpts
      let pl = contractUpdateTransactionPayload cuCfg
      withClient backend $ do
        mTsr <- sendAndTailTransaction verbose txCfg (Types.encodePayload pl) intOpts
        case extractUpdate mTsr of
          Nothing -> return ()
          Just (Left err) -> logFatal ["updating contract instance failed:", err]
          Just (Right _) -> do
            namedContrAddr <- getNamedContractAddress (bcContractNameMap baseCfg) indexOrName subindex
            logSuccess [[iii|successfully updated contract instance #{showNamedContractAddress namedContrAddr}
                                              using the function '#{receiveName}'|]]

    ContractInvoke indexOrName subindex receiveName parameterFile schemaFile amount invoker energy block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      namedContrAddr <- getNamedContractAddress (bcContractNameMap baseCfg) indexOrName subindex

      invoker' :: Maybe Types.Address <- case invoker of
        Nothing -> return Nothing
        Just (InvokerAccount nameOrAddr) -> Just . Types.AddressAccount . naAddr <$> getAccountAddressArg (bcAccountNameMap baseCfg) nameOrAddr
        Just InvokerContract{..} -> Just . Types.AddressContract . ncaAddr <$> getNamedContractAddress (bcContractNameMap baseCfg) icIndexOrName icSubindex

      (bbHash, contrInfo) <- withClient backend $ do
        bhInput <- readBlockHashOrDefault Best block
        bHash <- getResponseValueOrFail' Queries.biBlockHash =<< getBlockInfoV2 bhInput
        cInfo <- getContractInfo namedContrAddr (Given bHash)
        return (bHash, cInfo)
      let namedModRef = NamedModuleRef {nmrRef = CI.ciSourceModule contrInfo, nmrNames = []} -- Skip finding nmrNames, as they won't be shown.

      let contractName = CI.getContractName contrInfo
      let wasmReceiveName = Wasm.ReceiveName [i|#{contractName}.#{receiveName}|]
      updatedReceiveName <- checkAndGetContractReceiveName contrInfo receiveName

      modSchema <- withClient backend $ getSchemaFromFileOrModule schemaFile namedModRef (Given bbHash)
      wasmParameter <- getWasmParameter parameterFile modSchema (CS.ReceiveFuncName contractName updatedReceiveName)
      let nrg = fromMaybe (Types.Energy 10_000_000) energy

      let invokeContext = InvokeContract.ContractContext
                          { ccInvoker = invoker'
                          , ccContract = ncaAddr namedContrAddr
                          , ccAmount = amount
                          , ccMethod = wasmReceiveName
                          , ccParameter = wasmParameter
                          , ccEnergy = nrg
                          }

      -- VH/FIXME: Should the blockhash input in the following be `Best` or `Given bbHash`?
      res <- withClient backend $ getResponseValueOrFail =<< invokeInstanceV2 (Given bbHash) invokeContext
      case res of
          InvokeContract.Failure{..} -> do
            returnValueMsg <- mkReturnValueMsg rcrReturnValue schemaFile modSchema contractName updatedReceiveName True
            -- Logs in cyan to indicate that the invocation returned with a failure.
            -- This might be what you expected from the contract, so logWarn or logFatal should not be used.
            log Info (Just ANSI.Cyan) [[iii|Invocation resulted in failure:\n
                                              - Energy used: #{showNrg rcrUsedEnergy}\n
                                              - Reason: #{showRejectReason verbose rcrReason}
                                              #{returnValueMsg}|]]
          InvokeContract.Success{..} -> do
            let eventsMsg = case mapMaybe (fmap (("  - " <>) . Text.pack) . showEvent verbose Nothing) rcrEvents of
                              [] -> Text.empty
                              evts -> [i|- Events:\n#{Text.intercalate "\n" evts}|]
            returnValueMsg <- mkReturnValueMsg rcrReturnValue schemaFile modSchema contractName updatedReceiveName False
            logSuccess [[iii|Invocation resulted in success:\n
                              - Energy used: #{showNrg rcrUsedEnergy}
                              #{returnValueMsg}
                              #{eventsMsg}
                              |]]

    ContractName index subindex contrName -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      let contrAddr = mkContractAddress index subindex
      nameAdded <- liftIO $ addContractNameAndWrite verbose baseCfg contrName contrAddr
      logSuccess [[i|contract address #{showCompactPrettyJSON contrAddr} was successfully named '#{nameAdded}'|]]

    ContractRemoveName name -> do
        baseCfg <- getBaseConfig baseCfgDir verbose
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        let nameMap = bcContractNameMap baseCfg
        case Map.lookup name nameMap of
          Nothing -> logFatal [[i|the name '#{name}' is not in use|]]
          Just currentAddr -> do
            logInfo [[i|removing mapping from '#{name}' to contract address '#{currentAddr}'|]]
            void $ removeContractNameAndWrite baseCfg name verbose

  where extractContractAddress = extractFromTsr (\case
                                                 Types.ContractInitialized {..} -> Just ecAddress
                                                 _ -> Nothing)

        -- |A successful contract update will contain at least one Updated event
        --  and zero or more Interrupted, Resumed, and Transferred events.
        extractUpdate = extractFromTsr (\case
                                        Types.Updated {} -> Just ()
                                        Types.Interrupted {} -> Just ()
                                        Types.Resumed {} -> Just ()
                                        Types.Transferred {} -> Just ()
                                        _ -> Nothing)
        paramsMsg = \case
            Nothing -> "no parameters."
            Just (ParameterBinary binFile) -> [i|binary parameters from '#{binFile}'.|]
            Just (ParameterJSON jsonFile) -> [i|JSON parameters from '#{jsonFile}'.|]

        -- |Calculates the minimum energy required for checking the signature of a contract initialization.
        -- The minimum will not cover the full initialization, but enough of it, so that a potential 'Not enough energy' error
        -- can be shown.
        contractInitMinimumEnergy :: ContractInitTransactionCfg -> EncryptedSigningData -> Types.Energy
        contractInitMinimumEnergy ContractInitTransactionCfg{..} encSignData = minimumCost (fromIntegral payloadSize) signatureCount
          where
            payloadSize =    1 -- tag
                          + 32 -- module ref
                          +  2 + (length $ show citcInitName) -- size length + length of initName
                          +  2 + (BSS.length . Wasm.parameter $ citcParams) -- size length + length of parameter
            signatureCount = mapNumKeys (esdKeys encSignData)

        -- |Calculates the minimum energy required for checking the signature of a contract update.
        -- The minimum will not cover the full update, but enough of it, so that a potential 'Not enough energy' error
        -- can be shown.
        contractUpdateMinimumEnergy :: ContractUpdateTransactionCfg -> EncryptedSigningData -> Types.Energy
        contractUpdateMinimumEnergy ContractUpdateTransactionCfg{..} encSignData = minimumCost (fromIntegral payloadSize) signatureCount
          where
            payloadSize =    1 -- tag
                          + 16 -- contract address
                          +  2 + (length $ show cutcReceiveName) -- size length + length of receiveName
                          +  2 + (BSS.length . Wasm.parameter $ cutcParams) -- size length + length of the parameter
            signatureCount = mapNumKeys (esdKeys encSignData)

        -- |Construct a message for displaying the return value of a smart contract invocation.
        --  The 'isError' parameter determines whether the returned bytes should be parsed with the error schema or the return value schema.
        mkReturnValueMsg :: Maybe BS.ByteString -> Maybe FilePath -> Maybe CS.ModuleSchema -> Text -> Text -> Bool -> IO Text
        mkReturnValueMsg rvBytes schemaFile modSchema contractName receiveName isError = case rvBytes of
          Nothing -> return Text.empty
          Just rv -> case modSchema >>= \modSchema' -> lookupSchema modSchema' (CS.ReceiveFuncName contractName receiveName) of
            Nothing -> return [i|\n - #{valueType} value (raw):\n  #{BS.unpack rv}\n|] -- Schema not provided or it doesn't contain the return value for this func.
            Just schemaForFunc -> case S.runGet (CP.getJSONUsingSchema schemaForFunc) rv of
              Left err -> do
                logWarn [[i|Could not parse the returned bytes using the schema:\n#{err}|]]
                if isJust schemaFile
                  then logWarn ["Make sure you supply the correct schema for the contract with --schema"]
                  else logWarn ["Try supplying a schema file with --schema"]
                return [i|\n - #{valueType} value (raw):\n  #{BS.unpack rv}\n|]
              Right rvJSON -> return [i|\n - #{valueType} value:\n#{indentBy 6 $ showPrettyJSON rvJSON}\n|]
          where
              (lookupSchema, valueType) = if isError then (CS.lookupErrorSchema, "Error"::Text) else (CS.lookupReturnValueSchema, "Return"::Text)

-- |Try to fetch info about the contract and deserialize it from JSON.
-- Or, log fatally with appropriate error messages if anything goes wrong.
getContractInfo :: (MonadIO m) => NamedContractAddress -> BlockHashInput -> ClientMonad m CI.ContractInfo
getContractInfo namedContrAddr block = do
  bh <- readOrFail block
  blockRes <- getBlockInfoV2 (Given bh)
  case blockRes of
    StatusOk _ -> return ()
    StatusNotOk (NOT_FOUND, _) -> logFatal [[i|block #{block} does not exist|]] 
    StatusNotOk (status, err) -> logFatal [[i|GRPC response with status '#{status}': #{err}|]] 
    StatusInvalid -> logFatal ["GRPC response contained an invalid status code."]
    RequestFailed err -> logFatal [[i|I/O error: #{err}|]]
  res <- getInstanceInfoV2 (ncaAddr namedContrAddr) (Given bh)
  case res of
    StatusOk resp -> case grpcResponseVal resp of
      Left err -> logFatal [[i|Could not decode contract info: #{err}|]] 
      Right v -> return $ instanceInfoToContractInfo v
    StatusNotOk (NOT_FOUND, _) -> logFatal [[i|the contract instance #{showNamedContractAddress namedContrAddr} does not exist in block #{block}|]] 
    StatusNotOk (status, err) -> logFatal [[i|GRPC response with status '#{status}': #{err}|]] 
    StatusInvalid -> logFatal ["GRPC response contained an invalid status code."]
    RequestFailed err -> logFatal [[i|I/O error: #{err}|]]

-- |Display contract info, optionally using a schema to decode the contract state.
displayContractInfo :: Maybe CS.ModuleSchema -> CI.ContractInfo -> NamedAddress -> NamedModuleRef -> ClientMonad IO ()
displayContractInfo schema contrInfo namedOwner namedModRef = do
  cInfo <- case schema of
    Just schema' -> do
      mContrInfo <- CI.addSchemaData contrInfo schema'
      case mContrInfo of
        Nothing -> return contrInfo -- Adding schema data failed, just return the regular contract info.
        Just infoWithSchemaData -> return infoWithSchemaData
    Nothing -> return contrInfo
  runPrinter $ printContractInfo cInfo namedOwner namedModRef

-- |Attempts to acquire the needed parts for updating a contract.
-- The two primary parts are a contract address, which is acquired using `getNamedContractAddress`,
-- and a `Wasm.Parameter` which is acquired using `getWasmParameter`.
-- It will log fatally if one of the two cannot be acquired.
getContractUpdateTransactionCfg :: Backend
                                -> BaseConfig
                                -> TransactionOpts Types.Energy
                                -> Text -- ^ Index of the contract address OR a contract name.
                                -> Maybe Word64 -- ^ Optional subindex.
                                -> Text -- ^ Name of the receive function to use.
                                -> Maybe ParameterFileInput -- ^ Optional parameter file.
                                -> Maybe FilePath -- ^ Optional schema file.
                                -> Types.Amount   -- ^ `Amount` to send to the contract.
                                -> IO ContractUpdateTransactionCfg
getContractUpdateTransactionCfg backend baseCfg txOpts indexOrName subindex receiveName
                                paramsFile schemaFile amount = do
  txCfg <- getRequiredEnergyTransactionCfg baseCfg txOpts
  namedContrAddr <- getNamedContractAddress (bcContractNameMap baseCfg) indexOrName subindex
  (bbHash, contrInfo) <- withClient backend $ do
    b <- getResponseValueOrFail' Queries.biBlockHash =<< getBlockInfoV2 Best  
    cInfo <- getContractInfo namedContrAddr (Given b)
    return (b, cInfo)
  updatedReceiveName <- checkAndGetContractReceiveName contrInfo receiveName
  let namedModRef = NamedModuleRef {nmrRef = CI.ciSourceModule contrInfo, nmrNames = []}
  let contrName = CI.getContractName contrInfo
  schema <- withClient backend $ getSchemaFromFileOrModule schemaFile namedModRef (Given bbHash)
  params <- getWasmParameter paramsFile schema (CS.ReceiveFuncName contrName updatedReceiveName)
  return $ ContractUpdateTransactionCfg txCfg (ncaAddr namedContrAddr)
          contrName (Wasm.ReceiveName [i|#{contrName}.#{receiveName}|]) params amount

contractUpdateTransactionPayload :: ContractUpdateTransactionCfg -> Types.Payload
contractUpdateTransactionPayload ContractUpdateTransactionCfg {..} =
  Types.Update cutcAmount cutcAddress cutcReceiveName cutcParams

data ContractUpdateTransactionCfg =
  ContractUpdateTransactionCfg
  { -- |Configuration for the transaction.
    cutcTransactionCfg :: !TransactionConfig
    -- |The address of the contract to invoke.
  , cutcAddress :: !Types.ContractAddress
    -- |Name of the contract that is being updated.
    -- This is resolved from the chain.
  , cutcContrName :: !Text
    -- |Name of the receive method to invoke.
  , cutcReceiveName :: !Wasm.ReceiveName
    -- |Parameters to the receive method.
  , cutcParams :: !Wasm.Parameter
    -- |Amount to transfer to the contract.
  , cutcAmount :: !Types.Amount
  }

-- |Attempts to acquire the needed parts for initializing a contract.
-- The two primary parts are a module reference, which can be acquired in one of three ways
-- (see the arguments for details), and a `Wasm.Parameter`, which is acquired using `getWasmParameter`.
-- It will log fatally if one of the two cannot be acquired.
getContractInitTransactionCfg :: Backend
                              -> BaseConfig
                              -> TransactionOpts Types.Energy
                              -> String -- ^ Module reference OR module name OR (if isPath == True) path to the module (reference then calculated by hashing).
                              -> Bool   -- ^ isPath: if True, the previous argument is assumed to be a path.
                              -> Maybe Wasm.WasmVersion -- ^ Optional WasmVersion for the module file.
                              -> Text   -- ^ Name of contract to init.
                              -> Maybe ParameterFileInput -- ^ Optional parameter file.
                              -> Maybe FilePath -- ^ Optional schema file.
                              -> Types.Amount   -- ^ `Amount` to send to the contract.
                              -> IO ContractInitTransactionCfg
getContractInitTransactionCfg backend baseCfg txOpts modTBD isPath mWasmVersion contrName paramsFile schemaFile amount = do
  namedModRef <- if isPath
            then (\ref -> NamedModuleRef {nmrRef = ref, nmrNames = []}) <$> getModuleRefFromFile modTBD mWasmVersion
            else getNamedModuleRef (bcModuleNameMap baseCfg) (Text.pack modTBD)
  txCfg <- getRequiredEnergyTransactionCfg baseCfg txOpts
  schema <- withClient backend $ getSchemaFromFileOrModule schemaFile namedModRef Best
  params <- getWasmParameter paramsFile schema (CS.InitFuncName contrName)
  return $ ContractInitTransactionCfg txCfg amount (nmrRef namedModRef) (Wasm.InitName [i|init_#{contrName}|]) params

-- |Query the node for a module reference, and parse the result.
-- Terminate program execution if either the module cannot be obtained,
-- or the result cannot be parsed.
getWasmModule :: (MonadIO m)
              => NamedModuleRef -- ^On-chain reference of the module.
              -> BlockHashInput -- ^The block to query in.
              -> ClientMonad m Wasm.WasmModule
getWasmModule namedModRef block = do
  bh <- readOrFail block
  blockRes <- getBlockInfoV2 (Given bh)
  case blockRes of
    StatusOk _ -> return ()
    StatusNotOk (NOT_FOUND, _) -> logFatal [[i|Block #{block} does not exist|]] 
    StatusNotOk (status, err) -> logFatal [[i|GRPC response with status '#{status}': #{err}|]] 
    StatusInvalid -> logFatal ["GRPC response contained an invalid status code."]
    RequestFailed err -> logFatal [[i|I/O error: #{err}|]]
  res <- getModuleSourceV2 (nmrRef namedModRef) (Given bh)
  case res of
    StatusOk resp -> case grpcResponseVal resp of
      Left err -> logFatal ["Could not decode Wasm Module: " <> err]
      Right v -> return v
    StatusNotOk (NOT_FOUND, _) -> logFatal [[i|The module reference #{showNamedModuleRef namedModRef} does not exist in block #{block}|]] 
    StatusNotOk (status, err) -> logFatal [[i|GRPC response with status '#{status}': #{err}|]] 
    StatusInvalid -> logFatal ["GRPC response contained an invalid status code."]
    RequestFailed err -> logFatal [[i|I/O error: #{err}|]]

data ContractInitTransactionCfg =
  ContractInitTransactionCfg
  { -- |Configuration for the transaction.
    citcTransactionCfg :: !TransactionConfig
    -- |Initial amount on the contract's account.
  , citcAmount :: !Types.Amount
    -- |Reference of the module (on-chain) in which the contract exist.
  , citcModuleRef :: !Types.ModuleRef
    -- |Name of the init method to invoke in that module.
  , citcInitName :: !Wasm.InitName
    -- |Parameters to the init method.
  , citcParams :: !Wasm.Parameter
  }

contractInitTransactionPayload :: ContractInitTransactionCfg -> Types.Payload
contractInitTransactionPayload ContractInitTransactionCfg {..} =
  Types.InitContract citcAmount citcModuleRef citcInitName citcParams

-- |Load a WasmModule from the specified file path.
-- The module will be prefixed with the wasmVersion and moduleSize if wasmVersion is provided.
-- This enables the use of wasm modules compiled with cargo-concordium version < 2, and modules compiled
-- without cargo-concordium version >= 2.
getWasmModuleFromFile :: FilePath -- ^ The module file.
                      -> Maybe Wasm.WasmVersion -- ^ Optional wasmVersion.
                      -> IO Wasm.WasmModule
getWasmModuleFromFile moduleFile mWasmVersion = do
  source <- handleReadFile BS.readFile moduleFile >>= \source ->
    case mWasmVersion of
      Nothing -> return source -- Module should already be prefixed with the wasmVersion and moduleSize.
      Just wasmVersion -> let wasmVersionBytes = S.encode wasmVersion
                              moduleSizeBytes = S.encode ((fromIntegral $ BS.length source) :: Word32)
                          in  return $ wasmVersionBytes <> moduleSizeBytes <> source -- Prefix the wasmVersion and moduleSize.
  case S.decode source of
    Right wm -> if hasValidWasmMagic wm
                then return wm
                else logFatal $ [[iii|Supplied file '{moduleFile}' cannot be parsed as a smart contract module to be deployed:\n
                                 The module does not have a valid wasm magic value.|]]
                                 ++ if isJust mWasmVersion
                                    then ["This is /very/ likely because you used the flag '--wasm-version' on a module \
                                          \created with cargo-concordium version >= 2.\n\
                                          \Try again without the '--wasm-version' flag."]
                                    else []
    Left err -> logFatal [[i|Supplied file '#{moduleFile}' cannot be parsed as a smart contract module to be deployed:\n#{err}|]]
  where
    hasValidWasmMagic :: Wasm.WasmModule -> Bool
    hasValidWasmMagic wm = S.runGet getMagicBytes (Wasm.wasmSource wm) == Right wasmMagicValue
      where wasmMagicValue = BS.pack [0x00, 0x61, 0x73, 0x6D]
            getMagicBytes = S.getByteString 4


-- |Load `Wasm.Parameter` through one of several ways, dependent on the arguments:
--   * If binary file provided -> Read the file and wrap its contents in `Wasm.Parameter`.
--   * If JSON file provided   -> Try to use the schema to encode the parameters into a `Wasm.Parameter`.
-- If invalid arguments are provided or something fails, appropriate warning or error messages are logged.
getWasmParameter :: Maybe ParameterFileInput -- ^ Optional parameter file in JSON or binary format.
                 -> Maybe CS.ModuleSchema -- ^ Optional module schema.
                 -> CS.FuncName -- ^ A func name used for finding the func signature in the schema.
                 -> IO Wasm.Parameter
getWasmParameter paramsFile schema funcName =
  case paramsFile of
    Nothing -> emptyParams
    Just (ParameterJSON jsonParamFile) -> case schema of
      Nothing -> logFatal [[iii|could not parse the json parameter because a no schema was embedded in the module or provided with the --schema flag|]]
      Just schema' -> getFromJSONParams jsonParamFile schema'
    Just (ParameterBinary binaryParamFile) -> binaryParams binaryParamFile
  where getFromJSONParams :: FilePath -> CS.ModuleSchema -> IO Wasm.Parameter
        getFromJSONParams jsonFile schema' = case CS.lookupParameterSchema schema' funcName of
          Nothing -> logFatal [[i|The JSON parameter could not be used because there was no schema for it.|]]
          Just schemaForParams -> do
            jsonFileContents <- handleReadFile BSL8.readFile jsonFile
            let params = AE.eitherDecode jsonFileContents >>= CP.serializeWithSchema schemaForParams
            case params of
              Left errParams -> logFatal [[i|Could not decode parameters from file '#{jsonFile}' as JSON:|], errParams]
              Right params' -> pure . Wasm.Parameter . BS.toShort $ params'
        emptyParams = pure . Wasm.Parameter $ BSS.empty
        binaryParams file = Wasm.Parameter . BS.toShort <$> handleReadFile BS.readFile file

-- |Get a schema from a file or, alternatively, try to extract an embedded schema from a module.
-- The schema from the file will take precedence over an embedded schema in the module.
--
-- Can logWarn and logFatal in the following situations:
--   - Invalid schemafile: logs fatally.
--   - No schemafile and invalid embedded schema: logs a warning and returns `Nothing`.
getSchemaFromFileOrModule :: (MonadIO m)
                          => Maybe FilePath -- ^ Optional schema file.
                          -> NamedModuleRef -- ^ A reference to a module on chain.
                          -> BlockHashInput -- ^ A block hash.
                          -> ClientMonad m (Maybe CS.ModuleSchema)
getSchemaFromFileOrModule schemaFile namedModRef block = do
  wasmModule <- getWasmModule namedModRef block
  case schemaFile of
    Nothing -> do
      liftIO $ case CS.decodeEmbeddedSchema wasmModule of
        Left err -> do
          logWarn [[i|Could not parse embedded schema from module:|], err]
          return Nothing
        Right schema -> return schema
    Just schemaFile' -> liftIO (Just <$> getSchemaFromFile (Wasm.wasmVersion wasmModule) schemaFile')

-- |Try to load and decode a schema from a file. Logs fatally if the file is not a valid Wasm module.
getSchemaFromFile :: Wasm.WasmVersion -> FilePath -> IO CS.ModuleSchema
getSchemaFromFile wasmVersion schemaFile = do
  schema <- CS.decodeModuleSchema wasmVersion <$> handleReadFile BS.readFile schemaFile
  case schema of
    Left err -> logFatal [[i|Could not decode schema from file '#{schemaFile}':|], err]
    Right schema' -> pure schema'

-- |Get a schema and a list of exported function names from an optional schema file and a module.
-- Logs fatally if an invalid schema is found (either from a file or embedded).
-- The schema from the file will take precedence over an embedded schema in the module.
-- It will only return `(Nothing, _)` if no schemafile is provided and no embedded schema was found in the module.
getSchemaAndExports :: Maybe FilePath -- ^ Optional schema file.
                    -> Wasm.WasmModule -- ^ Module used for looking up a schema and exports.
                    -> IO (Maybe CS.ModuleSchema, [Text])
getSchemaAndExports schemaFile wasmModule = do
  preferredSchema <- case schemaFile of
    Nothing -> return Nothing
    Just schemaFile' -> fmap Just . getSchemaFromFile (Wasm.wasmVersion wasmModule) $ schemaFile'
  (schema, exports) <- getSchemaAndExportsOrDie

  if isJust preferredSchema
  then return (preferredSchema, exports)
  else return (schema, exports)

  where getSchemaAndExportsOrDie = case CS.decodeEmbeddedSchemaAndExports wasmModule of
          Left err -> logFatal [[i|Could not parse embedded schema or exports from module:|], err]
          Right schemaAndExports -> return schemaAndExports

-- |Try to parse the input as a module reference and assume it is a path if it fails.
getModuleRefFromRefOrFile :: String -> Maybe Wasm.WasmVersion -> IO Types.ModuleRef
getModuleRefFromRefOrFile modRefOrFile mWasmVersion = case readMaybe modRefOrFile of
  Just modRef -> pure modRef
  Nothing -> getModuleRefFromFile modRefOrFile mWasmVersion

-- |Load the module file and compute its hash, which is the reference.
getModuleRefFromFile :: String -> Maybe Wasm.WasmVersion -> IO Types.ModuleRef
getModuleRefFromFile file mWasmVersion = Types.ModuleRef . getHash <$> getWasmModuleFromFile file mWasmVersion

-- |Get a NamedContractAddress from either a name or index and an optional subindex.
-- LogWarn if subindex is provided with a contract name.
-- LogFatal if it is neither an index nor a contract name.
getNamedContractAddress :: MonadIO m => ContractNameMap -> Text -> Maybe Word64 -> m NamedContractAddress
getNamedContractAddress nameMap indexOrName subindex = case readMaybe $ Text.unpack indexOrName of
  Just index -> return $ NamedContractAddress {ncaAddr = mkContractAddress index subindex, ncaNames = []}
  Nothing -> do
    when (isJust subindex) $ logWarn ["ignoring the --subindex as it should not be used in combination with a contract name"]
    case Map.lookup indexOrName nameMap of
      Just addr -> return $ NamedContractAddress {ncaAddr = addr, ncaNames = [indexOrName]}
      Nothing -> logFatal [[i|'#{indexOrName}' is neither the address index nor the name of a contract|]]

-- |Get a NamedModuleRef from either a name or a module reference.
-- LogFatal if it is neither a module reference nor a module name.
getNamedModuleRef :: MonadIO m => ModuleNameMap -> Text -> m NamedModuleRef
getNamedModuleRef nameMap modRefOrName = case readMaybe $ Text.unpack modRefOrName of
  Just modRef -> return $ NamedModuleRef {nmrRef = modRef, nmrNames = []}
  Nothing -> case Map.lookup modRefOrName nameMap of
    Just modRef -> return $ NamedModuleRef {nmrRef = modRef, nmrNames = [modRefOrName]}
    Nothing -> logFatal [[i|'#{modRefOrName}' is neither the reference nor the name of a module|]]

-- |Make a contract address from an index and an optional subindex (default: 0).
mkContractAddress :: Word64 -> Maybe Word64 -> Types.ContractAddress
mkContractAddress index subindex = Types.ContractAddress (Types.ContractIndex index) (Types.ContractSubindex subindex')
  where subindex' = fromMaybe 0 subindex

-- |Try to extract event information from a TransactionStatusResult.
-- The Maybe returned by the supplied function is mapped to Either with an error message.
-- 'Nothing' is mapped to 'Nothing'
extractFromTsr :: (Types.Event -> Maybe a) -> Maybe TransactionStatusResult -> Maybe (Either String a)
extractFromTsr _ Nothing = Nothing -- occurs when ioTail is disabled.
extractFromTsr eventMatcher (Just tsr) = Just $ case parseTransactionBlockResult tsr of
  SingleBlock _ tSummary -> getEvents tSummary >>= maybeToRight "transaction not included in any blocks" . findModRef
  NoBlocks -> Left "transaction not included in any blocks"
  _ -> Left "internal server: Finalized chain has split"
  where
    getEvents tSum = case Types.tsResult tSum of
                Types.TxSuccess {..} -> Right vrEvents
                Types.TxReject {..}  -> Left $ showRejectReason True vrRejectReason
    findModRef = foldr (\e _ -> eventMatcher e) Nothing

    maybeToRight _ (Just x) = Right x
    maybeToRight y Nothing  = Left y

-- |Process a 'consensus ...' command.
processConsensusCmd :: ConsensusCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processConsensusCmd action _baseCfgDir verbose backend =
  case action of
    ConsensusStatus -> do
      v <- withClient backend $ getResponseValueOrFail =<< getConsensusInfoV2
      runPrinter $ printConsensusStatus v

    ConsensusShowParameters b includeBakers -> do
      baseCfg <- getBaseConfig _baseCfgDir verbose
      p <- withClient backend $
        readBlockHashOrDefault Best b >>=
          getElectionInfoV2 >>=
            getResponseValueOrFail
      let addrMap = Map.fromList . map Tuple.swap . Map.toList $ bcAccountNameMap baseCfg
      runPrinter $ printBirkParameters includeBakers p addrMap

    ConsensusShowChainParameters b -> do
      cParams <- withClient backend $
        readBlockHashOrDefault Best b >>=
          getBlockChainParametersV2 >>=
            getResponseValueOrFail
      runPrinter $ printChainParameters cParams

    ConsensusChainUpdate rawUpdateFile keysFiles intOpts -> do
      let
        loadJSON :: (FromJSON a) => FilePath -> IO a
        loadJSON fn = AE.eitherDecodeFileStrict fn >>= \case
          Left err -> logFatal [fn ++ ": " ++ err]
          Right r -> return r
      rawUpdate@Updates.RawUpdateInstruction{..} <- loadJSON rawUpdateFile
      Queries.EChainParametersAndKeys{..} <- withClient backend $ getResponseValueOrFail =<< getBlockChainParametersV2 Best
      let keyCollectionStore = ecpKeys
      keys <- mapM loadJSON keysFiles
      let
        (keySet, th) = Updates.extractKeysIndices ruiPayload keyCollectionStore
        keyLU :: SigScheme.KeyPair -> IO (Word16, SigScheme.KeyPair)
        keyLU key =
          let vk = SigScheme.correspondingVerifyKey key in
            case ruiPayload of
              Updates.RootUpdatePayload{} -> case vk `Vec.elemIndex` (Updates.hlkKeys . Updates.rootKeys $ keyCollectionStore) of
                                               Nothing -> logFatal [printf "Current key collection at best block does not contain public key '%s'" (show vk)]
                                               Just idx -> return (fromIntegral idx, key)
              Updates.Level1UpdatePayload{} -> case vk `Vec.elemIndex` (Updates.hlkKeys . Updates.level1Keys $ keyCollectionStore) of
                                                 Nothing -> logFatal [printf "Current key collection at best block does not contain public key '%s'" (show vk)]
                                                 Just idx -> return (fromIntegral idx, key)
              _ -> case vk `Vec.elemIndex` (Updates.asKeys . Updates.level2Keys $ keyCollectionStore) of
                     Nothing -> logFatal [printf "Current key collection at best block does not contain public key '%s'" (show vk)]
                     Just idx -> do
                       unless (fromIntegral idx `Set.member` keySet) $
                         logWarn [printf "Key with index %u (%s) is not authorized to perform this update type." idx (show vk)]
                       return (fromIntegral idx, key)
      when (length keys < fromIntegral th) $
        logFatal [printf "Not enough keys provided for signing this operation, got %u, need %u" (length keys) (fromIntegral th :: Int)]
      keyMap <- Map.fromList <$> mapM keyLU keys
      let ui = Updates.makeUpdateInstruction rawUpdate keyMap
      when verbose $ logInfo ["Generated update instruction:", show ui]
      now <- getCurrentTimeUnix
      let expiryOK = ruiTimeout > now
      unless expiryOK $
        logWarn [printf "Update timeout (%s) has already expired" (showTimeFormatted $ timeFromTransactionExpiryTime ruiTimeout)]
      let effectiveTimeOK = ruiEffectiveTime == 0 || ruiEffectiveTime > ruiTimeout
      unless effectiveTimeOK $
        logWarn [printf "Update effective time (%s) is not later than expiry time (%s)" (showTimeFormatted $ timeFromTransactionExpiryTime ruiEffectiveTime) (showTimeFormatted $ timeFromTransactionExpiryTime ruiTimeout)]
      let authorized = Updates.checkAuthorizedUpdate keyCollectionStore ui
      unless authorized $ do
        logWarn ["The update instruction is not authorized by the keys used to sign it."]
      when (ioConfirm intOpts) $ unless (expiryOK && effectiveTimeOK && authorized) $ do
        confirmed <- askConfirmation $ Just "Proceed anyway? Confirm"
        unless confirmed exitTransactionCancelled
      withClient backend $ do
        let
          tx = Types.ChainUpdate ui
          hash = getBlockItemHash tx
        _ <- sendBlockItemV2 tx >>=
          getResponseValueOrFail
        when (ioTail intOpts) $
          tailTransaction_ verbose hash

-- |Process a 'block ...' command.
processBlockCmd :: BlockCmd -> Verbose -> Backend -> IO ()
processBlockCmd action _ backend =
  case action of
    BlockShow b -> do
      bHash <- readBlockHashOrDefault Best b
      withClient backend $
        getBlockInfoV2 bHash>>=
          getResponseValueOrFail >>=
            -- VH/FIXME: Output changes slightly due to V2 API - document?
            -- Specifically `printBlockInfo` prints `Block not found` when
            -- its input is @Nothing@, which is never the case here.
            -- Instead, `Error: A GRPC error occurred: gRPC error: block not found.``
            -- is printed.
            runPrinter . printBlockInfo . Just

-- |Generate a fresh set of baker keys.
generateBakerKeys :: Maybe Types.BakerId -> IO BakerKeys
generateBakerKeys bkBakerId = do
  -- Aggr/bls keys.
  aggrSk <- Bls.generateSecretKey
  let aggrPk = Bls.derivePublicKey aggrSk
  -- Election keys.
  VRF.KeyPair {privateKey=elSk, publicKey=elPk} <- VRF.newKeyPair
  -- Signature keys.
  BlockSig.KeyPair {signKey=sigSk, verifyKey=sigVk} <- BlockSig.newKeyPair
  return BakerKeys { bkAggrSignKey = aggrSk
                   , bkAggrVerifyKey = aggrPk
                   , bkElectionSignKey = elSk
                   , bkElectionVerifyKey = elPk
                   , bkSigSignKey = sigSk
                   , bkSigVerifyKey = sigVk
                   , ..}

-- General function to fetch NrgGtu rate
-- This functionality is going to be more generally used in an upcoming branch adding CCD prices to NRG printouts across the client.
getNrgGtuRate :: ClientMonad IO Types.EnergyRate
getNrgGtuRate = do
  blockSummary <- getFromJson' =<< withLastFinalBlockHash Nothing getBlockSummary
  case blockSummary of
    Nothing -> do
      logError ["Failed to retrieve NRG-CCD rate from the chain using best block hash, unable to estimate CCD cost"]
      logError ["Falling back to default behaviour, all CCD values derived from NRG will be set to 0"]
      return 0
    Just bs ->
      return $ Queries.bsWithUpdates bs $ \_ ups ->
          ups ^. Types.currentParameters ^. energyRate

-- |Process the 'baker configure ...' command.
processBakerConfigureCmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> Bool -- ^Whether this was called via `baker configure`
  -> Maybe Types.Amount -- ^New stake/capital.
  -> Maybe Bool -- ^Select whether to restake earnings.
  -> Maybe Types.OpenStatus -- ^Open for delegation status.
  -> Maybe String -- ^URL to metadata about baker.
  -> Maybe Types.AmountFraction -- ^Transaction fee commission.
  -> Maybe Types.AmountFraction -- ^Baking reward commission.
  -> Maybe Types.AmountFraction -- ^Finalization commission.
  -> Maybe FilePath -- ^File to read baker keys from.
  -> Maybe FilePath -- ^File to write baker keys to.
  -> IO ()
processBakerConfigureCmd baseCfgDir verbose backend txOpts isBakerConfigure cbCapital cbRestakeEarnings cbOpenForDelegation metadataURL cbTransactionFeeCommission cbBakingRewardCommission cbFinalizationRewardCommission inputKeysFile outputKeysFile = do
  let intOpts = toInteractionOpts txOpts
  (bakerKeys, txCfg, pl) <- transactionForBakerConfigure (ioConfirm intOpts)
  withClient backend $ do
    when isBakerConfigure $ warnAboutMissingAddBakerParameters txCfg
    mapM_ (warnAboutBadCapital txCfg) cbCapital
    result <- sendAndTailTransaction verbose txCfg pl intOpts
    events <- eventsFromTransactionResult result
    mapM_ (tryPrintKeyUpdateEventToOutputFile bakerKeys) events
  where
    warnAboutMissingAddBakerParameters txCfg = do
      let allPresent = case (cbOpenForDelegation, metadataURL, cbTransactionFeeCommission, cbBakingRewardCommission, cbFinalizationRewardCommission, cbRestakeEarnings, cbCapital, inputKeysFile) of
                (Just _, Just _, Just _, Just _, Just _, Just _, Just _, Just _) -> True
                _ -> False
      when (not allPresent) $ do
        let senderAddr = naAddr . esdAddress . tcEncryptedSigningData $ txCfg
        Types.AccountInfo{..} <- getAccountInfoOrDie senderAddr Best
        case aiStakingInfo of
          Types.AccountStakingBaker{} -> return ()
          _ -> do
            logWarn $ [init ("To add a baker, more options are necessary. The following are missing "
                  ++ (if isNothing cbCapital then "\n--stake," else "")
                  ++ (if isNothing cbOpenForDelegation then "\n--open-delegation-for," else "")
                  ++ (if isNothing inputKeysFile then "\n--keys-in," else "")
                  ++ (if isNothing metadataURL then "\n--baker-url," else "")
                  ++ (if isNothing cbTransactionFeeCommission then "\n--delegation-transaction-fee-commission," else "")
                  ++ (if isNothing cbBakingRewardCommission then "\n--delegation-baking-commission," else "")
                  ++ (if isNothing cbFinalizationRewardCommission then "\n--delegation-finalization-commission," else ""))
                  ++ (if isNothing cbRestakeEarnings then ". \nExactly one of the options --restake and --no-restake must be present" else "")]
            confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
            unless confirmed exitTransactionCancelled
    askUntilEqual credentials = do
      pwd <- askPassword "Enter password for encryption of baker credentials (leave blank for no encryption): "
      case Password.getPassword pwd of
        "" -> do
          logInfo [ printf "Empty password, not encrypting baker credentials" ]
          return $ AE.encodePretty credentials
        _ -> do
          pwd2 <- askPassword "Re-enter password for encryption of baker credentials: "
          if pwd == pwd2 then
            AE.encodePretty <$> Password.encryptJSON Password.AES256 Password.PBKDF2SHA256 credentials pwd
          else do
            logWarn ["The two passwords were not equal. Try again"]
            askUntilEqual credentials

    printToOutputFile credentials out = liftIO $ do
      credentialsMaybeEncrypted <- askUntilEqual credentials
      void $ handleWriteFile BSL.writeFile PromptBeforeOverwrite verbose out credentialsMaybeEncrypted

    printToOutputFileIfJust bakerKeys bakerId =
      case (outputKeysFile, bakerKeys) of
        (Just outFile, Just (keys, _)) ->
          printToOutputFile BakerCredentials{bcKeys = keys, bcIdentity = bakerId} outFile
        _ ->
          let encrypted = fmap snd bakerKeys == Just True
          in unless encrypted $
            case inputKeysFile of
              Nothing ->
                logInfo [printf "To use it add \"bakerId\": %s to the keys file." (show bakerId)]
              Just inf ->
                logInfo [printf "To use it add \"bakerId\": %s to the keys file %s." (show bakerId) inf]

    eventsFromTransactionResult Nothing = return []
    eventsFromTransactionResult (Just result) = do
      case tsrState result of
        Finalized | SingleBlock _ summary <- parseTransactionBlockResult result -> do
          case Types.tsResult summary of
            Types.TxReject reason -> logWarn [showRejectReason True reason] >> return []
            Types.TxSuccess es -> return es
        Absent ->
          logFatal ["Transaction is absent."]
        _ ->
          logFatal ["Unexpected status."]

    tryPrintKeyUpdateEventToOutputFile bakerKeys Types.BakerAdded{..} = do
      logInfo ["Baker with ID " ++ show ebaBakerId ++ " added."]
      printToOutputFileIfJust bakerKeys ebaBakerId
    tryPrintKeyUpdateEventToOutputFile bakerKeys Types.BakerKeysUpdated{..} = do
      logInfo ["Keys for baker with ID " ++ show ebkuBakerId ++ " updated."]
      printToOutputFileIfJust bakerKeys ebkuBakerId
    tryPrintKeyUpdateEventToOutputFile _ _ = return ()

    warnAboutBadCapital txCfg capital = do
      let senderAddr = naAddr . esdAddress . tcEncryptedSigningData $ txCfg
      Types.AccountInfo{..} <- getAccountInfoOrDie senderAddr Best
      warnIfCapitalIsSmall capital
      cannotAfford <- warnIfCannotAfford txCfg capital aiAccountAmount
      case aiStakingInfo of
        Types.AccountStakingBaker{..} -> liftIO $ warnIfCapitalIsLowered capital asiStakedAmount
        _ -> return ()
      unless cannotAfford (warnIfCapitalIsBig capital aiAccountAmount)

    warnIfCannotAfford txCfg capital amount = do
      energyrate <- getNrgGtuRate
      let gtuTransactionPrice = Types.computeCost energyrate (tcEnergy txCfg)
      let cannotAfford = amount - gtuTransactionPrice < capital
      when cannotAfford $ do
        logWarn [[i|Account balance (#{showCcd amount}) minus the cost of the transaction (#{showCcd gtuTransactionPrice}) is lower than the amount requested to be staked (#{showCcd capital}).|]]
        confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
        unless confirmed exitTransactionCancelled
      return cannotAfford

    warnIfCapitalIsSmall capital = when (capital /= 0) $ do
      minimumBakerStake <- getBakerStakeThresholdOrDie
      when (capital < minimumBakerStake) $ do
        logWarn [[i|The staked amount (#{showCcd capital}) is lower than the minimum baker stake threshold (#{showCcd minimumBakerStake}).|]]
        confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
        unless confirmed exitTransactionCancelled

    warnIfCapitalIsLowered capital stakedAmount = do
      -- VH/FIXME: Error message changed here, probably not good - should this be changed back to reflect how it was?
      cooldownDate <- withClient backend $
        getBlockChainParametersV2 Best >>=
          getResponseValueOrFail >>=
            liftIO . getDelegatorCooldown
      when (capital < stakedAmount) $ do
        let removing = capital == 0
        if removing then
          logWarn ["This will remove the baker."]
        else
          logWarn ["The new staked value appears to be lower than the amount currently staked on chain by this baker."]
        let decreaseOrRemove = if removing then "Removing a baker" else "Decreasing the amount a baker is staking"
        logWarn [decreaseOrRemove ++ " will lock the stake of the baker for a cooldown period before the CCD are made available."]
        logWarn ["During this period it is not possible to update the baker's stake, or stop the baker."]
        logWarn [[i|The current baker cooldown would last until approximately #{cooldownDate}|]]
        let confirmStr = if removing then "remove the baker" else "update the baker's stake"
        confirmed <- askConfirmation $ Just $ "Confirm that you want to " ++ confirmStr
        unless confirmed exitTransactionCancelled

    warnIfCapitalIsBig capital amount =
      when ((capital * 100) > (amount * 95)) $ do
        logWarn ["You are attempting to stake >95% of your total CCD on this account. Staked CCD is not available for spending."]
        logWarn ["Be aware that updating or stopping your baker in the future will require some amount of non-staked CCD to pay for the transactions to do so."]
        confirmed <- askConfirmation $ Just "Confirm that you wish to stake this much CCD"
        unless confirmed exitTransactionCancelled

    transactionForBakerConfigure confirm = do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""
      let cbMetadataURL = fmap (Types.UrlText . Text.pack) metadataURL
      (bakerKeys, cbKeysWithProofs) <- readInputKeysFile baseCfg
      let payload = Types.encodePayload Types.ConfigureBaker{..}
          nrgCost _ = case cbKeysWithProofs of
                        Nothing -> return . Just $ bakerConfigureEnergyCostWithoutKeys (Types.payloadSize payload)
                        Just _ -> return . Just $ bakerConfigureEnergyCostWithKeys (Types.payloadSize payload)
      txCfg@TransactionConfig{..} <- getTransactionCfg baseCfg txOpts nrgCost
      logSuccess
        ([ printf "configuring baker with account %s" (show (naAddr $ esdAddress tcEncryptedSigningData))
         , printf "allowing up to %s to be spent as transaction fee" (showNrg tcEnergy) ]
         ++ configureCapitalLogMsg
         ++ configureRestakeLogMsg
         ++ configureOpenForDelegationLogMsg
         ++ configureTransactionFeeCommissionLogMsg
         ++ configureBakingRewardCommissionLogMsg
         ++ configureFinalizationRewardCommissionLogMsg)
      when confirm $ do
        confirmed <- askConfirmation Nothing
        unless confirmed exitTransactionCancelled
      when verbose $ do
        runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData
        putStrLn ""
      return (bakerKeys, txCfg, payload)

    configureCapitalLogMsg =
      case cbCapital of
        Nothing -> []
        Just capital -> [[i|stake will be #{Types.amountToString capital} CCD|]]

    configureRestakeLogMsg =
      case cbRestakeEarnings of
        Nothing -> []
        Just True -> ["rewards will be automatically added to the baking stake"]
        Just False -> ["rewards will _not_ be automatically added to the baking stake"]

    configureOpenForDelegationLogMsg =
      case cbOpenForDelegation of
        Nothing -> []
        Just Types.OpenForAll -> ["baker pool will be open for delegation"]
        Just Types.ClosedForNew -> ["baker pool will be closed for new delegators"]
        Just Types.ClosedForAll -> ["baker pool will be closed for delegators and existing delegators will be moved to passive delegation"]

    configureTransactionFeeCommissionLogMsg =
      case cbTransactionFeeCommission of
        Nothing -> []
        Just fee -> [printf "transaction fee commission from delegators will be %s" (show fee)]

    configureBakingRewardCommissionLogMsg =
      case cbBakingRewardCommission of
        Nothing -> []
        Just fee -> [printf "baking reward commission from delegators will be %s" (show fee)]

    configureFinalizationRewardCommissionLogMsg =
      case cbFinalizationRewardCommission of
        Nothing -> []
        Just fee -> [printf "finalization reward commission from delegators will be %s" (show fee)]

    readInputKeysFile baseCfg = do
      encSignData <- getAccountCfgFromTxOpts baseCfg txOpts
      case inputKeysFile of
        Nothing -> return (Nothing, Nothing)
        Just inf -> do
          bakerKeysMaybeEncrypted <- handleReadFile BS.readFile inf
          let pwdAction = askPassword "Enter password for decrypting baker keys: "
          Password.decodeMaybeEncrypted pwdAction bakerKeysMaybeEncrypted >>= \case
            Left err -> logFatal [printf "error: %s" err]
            Right (keys, enc) -> do
              let electionSignKey = bkElectionSignKey keys
                  signatureSignKey = bkSigSignKey keys
                  aggrSignKey = bkAggrSignKey keys
                  bkwpElectionVerifyKey = bkElectionVerifyKey keys
                  bkwpSignatureVerifyKey = bkSigVerifyKey keys
                  bkwpAggregationVerifyKey = bkAggrVerifyKey keys
                  senderAddress = applyAlias (toAlias txOpts) $ naAddr $ esdAddress encSignData
                  challenge = Types.configureBakerKeyChallenge senderAddress bkwpElectionVerifyKey bkwpSignatureVerifyKey bkwpAggregationVerifyKey
              bkwpProofElection <- Proofs.proveDlog25519VRF challenge (VRF.KeyPair electionSignKey bkwpElectionVerifyKey) `except` "cannot produce VRF key proof"
              bkwpProofSig <- Proofs.proveDlog25519Block challenge (BlockSig.KeyPair signatureSignKey bkwpSignatureVerifyKey) `except` "cannot produce signature key proof"
              bkwpProofAggregation <- Bls.proveKnowledgeOfSK challenge aggrSignKey
              return (Just (keys, enc), Just Types.BakerKeysWithProofs{..})

    except c err = c >>= \case
      Just x -> return x
      Nothing -> logFatal [err]

-- |Process the old 'baker add ...' command to add a baker in protocol version < 4.
processBakerAddCmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> Types.Amount -- ^New stake/capital.
  -> Bool -- ^Select whether to restake earnings.
  -> FilePath -- ^File to read baker keys from.
  -> Maybe FilePath -- ^File to write baker keys to.
  -> IO ()
processBakerAddCmd baseCfgDir verbose backend txOpts abBakingStake abRestakeEarnings inputKeysFile outputKeysFile = do
  let intOpts = toInteractionOpts txOpts
  (bakerKeys, txCfg, pl) <- transactionForBakerAdd (ioConfirm intOpts)
  withClient backend $ do
    warnAboutBadCapital txCfg abBakingStake
    result <- sendAndTailTransaction verbose txCfg pl intOpts
    events <- eventsFromTransactionResult result
    mapM_ (tryPrintKeyUpdateEventToOutputFile bakerKeys) events
  where
    askUntilEqual credentials = do
      pwd <- askPassword "Enter password for encryption of baker credentials (leave blank for no encryption): "
      case Password.getPassword pwd of
        "" -> do
          logInfo [ printf "Empty password, not encrypting baker credentials" ]
          return $ AE.encodePretty credentials
        _ -> do
          pwd2 <- askPassword "Re-enter password for encryption of baker credentials: "
          if pwd == pwd2 then
            AE.encodePretty <$> Password.encryptJSON Password.AES256 Password.PBKDF2SHA256 credentials pwd
          else do
            logWarn ["The two passwords were not equal. Try again"]
            askUntilEqual credentials

    printToOutputFile credentials out = liftIO $ do
      credentialsMaybeEncrypted <- askUntilEqual credentials
      void $ handleWriteFile BSL.writeFile PromptBeforeOverwrite verbose out credentialsMaybeEncrypted

    printToOutputFileIfJust bakerKeys bakerId =
      case (outputKeysFile, bakerKeys) of
        (Just outFile, (keys, _)) ->
          printToOutputFile BakerCredentials{bcKeys = keys, bcIdentity = bakerId} outFile
        _ ->
          let encrypted = snd bakerKeys
          in unless encrypted $
                logInfo [printf "To use it add \"bakerId\": %s to the keys file %s." (show bakerId) inputKeysFile]

    eventsFromTransactionResult Nothing = return []
    eventsFromTransactionResult (Just result) = do
      case tsrState result of
        Finalized | SingleBlock _ summary <- parseTransactionBlockResult result -> do
          case Types.tsResult summary of
            Types.TxReject reason -> logWarn [showRejectReason True reason] >> return []
            Types.TxSuccess es -> return es
        Absent ->
          logFatal ["Transaction is absent."]
        _ ->
          logFatal ["Unexpected status."]

    tryPrintKeyUpdateEventToOutputFile bakerKeys Types.BakerAdded{..} = do
      logInfo ["Baker with ID " ++ show ebaBakerId ++ " added."]
      printToOutputFileIfJust bakerKeys ebaBakerId
    tryPrintKeyUpdateEventToOutputFile _ _ = return ()

    warnAboutBadCapital txCfg capital = do
      let senderAddr = naAddr . esdAddress . tcEncryptedSigningData $ txCfg
      Types.AccountInfo{..} <- getAccountInfoOrDie senderAddr Best
      warnIfCapitalIsSmall capital
      cannotAfford <- warnIfCannotAfford txCfg capital aiAccountAmount
      unless cannotAfford (warnIfCapitalIsBig capital aiAccountAmount)

    warnIfCannotAfford txCfg capital amount = do
      energyrate <- getNrgGtuRate
      let gtuTransactionPrice = Types.computeCost energyrate (tcEnergy txCfg)
      let cannotAfford = amount - gtuTransactionPrice < capital
      when cannotAfford $ do
        logWarn [[i|Account balance (#{showCcd amount}) minus the cost of the transaction (#{showCcd gtuTransactionPrice}) is lower than the amount requested to be staked (#{showCcd capital}).|]]
        confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
        unless confirmed exitTransactionCancelled
      return cannotAfford

    warnIfCapitalIsSmall capital = do
      minimumBakerStake <- getBakerStakeThresholdOrDie
      when (capital < minimumBakerStake) $ do
        logWarn [[i|The staked amount (#{showCcd capital}) is lower than the minimum baker stake threshold (#{showCcd minimumBakerStake}).|]]
        confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
        unless confirmed exitTransactionCancelled

    warnIfCapitalIsBig capital amount =
      when ((capital * 100) > (amount * 95)) $ do
        logWarn ["You are attempting to stake >95% of your total CCD on this account. Staked CCD is not available for spending."]
        logWarn ["Be aware that updating or stopping your baker in the future will require some amount of non-staked CCD to pay for the transactions to do so."]
        confirmed <- askConfirmation $ Just "Confirm that you wish to stake this much CCD"
        unless confirmed exitTransactionCancelled

    transactionForBakerAdd confirm = do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""
      (bakerKeys, Types.BakerKeysWithProofs {
    bkwpElectionVerifyKey = abElectionVerifyKey,
    bkwpProofElection = abProofElection,
    bkwpSignatureVerifyKey = abSignatureVerifyKey,
    bkwpProofSig = abProofSig,
    bkwpAggregationVerifyKey = abAggregationVerifyKey,
    bkwpProofAggregation = abProofAggregation
  }) <- readInputKeysFile baseCfg
      let payload = Types.encodePayload Types.AddBaker{..}
          nrgCost _ = return . Just $ bakerAddEnergyCost $ Types.payloadSize payload
      txCfg@TransactionConfig{..} <- getTransactionCfg baseCfg txOpts nrgCost
      logSuccess
        ([ printf "adding baker with account %s" (show (naAddr $ esdAddress tcEncryptedSigningData))
         , printf "allowing up to %s to be spent as transaction fee" (showNrg tcEnergy) ]
         ++ configureCapitalLogMsg
         ++ configureRestakeLogMsg)
      when confirm $ do
        confirmed <- askConfirmation Nothing
        unless confirmed exitTransactionCancelled
      when verbose $ do
        runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData
        putStrLn ""
      return (bakerKeys, txCfg, payload)

    configureCapitalLogMsg = [printf "stake will be %s CCD" (Types.amountToString abBakingStake)]

    configureRestakeLogMsg =
      case abRestakeEarnings of
        True -> ["rewards will be automatically added to the baking stake"]
        False -> ["rewards will _not_ be automatically added to the baking stake"]

    readInputKeysFile baseCfg = do
      encSignData <- getAccountCfgFromTxOpts baseCfg txOpts
      bakerKeysMaybeEncrypted <- handleReadFile BS.readFile inputKeysFile
      let pwdAction = askPassword "Enter password for decrypting baker keys: "
      Password.decodeMaybeEncrypted pwdAction bakerKeysMaybeEncrypted >>= \case
        Left err -> logFatal [printf "error: %s" err]
        Right (keys, enc) -> do
          let electionSignKey = bkElectionSignKey keys
              signatureSignKey = bkSigSignKey keys
              aggrSignKey = bkAggrSignKey keys
              bkwpElectionVerifyKey = bkElectionVerifyKey keys
              bkwpSignatureVerifyKey = bkSigVerifyKey keys
              bkwpAggregationVerifyKey = bkAggrVerifyKey keys
              senderAddress = applyAlias (toAlias txOpts) $ naAddr $ esdAddress encSignData
              challenge = Types.addBakerChallenge senderAddress bkwpElectionVerifyKey bkwpSignatureVerifyKey bkwpAggregationVerifyKey
          bkwpProofElection <- Proofs.proveDlog25519VRF challenge (VRF.KeyPair electionSignKey bkwpElectionVerifyKey) `except` "cannot produce VRF key proof"
          bkwpProofSig <- Proofs.proveDlog25519Block challenge (BlockSig.KeyPair signatureSignKey bkwpSignatureVerifyKey) `except` "cannot produce signature key proof"
          bkwpProofAggregation <- Bls.proveKnowledgeOfSK challenge aggrSignKey
          return ((keys, enc), Types.BakerKeysWithProofs{..})

    except c err = c >>= \case
      Just x -> return x
      Nothing -> logFatal [err]

-- |Process the old 'baker set-key ...' command to set baker keys in protocol version < 4.
processBakerSetKeysCmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> FilePath -- ^File to read baker keys from.
  -> Maybe FilePath -- ^File to write baker keys to.
  -> IO ()
processBakerSetKeysCmd baseCfgDir verbose backend txOpts inputKeysFile outputKeysFile = do
  let intOpts = toInteractionOpts txOpts
  (bakerKeys, txCfg, pl) <- transactionForBakerSetKeys (ioConfirm intOpts)
  withClient backend $ do
    result <- sendAndTailTransaction verbose txCfg pl intOpts
    events <- eventsFromTransactionResult result
    mapM_ (tryPrintKeyUpdateEventToOutputFile bakerKeys) events
  where
    askUntilEqual credentials = do
      pwd <- askPassword "Enter password for encryption of baker credentials (leave blank for no encryption): "
      case Password.getPassword pwd of
        "" -> do
          logInfo [ printf "Empty password, not encrypting baker credentials" ]
          return $ AE.encodePretty credentials
        _ -> do
          pwd2 <- askPassword "Re-enter password for encryption of baker credentials: "
          if pwd == pwd2 then
            AE.encodePretty <$> Password.encryptJSON Password.AES256 Password.PBKDF2SHA256 credentials pwd
          else do
            logWarn ["The two passwords were not equal. Try again"]
            askUntilEqual credentials

    printToOutputFile credentials out = liftIO $ do
      credentialsMaybeEncrypted <- askUntilEqual credentials
      void $ handleWriteFile BSL.writeFile PromptBeforeOverwrite verbose out credentialsMaybeEncrypted

    printToOutputFileIfJust bakerKeys bakerId =
      case (outputKeysFile, bakerKeys) of
        (Just outFile, (keys, _)) ->
          printToOutputFile BakerCredentials{bcKeys = keys, bcIdentity = bakerId} outFile
        _ ->
          let encrypted = snd bakerKeys
          in unless encrypted $
                logInfo [printf "To use it add \"bakerId\": %s to the keys file %s." (show bakerId) inputKeysFile]

    eventsFromTransactionResult Nothing = return []
    eventsFromTransactionResult (Just result) = do
      case tsrState result of
        Finalized | SingleBlock _ summary <- parseTransactionBlockResult result -> do
          case Types.tsResult summary of
            Types.TxReject reason -> logWarn [showRejectReason True reason] >> return []
            Types.TxSuccess es -> return es
        Absent ->
          logFatal ["Transaction is absent."]
        _ ->
          logFatal ["Unexpected status."]

    tryPrintKeyUpdateEventToOutputFile bakerKeys Types.BakerKeysUpdated{..} = do
      logInfo ["Keys for baker with ID " ++ show ebkuBakerId ++ " updated."]
      printToOutputFileIfJust bakerKeys ebkuBakerId
    tryPrintKeyUpdateEventToOutputFile _ _ = return ()

    transactionForBakerSetKeys confirm = do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""
      (bakerKeys, Types.BakerKeysWithProofs {
    bkwpElectionVerifyKey = ubkElectionVerifyKey,
    bkwpProofElection = ubkProofElection,
    bkwpSignatureVerifyKey = ubkSignatureVerifyKey,
    bkwpProofSig = ubkProofSig,
    bkwpAggregationVerifyKey = ubkAggregationVerifyKey,
    bkwpProofAggregation = ubkProofAggregation
  }) <- readInputKeysFile baseCfg
      let payload = Types.encodePayload Types.UpdateBakerKeys{..}
          nrgCost _ = return . Just $ bakerSetKeysEnergyCost $ Types.payloadSize payload
      txCfg@TransactionConfig{..} <- getTransactionCfg baseCfg txOpts nrgCost
      logSuccess
        ([ printf "setting new keys for baker with account %s" (show (naAddr $ esdAddress tcEncryptedSigningData))
         , printf "allowing up to %s to be spent as transaction fee" (showNrg tcEnergy) ])
      when confirm $ do
        confirmed <- askConfirmation Nothing
        unless confirmed exitTransactionCancelled
      when verbose $ do
        runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData
        putStrLn ""
      return (bakerKeys, txCfg, payload)

    readInputKeysFile baseCfg = do
      encSignData <- getAccountCfgFromTxOpts baseCfg txOpts
      bakerKeysMaybeEncrypted <- handleReadFile BS.readFile inputKeysFile
      let pwdAction = askPassword "Enter password for decrypting baker keys: "
      Password.decodeMaybeEncrypted pwdAction bakerKeysMaybeEncrypted >>= \case
        Left err -> logFatal [printf "error: %s" err]
        Right (keys, enc) -> do
          let electionSignKey = bkElectionSignKey keys
              signatureSignKey = bkSigSignKey keys
              aggrSignKey = bkAggrSignKey keys
              bkwpElectionVerifyKey = bkElectionVerifyKey keys
              bkwpSignatureVerifyKey = bkSigVerifyKey keys
              bkwpAggregationVerifyKey = bkAggrVerifyKey keys
              senderAddress = applyAlias (toAlias txOpts) $ naAddr $ esdAddress encSignData
              challenge = Types.addBakerChallenge senderAddress bkwpElectionVerifyKey bkwpSignatureVerifyKey bkwpAggregationVerifyKey
          bkwpProofElection <- Proofs.proveDlog25519VRF challenge (VRF.KeyPair electionSignKey bkwpElectionVerifyKey) `except` "cannot produce VRF key proof"
          bkwpProofSig <- Proofs.proveDlog25519Block challenge (BlockSig.KeyPair signatureSignKey bkwpSignatureVerifyKey) `except` "cannot produce signature key proof"
          bkwpProofAggregation <- Bls.proveKnowledgeOfSK challenge aggrSignKey
          return ((keys, enc), Types.BakerKeysWithProofs{..})

    except c err = c >>= \case
      Just x -> return x
      Nothing -> logFatal [err]

-- |Process the old 'baker set-key ...' command to set baker keys in protocol version < 4.
processBakerRemoveCmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> IO ()
processBakerRemoveCmd baseCfgDir verbose backend txOpts = do
  let intOpts = toInteractionOpts txOpts
  (txCfg, pl) <- transactionForBakerRemove (ioConfirm intOpts)
  withClient backend $ do
    liftIO $ warnAboutRemoving
    sendAndTailTransaction_ verbose txCfg pl intOpts
  where
    warnAboutRemoving = do
      -- VH/FIXME: Error message changed here, probably not good - should this be changed back to reflect how it was?
      cooldownDate <- withClient backend $
        getBlockChainParametersV2 Best >>=
          getResponseValueOrFail >>=
            getBakerCooldown
      logWarn ["Stopping a baker that is staking will lock the stake of the baker for a cooldown period before the CCD are made available."]
      logWarn ["During this period it is not possible to update the baker's stake, or restart the baker."]
      logWarn [[i|The current baker cooldown would last until approximately #{cooldownDate}|]]
      confirmed <- askConfirmation $ Just "Confirm that you want to send the transaction to stop this baker"
      unless confirmed exitTransactionCancelled

    transactionForBakerRemove confirm = do
      baseCfg <- getBaseConfig baseCfgDir verbose
      let payload = Types.encodePayload Types.RemoveBaker
          nrgCost _ = return . Just $ bakerRemoveEnergyCost $ Types.payloadSize payload
      txCfg@TransactionConfig{..} <- getTransactionCfg baseCfg txOpts nrgCost
      logSuccess
        ([ printf "submitting transaction to remove baker with %s" (show (naAddr $ esdAddress tcEncryptedSigningData))
         , printf "allowing up to %s to be spent as transaction fee" (showNrg tcEnergy) ])
      when confirm $ do
        confirmed <- askConfirmation Nothing
        unless confirmed exitTransactionCancelled
      when verbose $ do
        runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData
        putStrLn ""
      return (txCfg, payload)

-- |Process the old 'baker update-stake ...' command in protocol version < 4.
processBakerUpdateStakeBeforeP4Cmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> Types.Amount -- ^New stake
  -> IO ()
processBakerUpdateStakeBeforeP4Cmd baseCfgDir verbose backend txOpts ubsStake = do
  let intOpts = toInteractionOpts txOpts
  (txCfg, pl) <- transactionForBakerUpdateStake (ioConfirm intOpts)
  withClient backend $ do
    warnAboutBadCapital txCfg ubsStake
    sendAndTailTransaction_ verbose txCfg pl intOpts
  where

    warnAboutBadCapital txCfg capital = do
      let senderAddr = naAddr . esdAddress . tcEncryptedSigningData $ txCfg
      Types.AccountInfo{..} <- getAccountInfoOrDie senderAddr Best
      warnIfCapitalIsSmall capital
      cannotAfford <- warnIfCannotAfford txCfg capital aiAccountAmount
      case aiStakingInfo of
        Types.AccountStakingBaker{..} -> liftIO $ warnIfCapitalIsLowered capital asiStakedAmount
        _ -> return ()
      unless cannotAfford (warnIfCapitalIsBig capital aiAccountAmount)

    warnIfCannotAfford txCfg capital amount = do
      energyrate <- getNrgGtuRate
      let gtuTransactionPrice = Types.computeCost energyrate (tcEnergy txCfg)
      let cannotAfford = amount - gtuTransactionPrice < capital
      when cannotAfford $ do
        logWarn [[i|Account balance (#{showCcd amount}) minus the cost of the transaction (#{showCcd gtuTransactionPrice}) is lower than the amount requested to be staked (#{showCcd capital}).|]]
        confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
        unless confirmed exitTransactionCancelled
      return cannotAfford

    warnIfCapitalIsSmall capital = do
      minimumBakerStake <- getBakerStakeThresholdOrDie
      when (capital < minimumBakerStake) $ do
        logWarn [[i|The staked amount (#{showCcd capital}) is lower than the minimum baker stake threshold (#{showCcd minimumBakerStake}).|]]
        confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
        unless confirmed exitTransactionCancelled

    warnIfCapitalIsLowered capital stakedAmount = do
      -- VH/FIXME: Error message changed here, probably not good - should this be changed back to reflect how it was?
      cooldownDate <- withClient backend $
        getBlockChainParametersV2 Best >>=
          getResponseValueOrFail >>=
            getBakerCooldown
      if capital < stakedAmount
      then do
        logWarn ["The new staked value appears to be lower than the amount currently staked on chain by this baker."]
        logWarn ["Decreasing the amount a baker is staking will lock the stake of the baker for a cooldown period before the CCD are made available."]
        logWarn ["During this period it is not possible to update the baker's stake, or stop the baker."]
        logWarn [[i|The current baker cooldown would last until approximately #{cooldownDate}|]]
        confirmed <- askConfirmation $ Just "Confirm that you want to update the baker's stake"
        unless confirmed exitTransactionCancelled
      else do
        logInfo ["Note that decreasing the amount a baker is staking will lock the stake of the baker for a cooldown period before the CCD are made available."]
        logInfo ["During this period it is not possible to update the baker's stake, or stop the baker."]
        logInfo [[i|The current baker cooldown would last until approximately #{cooldownDate}|]]

    warnIfCapitalIsBig capital amount =
      when ((capital * 100) > (amount * 95)) $ do
        logWarn ["You are attempting to stake >95% of your total CCD on this account. Staked CCD is not available for spending."]
        logWarn ["Be aware that updating or stopping your baker in the future will require some amount of non-staked CCD to pay for the transactions to do so."]
        confirmed <- askConfirmation $ Just "Confirm that you wish to stake this much CCD"
        unless confirmed exitTransactionCancelled

    transactionForBakerUpdateStake confirm = do
      baseCfg <- getBaseConfig baseCfgDir verbose
      let payload = Types.encodePayload Types.UpdateBakerStake{..}
          nrgCost _ = return . Just $ bakerUpdateStakeEnergyCost $ Types.payloadSize payload
      txCfg@TransactionConfig{..} <- getTransactionCfg baseCfg txOpts nrgCost
      logSuccess
        ([ printf "submitting transaction to update stake of baker to %s" (showCcd ubsStake)
         , printf "allowing up to %s to be spent as transaction fee" (showNrg tcEnergy) ])
      when confirm $ do
        confirmed <- askConfirmation Nothing
        unless confirmed exitTransactionCancelled
      when verbose $ do
        runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData
        putStrLn ""
      return (txCfg, payload)

-- |Process the old 'baker update-restake ...' command in protocol version < 4.
processBakerUpdateRestakeCmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> Bool -- ^Whether to restake earnings
  -> IO ()
processBakerUpdateRestakeCmd baseCfgDir verbose backend txOpts ubreRestakeEarnings = do
  let intOpts = toInteractionOpts txOpts
  (txCfg, pl) <- transactionForBakerUpdateRestake (ioConfirm intOpts)
  withClient backend $ do
    sendAndTailTransaction_ verbose txCfg pl intOpts
  where

    transactionForBakerUpdateRestake confirm = do
      baseCfg <- getBaseConfig baseCfgDir verbose
      let payload = Types.encodePayload Types.UpdateBakerRestakeEarnings{..}
          nrgCost _ = return . Just $ bakerUpdateRestakeEnergyCost $ Types.payloadSize payload
      txCfg@TransactionConfig{..} <- getTransactionCfg baseCfg txOpts nrgCost
      logSuccess
        ([ printf "submitting transaction to change restaking switch of baker to %s" (show ubreRestakeEarnings)
         , printf "allowing up to %s to be spent as transaction fee" (showNrg tcEnergy) ])
      when confirm $ do
        confirmed <- askConfirmation Nothing
        unless confirmed exitTransactionCancelled
      when verbose $ do
        runPrinter $ printSelectedKeyConfig tcEncryptedSigningData
        putStrLn ""
      return (txCfg, payload)

-- |Process the 'baker update-stake ...' command.
processBakerUpdateStakeCmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> Types.Amount -- ^New stake
  -> IO ()
processBakerUpdateStakeCmd baseCfgDir verbose backend txOpts newStake = do
  ok <- if newStake == 0
          then do
            logWarn ["Updating stake to 0 will remove baker", "it is possible to add the baker back with a 'baker add' transaction"]
            askConfirmation $ Just "confirm that you want to remove baker"
          else return True
  when ok $
    processBakerConfigureCmd baseCfgDir verbose backend txOpts False (Just newStake) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |Process a 'baker ...' command.
processBakerCmd :: BakerCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processBakerCmd action baseCfgDir verbose backend =
  case action of
    BakerGenerateKeys outputFile maybeBakerId -> do
      keys <- generateBakerKeys maybeBakerId
      let askUntilEqual = do
                pwd <- askPassword "Enter password for encryption of baker keys (leave blank for no encryption): "
                case Password.getPassword pwd of
                  "" -> do 
                    logInfo [ printf "Empty password, not encrypting baker keys" ]
                    return $ AE.encodePretty keys
                  _ -> do 
                    pwd2 <- askPassword "Re-enter password for encryption of baker keys: "
                    if pwd == pwd2 then
                      AE.encodePretty <$> Password.encryptJSON Password.AES256 Password.PBKDF2SHA256 keys pwd
                    else do
                      logWarn ["The two passwords were not equal. Try again."]
                      askUntilEqual
      out <- askUntilEqual
      let publicBakerKeysJSON = AE.encodePretty . AE.object $ bakerPublicKeysToPairs keys
      case outputFile of
        Nothing -> do
          -- TODO Store in config.
          BSL8.putStrLn out
          logInfo [ printf "to add a baker to the chain using these keys, store it in a file and use 'concordium-client baker add FILE'" ]
        Just f -> do
          keysSuccess <- handleWriteFile BSL.writeFile PromptBeforeOverwrite verbose f out
          when keysSuccess $ do
            let pubFile = f -<.> (".pub" ++ takeExtension f)
            logSuccess [ printf "keys written to file '%s'" f
                       , "DO NOT LOSE THIS FILE"
                       , printf "to add a baker to the chain using these keys, use 'concordium-client baker add %s'" f]
            pubSuccess <- handleWriteFile BSL.writeFile PromptBeforeOverwrite verbose pubFile publicBakerKeysJSON
            when pubSuccess $ do
              logSuccess [ printf "public keys written to file '%s'" pubFile]

    BakerAdd bakerKeysFile txOpts initialStake autoRestake extraData outputFile -> do
      pv <- withClient backend $ do
        cs <- getResponseValueOrFail =<< getConsensusInfoV2
        return $ Queries.csProtocolVersion cs
      if pv < Types.P4
        then
          processBakerAddCmd baseCfgDir verbose backend txOpts initialStake autoRestake bakerKeysFile outputFile
        else do
          when (isNothing extraData) $ do
            logWarn $ ["To add a baker, all of the options\n"
                 ++ "--open-delegation-for,\n"
                 ++ "--baker-url,\n"
                 ++ "--delegation-transaction-fee-commission,\n"
                 ++ "--delegation-baking-commission,\n"
                 ++ "--delegation-finalization-commission\nmust be present"]
            confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
            unless confirmed exitTransactionCancelled
          let openForDelegation = ebadOpenForDelegation <$> extraData
              metadataURL = ebadMetadataURL <$> extraData
              transactionFeeCommission = ebadTransactionFeeCommission <$> extraData
              bakingRewardCommission = ebadBakingRewardCommission <$> extraData
              finalizationRewardCommission = ebadFinalizationRewardCommission <$> extraData
          processBakerConfigureCmd baseCfgDir verbose backend txOpts False (Just initialStake) (Just autoRestake) openForDelegation metadataURL transactionFeeCommission bakingRewardCommission finalizationRewardCommission (Just bakerKeysFile) outputFile

    BakerConfigure txOpts capital restake openForDelegation metadataURL transactionFeeCommission bakingRewardCommission finalizationRewardCommission inputKeysFile outputKeysFile ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts True capital restake openForDelegation metadataURL transactionFeeCommission bakingRewardCommission finalizationRewardCommission inputKeysFile outputKeysFile

    BakerSetKeys file txOpts outfile -> do
      pv <- withClient backend $ do
        cs <- getResponseValueOrFail =<< getConsensusInfoV2
        return $ Queries.csProtocolVersion cs
      if pv < Types.P4
        then processBakerSetKeysCmd baseCfgDir verbose backend txOpts file outfile
        else processBakerConfigureCmd baseCfgDir verbose backend txOpts False Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just file) outfile

    BakerRemove txOpts -> do
      pv <- withClient backend $ do
        cs <- getResponseValueOrFail =<< getConsensusInfoV2
        return $ Queries.csProtocolVersion cs
      if pv < Types.P4
        then processBakerRemoveCmd baseCfgDir verbose backend txOpts
        else processBakerConfigureCmd baseCfgDir verbose backend txOpts False (Just 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    BakerUpdateStake newStake txOpts -> do
      pv <- withClient backend $ do
        cs <- getResponseValueOrFail =<< getConsensusInfoV2
        return $ Queries.csProtocolVersion cs
      if pv < Types.P4
      then processBakerUpdateStakeBeforeP4Cmd baseCfgDir verbose backend txOpts newStake
      else processBakerUpdateStakeCmd baseCfgDir verbose backend txOpts newStake

    BakerUpdateRestakeEarnings restake txOpts -> do
      pv <- withClient backend $ do
        cs <- getResponseValueOrFail =<< getConsensusInfoV2
        return $ Queries.csProtocolVersion cs
      if pv < Types.P4
      then processBakerUpdateRestakeCmd baseCfgDir verbose backend txOpts restake
      else processBakerConfigureCmd baseCfgDir verbose backend txOpts False Nothing (Just restake) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    BakerUpdateMetadataURL url txOpts ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts False Nothing Nothing Nothing (Just url) Nothing Nothing Nothing Nothing Nothing

    BakerUpdateOpenDelegationStatus status txOpts ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts False Nothing Nothing (Just status) Nothing Nothing Nothing Nothing Nothing Nothing

-- |Process a 'delegator configure ...' command.
processDelegatorConfigureCmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> Maybe Types.Amount -- ^New stake/capital.
  -> Maybe Bool -- ^Select whether to restake earnings.
  -> Maybe Types.DelegationTarget -- ^Delegation target: baker or passive delegation.
  -> IO ()
processDelegatorConfigureCmd baseCfgDir verbose backend txOpts cdCapital cdRestakeEarnings cdDelegationTarget = do
  let intOpts = toInteractionOpts txOpts
  (txCfg, pl) <- transactionForDelegatorConfigure (ioConfirm intOpts)
  withClient backend $ do
    warnInOldProtocol
    mapM_ (warnAboutBadCapital txCfg) cdCapital
    result <- sendAndTailTransaction verbose txCfg pl intOpts
    warnAboutFailedResult result
  where
    warnInOldProtocol = do
      cs <- getResponseValueOrFail =<< getConsensusInfoV2
      when (Queries.csProtocolVersion cs < Types.P4) $ do
        logWarn [[i|Delegation is not supported in protocol versions < 4.|]]
        confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
        unless confirmed exitTransactionCancelled
    warnAboutPoolStatus capital alreadyDelegatedToBakerPool alreadyBakerId = do
      case cdDelegationTarget of
        Nothing -> return ()
        Just Types.DelegatePassive -> return ()
        Just (Types.DelegateToBaker bid) -> do
          poolStatus <- getPoolStatusOrDie $ Just bid
          let alreadyDelegatedToThisBaker = case alreadyBakerId of
                Just abid -> if abid == bid then alreadyDelegatedToBakerPool else 0
                Nothing -> 0
          case poolStatus of
            Queries.BakerPoolStatus{..} -> when (psDelegatedCapital + capital - alreadyDelegatedToThisBaker > psDelegatedCapitalCap) $ do
              logWarn [[i|Staked amount (#{showCcd capital}) plus the stake already delegated the pool is larger than the maximum allowed delegated stake).|]]
              confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
              unless confirmed exitTransactionCancelled
            _ -> return () -- Should not happen
    warnAboutBadCapital txCfg capital = do
      let senderAddr = naAddr . esdAddress . tcEncryptedSigningData $ txCfg
      Types.AccountInfo{..} <- getAccountInfoOrDie senderAddr Best
      warnIfCannotAfford txCfg capital aiAccountAmount
      (alreadyDelegatedToBakerPool, alreadyBakerId) <- case aiStakingInfo of
            Types.AccountStakingDelegated{..} -> do
                liftIO $ warnIfCapitalIsLowered capital asiStakedAmount
                mbid <- case asiDelegationTarget of
                  Types.DelegatePassive -> return Nothing
                  Types.DelegateToBaker bid -> return $ Just bid
                return (asiStakedAmount, mbid)
            _ -> return (0, Nothing)
      warnAboutPoolStatus capital alreadyDelegatedToBakerPool alreadyBakerId

    warnIfCapitalIsLowered capital stakedAmount = do
      -- VH/FIXME: Error message changed here, probably not good - should this be changed back to reflect how it was?
      cooldownDate <- withClient backend $
        getBlockChainParametersV2 Best >>=
          getResponseValueOrFail >>=
            liftIO . getDelegatorCooldown
      let cooldownString :: String = case cooldownDate of
            Just cd -> [i|The current baker cooldown would last until approximately #{cd}|]
            Nothing -> [i||]
      when (capital < stakedAmount) $ do
        let removing = capital == 0
        if removing then
          logWarn ["This will remove the delegator."]
        else
          logWarn ["The new staked value appears to be lower than the amount currently staked on chain by this delegator."]
        let decreaseOrRemove = if removing then "Removing a delegator" else "Decreasing the amount a delegator is staking"
        logWarn [decreaseOrRemove ++ " will lock the stake of the delegator for a cooldown period before the CCD are made available."]
        logWarn ["During this period it is not possible to update the delegator's stake, or stop the delegation of stake."]
        logWarn [cooldownString]
        let confirmStr = if removing then "remove the delegator" else "update the delegator's stake"
        confirmed <- askConfirmation $ Just $ "Confirm that you want to " ++ confirmStr
        unless confirmed exitTransactionCancelled

    warnIfCannotAfford txCfg capital amount = do
      energyrate <- getNrgGtuRate
      let gtuTransactionPrice = Types.computeCost energyrate (tcEnergy txCfg)
      when (amount - gtuTransactionPrice < capital) $ do
        logWarn [[i|Account balance (#{showCcd amount}) minus the cost of the transaction (#{showCcd gtuTransactionPrice}) is lower than the amount requested to be staked (#{showCcd capital}).|]]
        confirmed <- askConfirmation $ Just "This transaction will most likely be rejected by the chain, do you wish to send it anyway"
        unless confirmed exitTransactionCancelled

    warnAboutFailedResult Nothing = return ()
    warnAboutFailedResult (Just result) =
      case tsrState result of
        Finalized | SingleBlock _ summary <- parseTransactionBlockResult result -> do
          case Types.tsResult summary of
            Types.TxReject reason -> logWarn [showRejectReason True reason]
            Types.TxSuccess _ -> return ()
        Absent ->
          logFatal ["Transaction is absent."]
        _ ->
          logFatal ["Unexpected status."]

    transactionForDelegatorConfigure confirm = do
      baseCfg <- getBaseConfig baseCfgDir verbose
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""
      let payload = Types.encodePayload Types.ConfigureDelegation{..}
          nrgCost _ = return . Just $ delegationConfigureEnergyCost (Types.payloadSize payload)
      txCfg@TransactionConfig{..} <- getTransactionCfg baseCfg txOpts nrgCost
      logSuccess
        ([ printf "configuring delegator with account %s" (show (naAddr $ esdAddress tcEncryptedSigningData))
         , printf "allowing up to %s to be spent as transaction fee" (showNrg tcEnergy) ]
         ++ configureCapitalLogMsg
         ++ configureRestakeLogMsg
         ++ configureDelegationTargetLogMsg)

      when confirm $ do
        confirmed <- askConfirmation Nothing
        unless confirmed exitTransactionCancelled
      when verbose $ do
        runPrinter $ printSelectedKeyConfig tcEncryptedSigningData
        putStrLn ""
      return (txCfg, payload)

    configureCapitalLogMsg =
      case cdCapital of
        Nothing -> []
        Just capital -> [printf "stake will be %s CCD" (Types.amountToString capital)]

    configureRestakeLogMsg =
      case cdRestakeEarnings of
        Nothing -> []
        Just True -> ["rewards will be automatically added to the delegated stake"]
        Just False -> ["rewards will _not_ be automatically added to the delegated stake"]

    configureDelegationTargetLogMsg =
      case cdDelegationTarget of
        Nothing -> []
        Just Types.DelegatePassive -> ["stake will be delegated passively"]
        Just (Types.DelegateToBaker bid) -> [printf "stake will be delegated to baker %s" (show bid)]


-- |Process a 'delegator ...' command.
processDelegatorCmd :: DelegatorCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processDelegatorCmd action baseCfgDir verbose backend =
  case action of
    DelegatorConfigure txOpts capital restake target -> do
      delegationTarget <- makeTarget target
      processDelegatorConfigureCmd baseCfgDir verbose backend txOpts capital restake delegationTarget
    DelegatorRemove txOpts ->
      processDelegatorConfigureCmd baseCfgDir verbose backend txOpts (Just 0) Nothing Nothing
    DelegatorAdd txOpts capital restake target -> do
      delegationTarget <- makeTarget $ Just target
      processDelegatorConfigureCmd baseCfgDir verbose backend txOpts (Just capital) (Just restake) delegationTarget
  where
    makeTarget Nothing = return Nothing
    makeTarget (Just target) = do
      baseCfg <- getBaseConfig baseCfgDir verbose
      let maybeAddress = resolveAccountAddress (bcAccountNameMap baseCfg) target
      case maybeAddress of
        Nothing -> case Text.unpack target of
          "Passive" -> return $ Just Types.DelegatePassive
          s | Prelude.all Char.isDigit s ->
            let n = read s :: Integer
                w = fromIntegral n :: Word64
            in if n >= 0 && n <= fromIntegral (maxBound :: Word64)
                then return $ Just $ Types.DelegateToBaker $ Types.BakerId $ Types.AccountIndex w
                else do
                  logWarn $ ["The BAKERID '" ++ s ++ "'" ++ " is out of range."]
                  exitTransactionCancelled
          s -> do
            logWarn $ ["Unexpected delegation target '" ++ s ++ "'. The allowed values are: " ++ COM.allowedValuesDelegationTargetAsString]
            exitTransactionCancelled
        Just na -> do
          let address = naAddr na
          withClient backend $ do
            ai <- Types.aiAccountIndex <$> getAccountInfoOrDie address Best
            return $ Just $ Types.DelegateToBaker $ Types.BakerId $ ai

processIdentityCmd :: IdentityCmd -> Backend -> IO ()
processIdentityCmd action backend =
  case action of
    IdentityShow c -> processIdentityShowCmd c backend

processIdentityShowCmd :: IdentityShowCmd -> Backend -> IO ()
processIdentityShowCmd action backend =
  case action of
    IdentityShowIPs block -> do
      bhInput <- readBlockHashOrDefault Best block
      withClient backend $ getIdentityProvidersV2 bhInput >>=
        getResponseValueOrFail >>=
          runPrinter . printIdentityProviders . toList
    IdentityShowARs block -> do
      bhInput <- readBlockHashOrDefault Best block
      withClient backend $ getAnonymityRevokersV2 bhInput >>=
        getResponseValueOrFail >>=
          runPrinter . printAnonymityRevokers . toList

-- |Process a "legacy" command.
processLegacyCmd :: LegacyCmd -> Backend -> IO ()
processLegacyCmd action backend =
  case action of
    SendTransaction fname nid -> do
      source <- handleReadFile BSL.readFile fname
      t <- withClient backend $ processTransaction source nid
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (getBlockItemHash t)
    GetConsensusInfo ->
      withClient backend $
        getConsensusInfoV2 >>=
        printResponseValueAsJSON
    GetBlockInfo every block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        printBlockInfos every
    GetBlockPendingUpdates block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getBlockPendingUpdatesV2 >>=
        printResponseValueAsJSON
    GetBlockSpecialEvents block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getBlockSpecialEventsV2 >>=
        printResponseValueAsJSON
    GetBlockChainParameters block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getBlockChainParametersV2 >>=
        printResponseValueAsJSON
    GetBlockFinalizationSummary block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getBlockFinalizationSummaryV2 >>=
        printResponseValueAsJSON
    GetBlocksAtHeight height gen restr ->
      withClient backend $
        getBlocksAtHeightV2
          (case (gen, restr) of
            (Just g, Just _) ->
              Relative g height True
            (Just g, Nothing) ->
              Relative g height False
            (Nothing, _) ->
              Absolute (Types.AbsoluteBlockHeight $ Types.theBlockHeight height)) >>=
        printResponseValueAsJSON
    GetAccountList block -> do
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getAccountListV2 >>=
        printResponseValueAsJSON
    GetInstances block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getInstanceListV2 >>=
        printResponseValueAsJSON
    GetTransactionStatus txhash ->
      withClient backend $
        readOrFail txhash >>=
        getBlockItemStatusV2 >>=
        printResponseValueAsJSON
    GetAccountInfo account block -> do
      acc <- case Types.decodeAccountIdentifier $ Text.encodeUtf8 account of
        Nothing -> logFatal [[i|cannot parse #{account} as an account identifier.|]]
        Just a -> return a
      b <- readBlockHashOrDefault Best block
      withClient backend $
        getAccountInfoV2 acc b >>=
        printResponseValueAsJSON
    GetAccountNonFinalized account -> do
      acc <- parseAccountAddress account
      withClient backend $
        getAccountNonFinalizedTransactionsV2 acc >>=
        printResponseValueAsJSON
    GetNextAccountNonce account -> do
      acc <- parseAccountAddress account
      withClient backend $
        getNextSequenceNumberV2 acc >>=
        printResponseValueAsJSON
    GetInstanceInfo addr block ->
      withClient backend $ do
        b <- readBlockHashOrDefault Best block
        -- The input is a JSON object of the form '{ "index":10, "subindex": 0 }'.
        cAddr <- case AE.eitherDecodeStrict $ Text.encodeUtf8 addr of
          Left err -> logFatal [[i|Unable to decode contract address: #{err}|]]
          Right v -> return v
        let contractIndex = Types.contractIndex cAddr
        let contractSubindex = Types.contractSubindex cAddr
        getInstanceInfoV2 Types.ContractAddress{..} b >>=
          printResponseValueAsJSON
    InvokeContract contextFile block -> do
      withClient backend $ do
        ctx <- liftIO $ BSL.readFile contextFile
        b <- readBlockHashOrDefault Best block
        (case eitherDecode ctx of
          Left err -> logFatal [[i|Unable to decode context: #{err}|]]
          Right c -> invokeInstanceV2 b c) >>=
          printResponseValueAsJSON
    GetPoolStatus pool block ->
      withClient backend $ do
        b <- readBlockHashOrDefault Best block
        (case pool of
          Nothing -> getPassiveDelegationInfoV2 b
          Just p -> getPoolInfoV2 b p) >>=
          printResponseValueAsJSON
    GetBakerList block -> withClient backend $
      readBlockHashOrDefault Best block >>=
      getBakerListV2 >>=
      printResponseValueAsJSON
    GetRewardStatus block -> withClient backend $
      readBlockHashOrDefault Best block >>=
      getTokenomicsInfoV2 >>=
      printResponseValueAsJSON
    GetBirkParameters block -> withClient backend $
      readBlockHashOrDefault Best block >>=
      getElectionInfoV2 >>=
      printResponseValueAsJSON
    GetModuleList block -> withClient backend $
      readBlockHashOrDefault Best block >>=
      getModuleListV2 >>=
      printResponseValueAsJSON
    GetNodeInfo -> withClient backend $
      getNodeInfoV2 >>= getResponseValueOrDie >>= printNodeInfo
    GetPeerData bootstrapper -> withClient backend $ do
      peersInfo <- getResponseValueOrDie =<< getPeersInfoV2
      nodeInfo <- getResponseValueOrDie =<< getNodeInfoV2
      printPeerData bootstrapper peersInfo nodeInfo
    PeerConnect ip port ->
      withClient backend $
        peerConnectV2 (Queries.IpAddress ip) (Queries.IpPort $ fromIntegral port) >>=
        printSuccess
    PeerDisconnect ip port ->
      withClient backend $
        peerDisconnectV2 (Queries.IpAddress ip) (Queries.IpPort $ fromIntegral port) >>=
        printSuccess
    GetPeerUptime ->
      withClient backend $
        getNodeInfoV2 >>=
        printResponseValue Queries.peerUptime
    BanNode nodeIp ->
      withClient backend $
        banPeerV2 (Queries.IpAddress nodeIp) >>=
        printSuccess
    UnbanNode nodeIp ->
      withClient backend $
        unbanPeerV2 (Queries.IpAddress nodeIp) >>=
        printSuccess
    GetAncestors amount block ->
      withClient backend $ do
        b <- readBlockHashOrDefault Best block
        getAncestorsV2 b (fromIntegral amount) >>=
          printResponseValueAsJSON
    GetBranches ->
      withClient backend $
        getBranchesV2  >>=
        printResponseValueAsJSON
    GetBannedPeers ->
      withClient backend $
        getBannedPeersV2 >>=
        getResponseValueOrDie >>=
        printJSONValues . toJSON
    Shutdown ->
      withClient backend $
        shutdownV2 >>=
        printSuccess
    DumpStart file raw ->
      withClient backend $
        dumpStartV2 file raw >>=
        printSuccess
    DumpStop ->
      withClient backend $
        dumpStopV2 >>=
        printSuccess
    GetIdentityProviders block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getIdentityProvidersV2 >>=
        printResponseValueAsJSON
    GetAnonymityRevokers block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getAnonymityRevokersV2 >>=
        printResponseValueAsJSON
    GetCryptographicParameters block ->
      withClient backend $
        readBlockHashOrDefault Best block >>=
        getCryptographicParametersV2 >>=
        printResponseValueAsJSON
  where
    -- |Print the response value under the provided mapping,
    -- or fail with an error message if the response contained
    -- an error.
    printResponseValue :: (MonadIO m, Show b)
      => (a -> b)
      -> GRPCResultV2 (Either String a)
      -> m ()
    printResponseValue f res =
      extractResponseValueOrDie f res >>= liftIO . print

    -- |Print the response value as JSON, or fail with an error
    -- message if the response contained an error.
    printResponseValueAsJSON :: (MonadIO m, ToJSON a)
      => GRPCResultV2 (Either String a)
      -> m ()
    printResponseValueAsJSON res = do
      v <- getResponseValueOrDie res
      printJSONValues . toJSON $ v

    -- |Print result of a query with side-effects.
    printSuccess (StatusOk _) = liftIO $ logSuccess ["Success"]
    printSuccess (StatusNotOk (c, x)) = liftIO $ logError [[i|Non-"OK" status code '#{c}' in response: #{x}|]]
    printSuccess StatusInvalid = liftIO $ logError [[i|Invalid status code in response|]]
    printSuccess (RequestFailed x) = liftIO $ logError [[i|Request failed: #{x}|]]

    -- |Parse an account address.
    parseAccountAddress :: (MonadIO m) => Text -> m ID.AccountAddress
    parseAccountAddress accAddr =
      case ID.addressFromText accAddr of
          Left _ -> logFatal ["Unable to parse account address."]
          Right a -> return a

    -- |Print info about a block and possibly its ancestors.
    -- The boolean indicates whether to recurse on the ancestor
    -- of the block. The recursion bottoms out when the genesis
    -- block is reached.
    printBlockInfos :: Bool -> BlockHashInput -> ClientMonad IO ()
    printBlockInfos recurse bh = do
      bi <- getResponseValueOrDie =<< getBlockInfoV2 bh
      printJSONValues $ toJSON bi
      -- Recurse if were instructed to and if we are not at the genesis block.
      when (recurse && Queries.biBlockHeight bi /= 0)
        (printBlockInfos recurse $ Given (Queries.biBlockParent bi))
                
-- |Helper function to specialize the type, avoiding the need for type
-- annotations in many places.
getBlockItemHash :: Types.BareBlockItem -> Types.TransactionHash
getBlockItemHash = getHash

data PeerData = PeerData {
  totalSent     :: Word64,
  totalReceived :: Word64,
  version       :: Text,
  peerStats     :: PeerStatsResponse,
  peerList      :: PeerListResponse
  }

printPeerData :: MonadIO m => Bool -> [Queries.PeerInfo] -> Queries.NodeInfo -> m ()
printPeerData bootstrapper pInfos Queries.NodeInfo{..} =
  let Queries.NetworkInfo{..} = networkInfo
      -- Filter bootstrappers.
      pInfos' = filter (\p -> bootstrapper || Queries.consensusInfo p /= Queries.Bootstrapper) pInfos
  in
  liftIO $ do
      putStrLn $ "Total packets sent: " ++ show peerTotalSent
      putStrLn $ "Total packets received: " ++ show peerTotalReceived
      putStrLn $ "Peer version: " ++ Text.unpack peerVersion

      putStrLn "Peer stats:"
      forM_ pInfos' printPeerInfo

      putStrLn $ "Peer type: " ++ showNodeDetails details

      putStrLn "Peers:"
      forM_ pInfos' printPeerInfo'
  where
    printPeerInfo Queries.PeerInfo{..} =
      let Queries.NetworkStats{..} = networkStats in do
        putStrLn $ "  Peer: " ++ Text.unpack peerId
        putStrLn $ "    Packets sent: " ++ show packetsSent
        putStrLn $ "    Packets received: " ++ show packetsReceived
        putStrLn $ "    Latency: " ++ show latency
        putStrLn ""
    printPeerInfo' Queries.PeerInfo{..} = do
      putStrLn $ "  Node id: " ++ Text.unpack peerId
      putStrLn $ "    Port: " ++ show (Queries.ipPort $ snd socketAddress)
      putStrLn $ "    IP: " ++ Text.unpack (Queries.ipAddress $ fst socketAddress)
      putStrLn $ "    Catchup Status: " ++ showCatchupStatus consensusInfo
      putStrLn ""
    showCatchupStatus =
      \case Queries.UpToDate -> "Up to date"
            Queries.Pending -> "Pending"
            Queries.CatchingUp -> "Catching up"
            Queries.Bootstrapper -> "N/A (Bootstrapper)"
    showNodeDetails =
      \case Queries.NodeBootstrapper -> "Bootstrapper"
            Queries.NodeNotRunning -> "Node (consensus shut down)"
            Queries.NodePassive -> "Node (passive)"
            Queries.NodeActive cInfo ->
                  "Node (running, "
              <>  case Queries.status cInfo of
                    Queries.PassiveBaker _ -> "not baking)"
                    Queries.ActiveBakerCommitteeInfo -> "in current baking committee)"
                    Queries.ActiveFinalizerCommitteeInfo -> "in current baking and finalizer committee)"

getPeerData :: Bool -> ClientMonad IO (Either String PeerData)
getPeerData bootstrapper = do
  totalSent' <- getPeerTotalSent
  totalReceived' <- getPeerTotalReceived
  version' <- getPeerVersion
  peerStats' <- getPeerStats bootstrapper
  peerList' <- getPeerList bootstrapper
  return $ do
    totalSent <- grpcResponseVal <$> totalSent'
    totalReceived <- grpcResponseVal <$> totalReceived'
    version <- grpcResponseVal <$> version'
    peerStats <- grpcResponseVal <$> peerStats'
    peerList <- grpcResponseVal <$> peerList'
    return PeerData{..}

printNodeInfo :: MonadIO m => Queries.NodeInfo -> m ()
printNodeInfo Queries.NodeInfo{..} = liftIO $
  let Queries.NetworkInfo{..} = networkInfo in do
      putStrLn $ "Node ID: " ++ show nodeId
      putStrLn $ "Current local time: " ++ show localTime
      putStrLn $ "Baker ID: " ++ maybe "not a baker" show (getBakerIdM details)
      putStrLn $ "Peer type: " ++ showNodeType details
      putStrLn $ "Baker running: " ++ show (getBakerRunning details)
      putStrLn $ "Consensus running: " ++ show (getConsensusRunning details)
      putStrLn $ "Consensus type: " ++ getConsensusType details
      putStrLn $ "Baker committee member: " ++ getBakerCommitteeMember details
      putStrLn $ "Finalization committee member: " ++ getFinalizerCommitteeMember details
  where showNodeType =
          \case Queries.NodeBootstrapper -> "Bootstrapper"
                Queries.NodeNotRunning -> "Node"
                Queries.NodePassive -> "Node"
                Queries.NodeActive _ -> "Node"
        getBakerRunning =
          \case Queries.NodeBootstrapper -> False
                Queries.NodeNotRunning -> False
                Queries.NodePassive -> False
                Queries.NodeActive _ -> True
        getBakerIdM =
          \case Queries.NodeActive cInfo -> Just . show $ Queries.bakerId cInfo
                _ -> Nothing
        getConsensusRunning =
          \case Queries.NodeBootstrapper -> False
                Queries.NodeNotRunning -> False
                Queries.NodePassive -> True
                Queries.NodeActive _ -> True
        getConsensusType =
          \case Queries.NodeBootstrapper -> "Bootstrapper"
                Queries.NodeNotRunning -> "Not running"
                Queries.NodePassive -> "Passive"
                Queries.NodeActive _ -> "Active"
        getBakerCommitteeMember =
          \case Queries.NodeBootstrapper -> show False
                Queries.NodeNotRunning -> show False
                Queries.NodePassive -> show False
                Queries.NodeActive (Queries.BakerConsensusInfo _ (Queries.PassiveBaker Queries.NotInCommittee)) ->
                  show False
                Queries.NodeActive (Queries.BakerConsensusInfo _ (Queries.PassiveBaker Queries.AddedButNotActiveInCommittee)) ->
                  show False
                Queries.NodeActive (Queries.BakerConsensusInfo _ (Queries.PassiveBaker Queries.AddedButWrongKeys)) ->
                  show False
                Queries.NodeActive (Queries.BakerConsensusInfo bId Queries.ActiveBakerCommitteeInfo) ->
                  "In current baker committee with baker ID '" <> show bId <> "'."
                Queries.NodeActive (Queries.BakerConsensusInfo bId Queries.ActiveFinalizerCommitteeInfo) ->
                  "In current baker committee with baker ID '" <> show bId <> "'."
        getFinalizerCommitteeMember =
          \case
                Queries.NodeActive (Queries.BakerConsensusInfo _ Queries.ActiveBakerCommitteeInfo) ->
                  show False
                Queries.NodeBootstrapper -> show False
                Queries.NodeNotRunning -> show False
                Queries.NodePassive -> show False
                Queries.NodeActive (Queries.BakerConsensusInfo _ (Queries.PassiveBaker Queries.NotInCommittee)) ->
                  show False
                Queries.NodeActive (Queries.BakerConsensusInfo _ (Queries.PassiveBaker Queries.AddedButNotActiveInCommittee)) ->
                  show False
                Queries.NodeActive (Queries.BakerConsensusInfo _ (Queries.PassiveBaker Queries.AddedButWrongKeys)) ->
                  show False
                Queries.NodeActive (Queries.BakerConsensusInfo bId Queries.ActiveFinalizerCommitteeInfo) ->
                  "In current finalizer committee with baker ID " <> show bId <> "'."

-- |FIXME: Move this some other place in refactoring.
data StatusOfPeers = StatusOfPeers {
  -- |How many peers we deem are up-to-date with us.
  numUpToDate :: !Int,
  -- |How many are in limbo, we don't know what the status is with respect to
  -- consensus status of the both of us.
  numPending :: !Int,
  -- |Number of peers we are catching up with.
  numCatchingUp :: !Int
  } deriving(Show, Generic)

instance ToJSON StatusOfPeers
instance FromJSON StatusOfPeers

-- |Get an indication of how caught up the node is in relation to its peers.
getStatusOfPeers :: ClientMonad IO (Either String StatusOfPeers)
getStatusOfPeers = do
  -- False means we don't include the bootstrap nodes here, since they are not running consensus.
  fmap grpcResponseVal <$> getPeerList False <&> \case
    Left err -> (Left err)
    Right peerList -> Right $
      L.foldl' (\status peerElem ->
                  case peerElem ^. CF.catchupStatus of
                    PeerElement'UPTODATE -> status { numUpToDate = numUpToDate status + 1 }
                    PeerElement'PENDING -> status { numPending = numPending status + 1 }
                    PeerElement'CATCHINGUP -> status { numCatchingUp = numCatchingUp status + 1 }
                    _ -> status -- this should not happen in well-formed responses
               )
          (StatusOfPeers 0 0 0)
          (peerList ^. CF.peers)


-- |Process a transaction from JSON payload given as a byte string
-- and with keys given explicitly.
-- The transaction is signed with all the provided keys.
processTransaction ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> ClientMonad m Types.BareBlockItem
processTransaction source networkId =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t  -> processTransaction_ t networkId True

-- |Process a transaction with unencrypted keys given explicitly.
-- The transaction is signed with all the provided keys.
-- This is only for testing purposes and currently used by the middleware.
processTransaction_ ::
     (MonadFail m, MonadIO m)
  => TransactionJSON
  -> Int
  -> Verbose
  -> ClientMonad m Types.BareBlockItem
processTransaction_ transaction _networkId _verbose = do
  let accountKeys = CT.keys transaction
  tx <- do
    let header = metadata transaction
        sender = thSenderAddress header
    nonce <-
      case thNonce header of
        Nothing -> do
          res <- getAccountInfoOrDie sender Best
          return $ Types.aiAccountNonce res
        Just nonce -> return nonce
    txPayload <- convertTransactionJsonPayload $ payload transaction
    return $ encodeAndSignTransaction
      txPayload
      sender
      (thEnergyAmount header)
      nonce
      (thExpiry header)
      accountKeys
  sbiRes <- sendBlockItemV2 tx
  let res = case sbiRes of
        StatusOk resp -> Right resp
        StatusNotOk (status, err) -> Left [[i|GRPC response with status '#{status}': #{err}|]] 
        StatusInvalid -> Left ["GRPC response contained an invalid status code."]
        RequestFailed err -> Left [[i|I/O error: #{err}|]]
  case res of
    Left err -> logFatal $ [[i|Transaction not accepted by the baker:|]] <> err
    Right _ -> return tx

-- |Read a versioned credential from the bytestring, failing if any errors occur.
processCredential ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> ClientMonad m Types.BareBlockItem
processCredential source networkId =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right vCred
        | vVersion vCred == 0 ->
            case fromJSON (vValue vCred) of
              AE.Success cred ->
                let tx = Types.CredentialDeployment cred
                in sendTransactionToBaker tx networkId >>= \case
                  Left err -> fail err
                  Right (GRPCResponse _ False) -> fail "Transaction not accepted by the baker."
                  Right (GRPCResponse _ True) -> return tx
              AE.Error err -> fail $ "Cannot parse credential according to V0: " ++ err
        | otherwise ->
          fail $ "Unsupported credential version: " ++ show (vVersion vCred)

-- |Convert JSON-based transaction type to one which is ready to be encoded, signed and sent.
convertTransactionJsonPayload :: (MonadFail m) => CT.TransactionJSONPayload -> ClientMonad m Types.Payload
convertTransactionJsonPayload = \case
  (CT.DeployModule _) ->
    fail "Use 'concordium-client module deploy' instead."
  CT.InitContract{} ->
    fail "Use 'concordium-client contract init' instead."
  CT.Update{} ->
    fail "Use 'concordium-client contract update' instead."
  (CT.Transfer transferTo transferAmount) ->
    return $ Types.Transfer transferTo transferAmount
  CT.RemoveBaker -> return $ Types.RemoveBaker
  CT.TransferToPublic{..} -> return $ Types.TransferToPublic{..}
  -- FIXME: The following two should have the inputs changed so that they are usable.
  -- They should only specify the amount and the index, and possibly the input encrypted amounts,
  -- but the proofs should be automatically generated here.
  CT.TransferToEncrypted{..} -> return $ Types.TransferToEncrypted{..}
  CT.EncryptedAmountTransfer{..} -> return Types.EncryptedAmountTransfer{..}

-- |Sign a transaction payload and configuration into a "normal" transaction,
-- which is ready to be sent.
encodeAndSignTransaction ::
     Types.Payload
  -> Types.AccountAddress
  -> Types.Energy
  -> Types.Nonce
  -> Types.TransactionExpiryTime
  -> AccountKeyMap
  -> Types.BareBlockItem
encodeAndSignTransaction txPayload = signEncodedTransaction (Types.encodePayload txPayload)

-- |Sign an encoded transaction payload and a configuration into a "normal" transaction,
-- which is ready to be sent.
signEncodedTransaction ::
     Types.EncodedPayload
  -> Types.AccountAddress
  -> Types.Energy
  -> Types.Nonce
  -> Types.TransactionExpiryTime
  -> AccountKeyMap
  -> Types.BareBlockItem
signEncodedTransaction encPayload sender energy nonce expiry accKeys = Types.NormalTransaction $
  let header = Types.TransactionHeader{
        thSender = sender,
        thPayloadSize = Types.payloadSize encPayload,
        thNonce = nonce,
        thEnergyAmount = energy,
        thExpiry = expiry
      }
      keys = Map.toList $ fmap Map.toList accKeys
  in Types.signTransaction keys header encPayload
