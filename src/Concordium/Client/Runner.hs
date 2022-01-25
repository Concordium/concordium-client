{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
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
  -- other auxiliary functions
  , getParseCryptographicParameters
  ) where

import           Concordium.Client.Utils
import           Concordium.Client.Cli
import           Concordium.Client.Config
import           Concordium.Client.Commands          as COM
import           Concordium.Client.Export
import           Concordium.Client.GRPC
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
import           Concordium.Crypto.ByteStringHelpers (deserializeBase16)
import qualified Concordium.Crypto.Proofs            as Proofs
import qualified Concordium.Crypto.SignatureScheme   as SigScheme
import qualified Concordium.Crypto.VRF               as VRF
import           Concordium.Types.UpdateQueues       as Types
import qualified Concordium.Types.Queries            as Queries
import qualified Concordium.Types.Updates            as Updates
import qualified Concordium.Types.Transactions       as Types
import           Concordium.Types.HashableTo
import           Concordium.Types.Parameters
import qualified Concordium.Cost as Cost
import qualified Concordium.Types.Execution          as Types
import qualified Concordium.Types                    as Types
import qualified Concordium.ID.Types                 as ID
import           Concordium.ID.Parameters
import qualified Concordium.Utils.Encryption         as Password
import qualified Concordium.Wasm                     as Wasm
import           Concordium.Constants
import           Proto.ConcordiumP2pRpc
import qualified Proto.ConcordiumP2pRpc_Fields       as CF

import           Control.Exception
import           Control.Monad.Fail
import           Control.Monad.Except
import           Control.Monad.Reader                hiding (fail)
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
import qualified Data.HashMap.Strict                 as HM
import           Data.Maybe
import qualified Data.List                           as L
import qualified Data.Serialize                      as S
import qualified Data.Set                            as Set
import           Data.String
import           Data.String.Interpolate (i, iii)
import           Data.Text(Text)
import qualified Data.Tuple                          as Tuple
import qualified Data.Text                           as Text
import qualified Data.Vector                         as Vec
import           Data.Word
import           Lens.Micro.Platform
import           Network.GRPC.Client.Helpers
import           Prelude                             hiding (fail, unlines)
import           System.IO
import           System.Exit
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.Read (readMaybe)
import Data.Time.Clock (addUTCTime, getCurrentTime, UTCTime)
import Data.Ratio
import Codec.CBOR.Write
import Codec.CBOR.Encoding
import Codec.CBOR.JSON

-- |Establish a new connection to the backend and run the provided computation.
-- Close a connection after completion of the computation. Establishing a
-- connection is expensive, and thus if multiple RPC calls are going to be made
-- they should be made in the context of the same 'withClient' so they reuse it.
withClient :: Backend -> ClientMonad IO a -> IO a
withClient bkend comp = do
  let config = GrpcConfig (COM.grpcHost bkend) (COM.grpcPort bkend) (COM.grpcAuthenticationToken bkend) (COM.grpcTarget bkend) (COM.grpcRetryNum bkend) Nothing
  runExceptT (mkGrpcClient config Nothing) >>= \case
      Left err -> logFatal ["Cannot establish connection to the node: " ++ show err]
      Right client -> do
        let body = runExceptT ((runReaderT . _runClientMonad) comp $! client) >>= \case
              Left err -> fail (show err)
              Right x -> return x
        let closeOrFail Nothing = return ()
            closeOrFail (Just ref) =
              runExceptT (close ref) >>= \case
                Left err -> logFatal ["Error closing connection: " ++ show err]
                Right () -> return ()
        let closeConnection = closeOrFail =<< (readIORef $ grpc client)
        finally body closeConnection

withClientJson :: (FromJSON a) => Backend -> ClientMonad IO (Either String Value) -> IO a
withClientJson b c = withClient b c >>= getFromJson

-- |Helper function for parsing JSON Value or fail if the value is missing or cannot be converted correctly.
-- The parameter has the same type as the one returned by e.g. eitherDecode or processJSON,
-- which many of the GRPC commands use.
getFromJson :: (MonadIO m, FromJSON a) => Either String Value -> m a
getFromJson = getFromJsonAndHandleError onError
  where onError val err = logFatal ["cannot convert '" ++ show val ++ "': " ++ err]

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
      pwd <- askPassword "Enter encryption password: "
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
          tailTransaction_ hash
--          logSuccess [ "transaction successfully completed" ]
    TransactionDeployCredential fname intOpts -> do
      source <- handleReadFile BSL.readFile fname
      withClient backend $ do
        tx <- processCredential source defaultNetId
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction_ hash
--          logSuccess [ "credential deployed successfully" ]
    TransactionStatus h -> do
      hash <- case parseTransactionHash h of
        Nothing -> logFatal [printf "invalid transaction hash '%s'" h]
        Just hash -> return hash
      status <- withClient backend $ queryTransactionStatus hash
      when verbose $ logInfo [printf "Response: %s" (show status)]
      runPrinter $ printTransactionStatus status
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
        cs <- getConsensusStatus >>= getFromJson
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
        sendAndTailTransaction_ txCfg pl intOpts

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
        cs <- getConsensusStatus >>= getFromJson
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
        case fromAddr == toAddr of
          False -> do
            let intOpts = toInteractionOpts txOpts
            liftIO $ transferWithScheduleTransactionConfirm ttxCfg (ioConfirm intOpts)
            sendAndTailTransaction_ txCfg pl intOpts
          True -> liftIO $ do 
            logWarn ["Scheduled transfers from an account to itself are not allowed."]
            logWarn ["Transaction Cancelled"]

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
        cs <- getConsensusStatus >>= getFromJson
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
        sendAndTailTransaction_ txCfg payload intOpts

    TransactionRegisterData file txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      rdCfg <- getRegisterDataTransactionCfg baseCfg txOpts file
      let txCfg = rdtcTransactionCfg rdCfg
      let nrg = tcEnergy txCfg

      logInfo [[i|Register data. Allowing up to #{showNrg nrg} to be spent as transaction fee.|]]

      registerConfirmed <- askConfirmation Nothing
      when registerConfirmed $ do
        logInfo ["Registering data..."]

        let intOpts = toInteractionOpts txOpts
        let pl = registerDataTransactionPayload rdCfg

        withClient backend $ do
          mTsr <- sendAndTailTransaction txCfg (Types.encodePayload pl) intOpts
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
  (bbHash, infoValue) <- logFatalOnError =<< withBestBlockHash Nothing (\bbHash -> ((bbHash,) <$>) <$> getAccountInfo (Text.pack . show $ senderAddr) bbHash)
  case AE.fromJSON infoValue of
    AE.Error err -> logFatal ["Cannot decode account info response from the node: " ++ err]
    AE.Success Nothing -> logFatal [printf "Account %s does not exist on the chain." $ show senderAddr]
    AE.Success (Just AccountInfoResult{airEncryptedAmount=a@Types.AccountEncryptedAmount{..}}) -> do
      let listOfEncryptedAmounts = Types.getIncomingAmountsList a
      taker <- case idx of
                Nothing -> return id
                Just v ->
                  if v < fromIntegral _startIndex
                     || v > fromIntegral _startIndex + length listOfEncryptedAmounts
                  then logFatal ["The index provided must be at least the index of the first incoming amount on the account and at most `start index + number of incoming amounts`"]
                  else return $ take (v - fromIntegral _startIndex)
      -- get receiver's public encryption key
      infoValueReceiver <- logFatalOnError =<< withBestBlockHash (Just bbHash) (getAccountInfo (Text.pack . show $ naAddr ettReceiver))
      case AE.fromJSON infoValueReceiver of
        AE.Error err -> logFatal ["Cannot decode account info response from the node: " ++ err]
        AE.Success Nothing -> logFatal [printf "Account %s does not exist on the chain." $ show $ naAddr ettReceiver]
        AE.Success (Just air) -> do
          globalContext <- logFatalOnError =<< getParseCryptographicParameters bbHash

          let receiverPk = ID._elgamalPublicKey $ airEncryptionKey air
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
getBakerCooldown :: Queries.BlockSummary -> ClientMonad IO UTCTime
getBakerCooldown bs = do
  cs <- getFromJson =<< getConsensusStatus
  let epochTime = (toInteger $ Time.durationMillis $ Queries.csEpochDuration cs) % 1000
  let cooldownTime = fromRational $ epochTime * ((cooldownEpochs + 2) % 1)
  currTime <- liftIO getCurrentTime
  let cooldownDate = addUTCTime cooldownTime currTime
  return cooldownDate
  where
    cooldownEpochs = Queries.bsWithUpdates bs $ \spv ups ->
        case Types.chainParametersVersionFor spv of
            Types.SCPV0 -> cooldownEpochsV0 ups
            Types.SCPV1 -> cooldownEpochsV1 ups
    cooldownEpochsV0 ups =
        toInteger $ ups ^. Types.currentParameters ^. cpCooldownParameters ^. cpBakerExtraCooldownEpochs
    cooldownEpochsV1 ups =
        let cp = ups ^. Types.currentParameters
            numPeriods = cp ^. cpCooldownParameters ^. cpPoolOwnerCooldown
            periodEpochLen = cp ^. cpTimeParameters ^. tpRewardPeriodLength
        in toInteger numPeriods * toInteger periodEpochLen

-- |Query the chain for the given account. Fail if either the chain cannot be reached, or
-- if the account does not exist.
getAccountInfoOrDie :: ID.AccountAddress ->  ClientMonad IO AccountInfoResult
getAccountInfoOrDie senderAddr = do
  infoValue <- logFatalOnError =<< withBestBlockHash Nothing (getAccountInfo (Text.pack . show $ senderAddr))
  case AE.fromJSON infoValue of
    AE.Error err -> logFatal ["Cannot decode account info response from the node: " ++ err]
    AE.Success Nothing -> logFatal [printf "Account %s does not exist on the chain." $ show senderAddr]
    AE.Success (Just air) -> return air

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
  (bbHash, infoValue) <- logFatalOnError =<< withBestBlockHash Nothing (\bbh -> ((bbh, ) <$>) <$> getAccountInfo (Text.pack . show $ senderAddr) bbh)
  case AE.fromJSON infoValue of
    AE.Error err -> logFatal ["Cannot decode account info response from the node: " ++ err]
    AE.Success Nothing -> logFatal [printf "Account %s does not exist on the chain." $ show senderAddr]
    AE.Success (Just AccountInfoResult{airEncryptedAmount=a@Types.AccountEncryptedAmount{..}}) -> do
      globalContext <- logFatalOnError =<< getParseCryptographicParameters bbHash

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
  case AE.fromJSON vglobalContext of
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
  blockSummary <- getFromJson =<< withLastFinalBlockHash Nothing getBlockSummary
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
                     Nothing -> liftIO $ failOnError $ decryptAccountKeyMapInteractive esdKeys (Nothing) Nothing
  let sender = applyAlias tcAlias naAddr
  let tx = signEncodedTransaction pl sender energy nonce expiry accountKeyMap
  when (isJust tcAlias) $
      logInfo [[i|Using the alias #{sender} as the sender of the transaction instead of #{naAddr}.|]]
  sendTransactionToBaker tx defaultNetId >>= \case
    Left err -> fail err
    Right False -> fail "transaction not accepted by the baker"
    Right True -> return tx

-- |Fetch next nonces relative to the account's most recently committed and
-- pending transactions, respectively.
-- If they match, the nonce is returned.
-- If they don't match, optionally ask the user to confirm proceeding with the latter nonce.
-- If rejected, the process is cancelled (exit with code 0).
getNonce :: (MonadFail m, MonadIO m) => Types.AccountAddress -> Maybe Types.Nonce -> Bool -> ClientMonad m Types.Nonce
getNonce sender nonce confirm =
  case nonce of
    Nothing -> do
      currentNonce <- getBestBlockHash >>= getAccountNonce sender
      nextNonce <- Queries.nanNonce <$> (getNextAccountNonce (Text.pack $ show sender) >>= getFromJson)
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
    => TransactionConfig -- ^ Information about the sender, and the context of transaction
    -> Types.EncodedPayload -- ^ Payload of the transaction to send
    -> InteractionOpts -- ^ How interactive should sending and tailing be
    -> ClientMonad m ()
sendAndTailTransaction_ txCfg pl intOpts = void $ sendAndTailTransaction txCfg pl intOpts

-- |Send a transaction and optionally tail it (see 'tailTransaction' below).
-- If tailed, it returns the TransactionStatusResult of the finalized status,
-- otherwise the return value is `Nothing`.
sendAndTailTransaction :: (MonadIO m, MonadFail m)
    => TransactionConfig -- ^ Information about the sender, and the context of transaction
    -> Types.EncodedPayload -- ^ Payload of the transaction to send
    -> InteractionOpts -- ^ How interactive should sending and tailing be
    -> ClientMonad m (Maybe TransactionStatusResult)
sendAndTailTransaction txCfg pl intOpts = do
  tx <- startTransaction txCfg pl (ioConfirm intOpts) Nothing
  let hash = getBlockItemHash tx
  logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
  if ioTail intOpts
  then Just <$> tailTransaction hash
  else return Nothing

-- |Continuously query and display transaction status until the transaction is finalized.
tailTransaction_ :: (MonadIO m) => Types.TransactionHash -> ClientMonad m ()
tailTransaction_ hash = void $ tailTransaction hash

-- |Continuously query and display transaction status until the transaction is finalized.
-- Returns the TransactionStatusResult of the finalized status.
tailTransaction :: (MonadIO m) => Types.TransactionHash -> ClientMonad m TransactionStatusResult
tailTransaction hash = do
  logInfo [ "waiting for the transaction to be committed and finalized"
          , "you may skip this step by interrupting the command using Ctrl-C (pass flag '--no-wait' to do this by default)"
          , printf "the transaction will still get processed and may be queried using\n  'concordium-client transaction status %s'" (show hash) ]

  liftIO $ printf "[%s] Waiting for the transaction to be committed..." =<< getLocalTimeOfDayFormatted
  committedStatus <- awaitState 2 Committed hash
  liftIO $ putStrLn ""

  when (tsrState committedStatus == Absent) $
    logFatal [ "transaction failed before it got committed"
             , "most likely because it was invalid" ]

  runPrinter $ printTransactionStatus committedStatus

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
    runPrinter $ printTransactionStatus finalizedStatus

    return finalizedStatus
  liftIO $ printf "[%s] Transaction finalized.\n" =<< getLocalTimeOfDayFormatted
  return status

  where
    getLocalTimeOfDayFormatted = showTimeOfDay <$> getLocalTimeOfDay

addCurrentEraGenesisTimeToEpochDuration :: Queries.ConsensusStatus -> Word64 -> UTCTime
addCurrentEraGenesisTimeToEpochDuration cs e =
    let d = Time.durationMillis $ Queries.csEpochDuration cs
    in addUTCTime
        (fromRational ((toInteger e * toInteger d) % 1000))
        (Queries.csCurrentEraGenesisTime cs)

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
          logInfo [[i|the ACCOUNT argument was not provided; using the default account name '#{defaultAccountName}'|]]
          return defaultAccountName
        Just acc -> return acc

      accountIdentifier <- case deserializeBase16 input of 
                Just (_ :: ID.CredentialRegistrationID) -> return input -- input is a wellformed credRegID
                Nothing -> case ID.addressFromText input of
                  Right _ -> return input -- input is a wellformed address
                  Left _ -> case Map.lookup input (bcAccountNameMap baseCfg) of
                    Just a -> return $ Text.pack $ show a -- input is the local name of an account
                    Nothing -> if isNothing inputMaybe
                               then logFatal [[i|The ACCOUNT argument was not provided; so the default account name '#{defaultAccountName}' was used, but no account with that name exists.|]]
                               else logFatal [[i|The identifier '#{input}' is neither a credential registration ID, the address nor the name of an account|]]

      (accInfo, na, dec, f) <- withClient backend $ do
        -- query account
        (bbh, accInfoValue :: Either String Value) <- withBestBlockHash block (\bbh -> (bbh,) <$> getAccountInfo accountIdentifier bbh)
        accInfo <- do 
          case accInfoValue of
            Left err -> logFatal [err]
            Right aiv -> case AE.fromJSON aiv of
                            AE.Error err -> logFatal ["Cannot decode account info response from the node: " ++ err]
                            AE.Success Nothing -> logFatal [printf "Account %s does not exist on the chain." $ show accountIdentifier]
                            AE.Success (Just air) -> return air
        -- derive the address of the account from the the initial credential
        resolvedAddress <- case Map.lookup (ID.CredentialIndex 0) (airCredentials accInfo) of
                            Nothing -> logFatal [printf "No initial credential found for the account identified by '%s'" accountIdentifier ]
                            Just v -> return $ ID.addressFromRegId $ ID.credId $ vValue v
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
        cs <- getFromJson =<< getConsensusStatus
        case encKey of
          Nothing ->
            return (accInfo, na, Nothing, addCurrentEraGenesisTimeToEpochDuration cs)
          Just k -> do
            gc <- logFatalOnError =<< getParseCryptographicParameters bbh
            return (accInfo, na, Just (k, gc), addCurrentEraGenesisTimeToEpochDuration cs)

      runPrinter $ printAccountInfo f na accInfo verbose (showEncrypted || showDecrypted) dec

    AccountList block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      accs <- withClientJson backend $ withBestBlockHash block getAccountList
      runPrinter $ printAccountList (bcAccountNameMap baseCfg) accs

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

        accInfo <- getAccountInfoOrDie senderAddress
        let numCredentials = Map.size $ airCredentials accInfo
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
        sendAndTailTransaction_ txCfg pl intOpts

    AccountUpdateCredentials cdisFile removeCidsFile newThreshold txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      accCfg <- liftIO $ getAccountCfgFromTxOpts baseCfg txOpts
      let senderAddress = naAddr $ esdAddress accCfg

      withClient backend $ do
        accInfo <- getAccountInfoOrDie senderAddress
        (epayload, numKeys, newCredentials, removedCredentials) <- liftIO $ getAccountUpdateCredentialsTransactionData cdisFile removeCidsFile newThreshold
        let numExistingCredentials =  Map.size (airCredentials accInfo)
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
        sendAndTailTransaction_ txCfg epayload intOpts

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
      withClient backend $ sendAndTailTransaction_ txCfg pl intOpts

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
        sendAndTailTransaction_ txCfg pl intOpts

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
    ModuleDeploy modFile modName txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose

      mdCfg <- getModuleDeployTransactionCfg baseCfg txOpts modFile
      let txCfg = mdtcTransactionCfg mdCfg

      let nrg = tcEnergy txCfg

      let msgIntro = case modName of
            Nothing -> [i|deploy the module '#{modFile}'|]
            Just modName' -> [i|deploy the module '#{modFile}' and name it '#{modName'}'|]
      logInfo [ msgIntro
              , [i|allowing up to #{showNrg nrg} to be spent as transaction fee|]]

      deployConfirmed <- askConfirmation Nothing

      when deployConfirmed $ do
          logInfo ["deploying module..."]

          let intOpts = toInteractionOpts txOpts
          let pl = moduleDeployTransactionPayload mdCfg

          withClient backend $ do
            mTsr <- sendAndTailTransaction txCfg (Types.encodePayload pl) intOpts
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
      (bestBlock, res) <- withClient backend $ withBestBlockHash block $ \bb -> (bb,) <$> getModuleList bb
      v <- getFromJsonAndHandleError (\_ _ -> logFatal ["could not retrieve the list of modules",
                                   "the provided block hash is invalid:", Text.unpack bestBlock]) res
      case v of
        Nothing -> logFatal ["could not retrieve the list of modules",
                               "the provided block does not exist:", Text.unpack bestBlock]
        Just [] -> logInfo ["there are no modules in block " ++ Text.unpack bestBlock]
        Just xs -> runPrinter $ printModuleList (bcModuleNameMap baseCfg) xs

    ModuleShow modRefOrName outFile block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      namedModRef <- getNamedModuleRef (bcModuleNameMap baseCfg) modRefOrName
      Wasm.WasmModule{..} <- withClient backend . withBestBlockHash block $ getWasmModule namedModRef
      logInfo [[i|WASM Version of module: #{wasmVersion}|]]
      case outFile of
        -- Write to stdout
        "-" -> BS.putStr . Wasm.moduleSource $ wasmSource
        -- Write to file
        _   -> do
          success <- handleWriteFile BS.writeFile PromptBeforeOverwrite verbose outFile (Wasm.moduleSource wasmSource)
          when success $ logSuccess [[i|wrote module source to the file '#{outFile}'|]]

    ModuleInspect modRefOrName schemaFile block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      namedModRef <- getNamedModuleRef (bcModuleNameMap baseCfg) modRefOrName
      (schema, exports) <- withClient backend . withBestBlockHash block $ getSchemaAndExports schemaFile namedModRef
      runPrinter $ printModuleInspectInfo namedModRef schema exports

    ModuleName modRefOrFile modName -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      modRef <- getModuleRefFromRefOrFile modRefOrFile
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

getModuleDeployTransactionCfg :: BaseConfig -> TransactionOpts (Maybe Types.Energy) -> FilePath -> IO ModuleDeployTransactionCfg
getModuleDeployTransactionCfg baseCfg txOpts moduleFile = do
  wasmModule <- getWasmModuleFromFile moduleFile
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

-- |Process a 'contract ...' command.
processContractCmd :: ContractCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processContractCmd action baseCfgDir verbose backend =
  case action of
    ContractList block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      (bestBlock, res) <- withClient backend $ withBestBlockHash block $ \bb -> (bb,) <$> getInstances bb
      v <- getFromJsonAndHandleError (\_ _ -> logFatal ["could not retrieve the list of contracts",
                                   "the provided block hash is invalid:", Text.unpack bestBlock]) res
      case v of
        Nothing -> logFatal ["could not retrieve the list of contracts",
                               "the provided block does not exist:", Text.unpack bestBlock]
        Just [] -> logInfo ["there are no contract instances in block " ++ Text.unpack bestBlock]
        Just xs -> runPrinter $ printContractList (bcContractNameMap baseCfg) xs

    ContractShow indexOrName subindex schemaFile block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      namedContrAddr <- getNamedContractAddress (bcContractNameMap baseCfg) indexOrName subindex
      (schema, contrInfo, namedOwner, namedModRef) <- withClient backend . withBestBlockHash block $ \bb -> do
        contrInfo@CI.ContractInfo{..} <- getContractInfo namedContrAddr bb
        let namedModRef = NamedModuleRef {nmrRef = ciSourceModule, nmrNames = findAllNamesFor (bcModuleNameMap baseCfg) ciSourceModule}
        schema <- getSchemaFromFileOrModule schemaFile (Right namedModRef) bb
        let namedOwner = NamedAddress {naAddr = ciOwner, naNames = findAllNamesFor (bcAccountNameMap baseCfg) ciOwner}
        return (schema, contrInfo, namedOwner, namedModRef)
      displayContractInfo schema contrInfo namedOwner namedModRef

    ContractInit modTBD contrName paramsFileJSON paramsFileBinary schemaFile contrAlias isPath amount txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      ciCfg <- getContractInitTransactionCfg backend baseCfg txOpts modTBD isPath contrName
                paramsFileJSON paramsFileBinary schemaFile amount
      let txCfg = citcTransactionCfg ciCfg
      let energy = tcEnergy txCfg
      let expiryTs = tcExpiry txCfg

      let minEnergy = contractInitMinimumEnergy ciCfg (tcEncryptedSigningData txCfg)
      when (energy < minEnergy) $ logFatal [ "insufficient energy provided"
                                           , [iii|to verify the transaction signature #{showNrg minEnergy} is needed,
                                                  and additional energy is needed to complete the initialization|]]

      logInfo [ [i|initialize contract '#{contrName}' from module '#{citcModuleRef ciCfg}' with |]
                  ++ paramsMsg paramsFileJSON paramsFileBinary ++ [i| Sending #{Types.amountToString $ citcAmount ciCfg} CCD.|]
              , [i|allowing up to #{showNrg energy} to be spent as transaction fee|]
              , [i|transaction expires on #{showTimeFormatted $ timeFromTransactionExpiryTime expiryTs}|]]

      initConfirmed <- askConfirmation Nothing

      when initConfirmed $ do
        let intOpts = toInteractionOpts txOpts
        let pl = contractInitTransactionPayload ciCfg
        withClient backend $ do
          mTsr <- sendAndTailTransaction txCfg (Types.encodePayload pl) intOpts
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

    ContractUpdate indexOrName subindex receiveName paramsFileJSON paramsFileBinary schemaFile amount txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      cuCfg <- getContractUpdateTransactionCfg backend baseCfg txOpts indexOrName subindex
                receiveName paramsFileJSON paramsFileBinary schemaFile amount
      let txCfg = cutcTransactionCfg cuCfg
      let energy = tcEnergy txCfg
      let expiryTs = tcExpiry txCfg

      let minEnergy = contractUpdateMinimumEnergy cuCfg (tcEncryptedSigningData txCfg)
      when (energy < minEnergy) $ logFatal [ "insufficient energy provided"
                                           , [iii|to verify the transaction signature #{showNrg minEnergy} is needed,
                                                  and additional energy is needed to complete the update|]]

      logInfo [ [i|update contract '#{cutcContrName cuCfg}' using the function '#{receiveName}' with |]
                  ++ paramsMsg paramsFileJSON paramsFileBinary ++ [i| Sending #{Types.amountToString $ cutcAmount cuCfg} CCD.|]
              , [i|allowing up to #{showNrg energy} to be spent as transaction fee|]
              , [i|transaction expires on #{showTimeFormatted $ timeFromTransactionExpiryTime expiryTs}|]]

      updateConfirmed <- askConfirmation Nothing

      when updateConfirmed $ do
        let intOpts = toInteractionOpts txOpts
        let pl = contractUpdateTransactionPayload cuCfg
        withClient backend $ do
          mTsr <- sendAndTailTransaction txCfg (Types.encodePayload pl) intOpts
          case extractUpdate mTsr of
            Nothing -> return ()
            Just (Left err) -> logFatal ["updating contract instance failed:", err]
            Just (Right _) -> do
              namedContrAddr <- getNamedContractAddress (bcContractNameMap baseCfg) indexOrName subindex
              logSuccess [[iii|successfully updated contract instance #{showNamedContractAddress namedContrAddr}
                                                using the function '#{receiveName}'|]]

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
        extractUpdate = extractFromTsr (\case
                                        Types.Updated {} -> Just ()
                                        _ -> Nothing)
        paramsMsg paramsFileJSON paramsFileBinary = case (paramsFileJSON, paramsFileBinary) of
            (Nothing, Nothing) -> "no parameters."
            (Nothing, Just binFile) -> [i|binary parameters from '#{binFile}'.|]
            (Just jsonFile, Nothing) -> [i|JSON parameters from '#{jsonFile}'.|]
            -- This case should already have failed while creating the config.
            _ -> ""

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

-- |Try to fetch info about the contract and deserialize it from JSON.
-- Or, log fatally with appropriate error messages if anything goes wrong.
getContractInfo :: NamedContractAddress -> Text -> ClientMonad IO CI.ContractInfo
getContractInfo namedContrAddr block = do
  res <- getInstanceInfo (Text.pack . showCompactPrettyJSON . ncaAddr $ namedContrAddr) block
  case res of
    Left err -> logFatal ["I/O error:", err]
    -- TODO: Handle nonexisting blocks separately from nonexisting contracts.
    Right AE.Null -> logFatal [[i|the contract instance #{showNamedContractAddress namedContrAddr} does not exist in block #{block}|]]
    Right contrInfo -> case AE.fromJSON contrInfo of
      Error err -> logFatal ["Could not decode contract info:", err]
      Success info -> pure info

-- |Display contract info, optionally using a schema to decode the contract state.
displayContractInfo :: Maybe CS.ModuleSchema -> CI.ContractInfo -> NamedAddress -> NamedModuleRef -> IO ()
displayContractInfo schema contrInfo namedOwner namedModRef = case schema of
  Nothing -> runPrinter $ printContractInfo contrInfo namedOwner namedModRef
  Just schema' -> case CI.decodeContractStateUsingSchema contrInfo schema' of
    Left err' -> logFatal ["Parsing the contract model failed:", err']
    Right infoWithSchema -> runPrinter $ printContractInfo infoWithSchema namedOwner namedModRef

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
                                -> Maybe FilePath -- ^ Optional parameter file in JSON format.
                                -> Maybe FilePath -- ^ Optional parameter file in binary format.
                                -> Maybe FilePath -- ^ Optional schema file.
                                -> Types.Amount   -- ^ `Amount` to send to the contract.
                                -> IO ContractUpdateTransactionCfg
getContractUpdateTransactionCfg backend baseCfg txOpts indexOrName subindex receiveName
                                paramsFileJSON paramsFileBinary schemaFile amount = do
  txCfg <- getRequiredEnergyTransactionCfg baseCfg txOpts
  namedContrAddr <- getNamedContractAddress (bcContractNameMap baseCfg) indexOrName subindex
  CI.ContractInfo{ciSourceModule = moduleRef,..} <- withClient backend . withBestBlockHash Nothing $ getContractInfo namedContrAddr
  let namedModRef = NamedModuleRef {nmrRef = moduleRef, nmrNames = []}
  let contrName = CI.contractNameFromInitName ciName
  params <- getWasmParameter backend paramsFileJSON paramsFileBinary schemaFile (Right namedModRef)
            (CS.ReceiveFuncName contrName receiveName)
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
                              -> Text   -- ^ Name of contract to init.
                              -> Maybe FilePath -- ^ Optional parameter file in JSON format.
                              -> Maybe FilePath -- ^ Optional parameter file in binary format.
                              -> Maybe FilePath -- ^ Optional schema file.
                              -> Types.Amount   -- ^ `Amount` to send to the contract.
                              -> IO ContractInitTransactionCfg
getContractInitTransactionCfg backend baseCfg txOpts modTBD isPath contrName paramsFileJSON paramsFileBinary schemaFile amount = do
  namedModRef <- if isPath
            then (\ref -> NamedModuleRef {nmrRef = ref, nmrNames = []}) <$> getModuleRefFromFile modTBD
            else getNamedModuleRef (bcModuleNameMap baseCfg) (Text.pack modTBD)
  txCfg <- getRequiredEnergyTransactionCfg baseCfg txOpts
  params <- getWasmParameter backend paramsFileJSON paramsFileBinary schemaFile (Right namedModRef) (CS.InitFuncName contrName)
  return $ ContractInitTransactionCfg txCfg amount (nmrRef namedModRef) (Wasm.InitName [i|init_#{contrName}|]) params

-- |Query the node for a module reference, and parse the result.
-- Terminate program execution if either the module cannot be obtained,
-- or the result cannot be parsed.
getWasmModule :: NamedModuleRef -- ^On-chain reference of the module.
              -> Text -- ^Hash of the block to query in.
              -> ClientMonad IO Wasm.WasmModule
getWasmModule namedModRef block = do
  res <- getModuleSource (Text.pack . show $ nmrRef namedModRef) block

  case res of
    Left err -> logFatal ["I/O error:", err]
    Right "" -> logFatal [[i|the module reference #{showNamedModuleRef namedModRef} does not exist in block #{block}|]]
    Right unparsedWasmMod -> case S.decode unparsedWasmMod of
      Left err' -> logFatal [[i|could not decode Wasm Module:|], err']
      Right wasmMod -> return wasmMod

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
-- It defaults to our internal wasmVersion of 0, which essentially is the
-- on-chain API version.
getWasmModuleFromFile :: FilePath -> IO Wasm.WasmModule
getWasmModuleFromFile moduleFile = do
  source <- handleReadFile BS.readFile moduleFile
  let moduleSize = BS.length source
  when (moduleSize > fromIntegral maxWasmModuleSize) $ logWarn [[i|Module file #{moduleFile} of size #{moduleSize} bytes is bigger than the maximum of #{maxWasmModuleSize} bytes, and will most likely be rejected on chain.|]]
  return $ Wasm.WasmModule 0 . Wasm.ModuleSource $ source

-- |Load `Wasm.Parameter` through one of several ways, dependent on the arguments:
--   * If binary file provided -> Read the file and wrap its contents in `Wasm.Parameter`.
--   * If JSON file provided   -> Try to get a schema using `getSchemaFromFileOrModule` and use it to encode the parameters
--                                into a `Wasm.Parameter`.
-- If invalid arguments are provided or something fails, appropriate warning or error messages are logged.
getWasmParameter :: Backend
                 -> Maybe FilePath -- ^ Optional parameter file in JSON format.
                 -> Maybe FilePath -- ^ Optional parameter file in binary format.
                 -> Maybe FilePath -- ^ Optional schema file.
                 -> Either Wasm.ModuleSource NamedModuleRef -- ^ Module source OR a `NamedModuleRef`.
                 -> CS.FuncName -- ^ A func name used for finding the func signature in the schema.
                 -> IO Wasm.Parameter
getWasmParameter backend paramsFileJSON paramsFileBinary schemaFile modSourceOrRef funcName =
  case (paramsFileJSON, paramsFileBinary, schemaFile) of
    (Nothing, Nothing, Nothing) -> emptyParams
    (Nothing, Nothing, Just _) -> logWarn ["ignoring the --schema as no --params were provided"] *> emptyParams
    (Nothing, Just binaryFile, Nothing) -> binaryParams binaryFile
    (_, Just binaryFile, Just _) -> logWarn ["ignoring the --schema as --parameter-bin was used"] *> binaryParams binaryFile
    (Just jsonFile', Nothing, _) -> do
      schema <- withClient backend . withBestBlockHash Nothing $ getSchemaFromFileOrModule schemaFile modSourceOrRef
      case schema of
        Nothing -> logFatal [[iii|could not parse the json parameter because the module does not include an embedded schema.
                                  Supply a schema using the --schema flag|]]
        Just schema' -> getFromJSONParams jsonFile' schema'
    (Just _, Just _, _) -> logFatal ["--parameter-json and --parameter-bin cannot be used at the same time"]
  where getFromJSONParams :: FilePath -> CS.ModuleSchema -> IO Wasm.Parameter
        getFromJSONParams jsonFile schema = case CS.lookupSignatureForFunc schema funcName of
          Nothing -> logFatal [[i|the schema did not include the provided function|]]
          Just schemaForParams -> do
            jsonFileContents <- handleReadFile BSL8.readFile jsonFile
            let params = AE.eitherDecode jsonFileContents >>= CP.encodeParameter schemaForParams
            case params of
              Left errParams -> logFatal [[i|Could not decode parameters from file '#{jsonFile}' as JSON:|], errParams]
              Right params' -> pure . Wasm.Parameter . BS.toShort $ params'

        emptyParams = pure . Wasm.Parameter $ BSS.empty
        binaryParams file = Wasm.Parameter . BS.toShort <$> handleReadFile BS.readFile file

-- |Get a schema from a file or, alternatively, try to extract an embedded schema from a module.
-- Logs fatally if an invalid schema is found (either from a file or embedded).
-- The schema from the file will take precedence over an embedded schema in the module.
-- Only returns `Nothing` if no schemaFile is provided and no embedded schema was found in the module.
getSchemaFromFileOrModule :: Maybe FilePath -- ^ Optional schema file.
                          -> Either Wasm.ModuleSource NamedModuleRef -- ^ Either a Wasm Module or a reference to a module on chain.
                          -> Text -- ^ A block hash.
                          -> ClientMonad IO (Maybe CS.ModuleSchema)
getSchemaFromFileOrModule schemaFile modSourceOrRef block = case (schemaFile, modSourceOrRef) of
  (Nothing, Left modSource) -> liftIO $ getSchemaFromModuleOrDie modSource
  (Nothing, Right namedModRef) -> do
    Wasm.WasmModule{..} <- getWasmModule namedModRef block
    liftIO $ getSchemaFromModuleOrDie wasmSource
  (Just schemaFile', _) -> liftIO (Just <$> getSchemaFromFile schemaFile')
  where getSchemaFromModuleOrDie (Wasm.ModuleSource modSrc) = case CS.decodeEmbeddedSchema modSrc of
          Left err -> logFatal [[i|Could not parse embedded schema from module:|], err]
          Right schema -> return schema

-- |Try to load and decode a schema from a file. Logs fatally if the file is not a valid Wasm module.
getSchemaFromFile :: FilePath -> IO CS.ModuleSchema
getSchemaFromFile schemaFile = do
  schema <- CS.decodeModuleSchema <$> handleReadFile BS.readFile schemaFile
  case schema of
    Left err -> logFatal [[i|Could not decode schema from file '#{schemaFile}':|], err]
    Right schema' -> pure schema'

-- |Get a schema and a list of exported function names from an optional schema file and a module reference.
-- Logs fatally if an invalid schema is found (either from a file or embedded).
-- The schema from the file will take precedence over an embedded schema in the module.
-- It will only return `(Nothing, _)` if no schemafile is provided and no embedded schema was found in the module.
getSchemaAndExports :: Maybe FilePath -- ^ Optional schema file.
                    -> NamedModuleRef -- ^ Module reference used for looking up a schema and exports.
                    -> Text -- ^ A block hash.
                    -> ClientMonad IO (Maybe CS.ModuleSchema, [Text])
getSchemaAndExports schemaFile namedModRef block = do

  preferredSchema <- case schemaFile of
    Nothing -> return Nothing
    Just schemaFile' -> fmap Just . liftIO . getSchemaFromFile $ schemaFile'

  (schema, exports) <- do
      Wasm.WasmModule{..} <- getWasmModule namedModRef block
      liftIO $ getSchemaAndExportsOrDie wasmSource

  if isJust preferredSchema
  then return (preferredSchema, exports)
  else return (schema, exports)

  where getSchemaAndExportsOrDie (Wasm.ModuleSource modSrc) = case CS.decodeEmbeddedSchemaAndExports modSrc of
          Left err -> logFatal [[i|Could not parse embedded schema or exports from module:|], err]
          Right schemaAndExports -> return schemaAndExports

-- |Try to parse the input as a module reference and assume it is a path if it fails.
getModuleRefFromRefOrFile :: String -> IO Types.ModuleRef
getModuleRefFromRefOrFile modRefOrFile = case readMaybe modRefOrFile of
  Just modRef -> pure modRef
  Nothing -> getModuleRefFromFile modRefOrFile

-- |Load the module file and compute its hash, which is the reference.
getModuleRefFromFile :: String -> IO Types.ModuleRef
getModuleRefFromFile file = Types.ModuleRef . getHash <$> getWasmModuleFromFile file

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
      v <- withClientJson backend getConsensusStatus
      runPrinter $ printConsensusStatus v
    ConsensusShowParameters b includeBakers -> do
      baseCfg <- getBaseConfig _baseCfgDir verbose
      v <- withClientJson backend $ withBestBlockHash b getBirkParameters
      case v of
        Nothing -> putStrLn "Block not found."
        Just p -> runPrinter $ printBirkParameters includeBakers p addrmap
                    where
                      addrmap = Map.fromList . map Tuple.swap . Map.toList $ bcAccountNameMap baseCfg

    ConsensusChainUpdate rawUpdateFile keysFiles intOpts -> do
      let
        loadJSON :: (FromJSON a) => FilePath -> IO a
        loadJSON fn = AE.eitherDecodeFileStrict fn >>= \case
          Left err -> logFatal [fn ++ ": " ++ err]
          Right r -> return r
      rawUpdate@Updates.RawUpdateInstruction{..} <- loadJSON rawUpdateFile
      eBlockSummaryJSON <- withClient backend $ withBestBlockHash Nothing getBlockSummary
      Queries.BlockSummary {bsProtocolVersion = _ :: Types.SProtocolVersion pv, bsUpdates = eUpdates} <-
        case eBlockSummaryJSON of
          Right jsn ->
            case fromJSON jsn of
              AE.Success bs -> return bs
              AE.Error e -> logFatal [printf "Error parsing a JSON for block summary: '%s'" (show e)]
          Left e -> logFatal [printf "Error getting block summary: '%s'" (show e)]
      let keyCollectionStore = eUpdates ^. Types.currentKeyCollection ^. Types.unhashed
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
        sendTransactionToBaker tx defaultNetId >>= \case
          Left err -> fail err
          Right False -> fail "update instruction not accepted by the node"
          Right True -> logSuccess [printf "update instruction '%s' sent to the baker" (show hash)]
        when (ioTail intOpts) $
          tailTransaction_ hash

-- |Process a 'block ...' command.
processBlockCmd :: BlockCmd -> Verbose -> Backend -> IO ()
processBlockCmd action _ backend =
  case action of
    BlockShow b -> do
      when (maybe False (isNothing . parseTransactionHash) b) $
        logFatal [printf "invalid block hash '%s'" (fromJust b)]

      v <- withClientJson backend $ withBestBlockHash b getBlockInfo
      runPrinter $ printBlockInfo v

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
  blockSummary <- getFromJson =<< withBestBlockHash Nothing getBlockSummary
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
processBakerConfigureCmd baseCfgDir verbose backend txOpts cbCapital cbRestakeEarnings cbOpenForDelegation metadataURL cbTransactionFeeCommission cbBakingRewardCommission cbFinalizationRewardCommission inputKeysFile outputKeysFile = do
  let intOpts = toInteractionOpts txOpts
  (bakerKeys, txCfg, pl) <- transactionForBakerConfigure (ioConfirm intOpts)
  withClient backend $ do
    mapM_ (warnAboutBadCapital txCfg) cbCapital
    result <- sendAndTailTransaction txCfg pl intOpts
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
      AccountInfoResult{..} <- getAccountInfoOrDie senderAddr
      warnIfCapitalIsSmall capital
      cannotAfford <- warnIfCannotAfford txCfg capital airAmount
      mapM_ (warnIfCapitalIsLowered capital) airBaker
      unless cannotAfford (warnIfCapitalIsBig capital airAmount)

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

    warnIfCapitalIsLowered capital aibresult = do
      blockSummary <- getFromJson =<< withBestBlockHash Nothing getBlockSummary
      cooldownDate <- case blockSummary of
        Just cpr -> getBakerCooldown cpr
        Nothing -> do
          logError ["Could not reach the node to get the baker cooldown period."]
          exitTransactionCancelled
      if capital < abirStakedAmount aibresult
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
         ++ configureRestateLogMsg
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
        Just capital -> [printf "stake will be %s CCD" (Types.amountToString capital)]

    configureRestateLogMsg =
      case cbRestakeEarnings of
        Nothing -> []
        Just True -> ["rewards will be automatically added to the baking stake"]
        Just False -> ["rewards will _not_ be automatically added to the baking stake"]

    configureOpenForDelegationLogMsg =
      case cbOpenForDelegation of
        Nothing -> []
        Just Types.OpenForAll -> ["baker pool will be open for delegation"]
        Just Types.ClosedForNew -> ["baker pool will be closed for new delegators"]
        Just Types.ClosedForAll -> ["baker pool will be closed for delegators and existing delegators will be moved to the L-pool"]

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
                  challenge = Types.addBakerChallenge senderAddress bkwpElectionVerifyKey bkwpSignatureVerifyKey bkwpAggregationVerifyKey
              bkwpProofElection <- Proofs.proveDlog25519VRF challenge (VRF.KeyPair electionSignKey bkwpElectionVerifyKey) `except` "cannot produce VRF key proof"
              bkwpProofSig <- Proofs.proveDlog25519Block challenge (BlockSig.KeyPair signatureSignKey bkwpSignatureVerifyKey) `except` "cannot produce signature key proof"
              bkwpProofAggregation <- Bls.proveKnowledgeOfSK challenge aggrSignKey
              return (Just (keys, enc), Just Types.BakerKeysWithProofs{..})

    except c err = c >>= \case
      Just x -> return x
      Nothing -> logFatal [err]

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
    processBakerConfigureCmd baseCfgDir verbose backend txOpts (Just newStake) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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

    BakerAdd bakerKeysFile txOpts initialStake autoRestake openForDelegation metadataURL transactionFeeCommission bakingRewardCommission finalizationRewardCommission outputFile ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts (Just initialStake) (Just autoRestake) (Just openForDelegation) (Just metadataURL) (Just transactionFeeCommission) (Just bakingRewardCommission) (Just finalizationRewardCommission) (Just bakerKeysFile) outputFile

    BakerConfigure txOpts capital restake openForDelegation metadataURL transactionFeeCommission bakingRewardCommission finalizationRewardCommission inputOutputKeyFiles -> do
      let inputKeysFile = fmap fst inputOutputKeyFiles
          outputKeysFile = fmap snd inputOutputKeyFiles
      processBakerConfigureCmd baseCfgDir verbose backend txOpts capital restake openForDelegation metadataURL transactionFeeCommission bakingRewardCommission finalizationRewardCommission inputKeysFile outputKeysFile

    BakerSetKeys file txOpts outfile ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just file) outfile

    BakerRemove txOpts ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts (Just 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    BakerUpdateStake newStake txOpts ->
      processBakerUpdateStakeCmd baseCfgDir verbose backend txOpts newStake

    BakerUpdateRestakeEarnings restake txOpts ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts Nothing (Just restake) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    BakerUpdateMetadataURL url txOpts ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts Nothing Nothing Nothing (Just url) Nothing Nothing Nothing Nothing Nothing

    BakerUpdateOpenDelegationStatus status txOpts ->
      processBakerConfigureCmd baseCfgDir verbose backend txOpts Nothing Nothing (Just status) Nothing Nothing Nothing Nothing Nothing Nothing

-- |Process a 'delegator configure ...' command.
processDelegatorConfigureCmd :: Maybe FilePath -> Verbose -> Backend -> TransactionOpts (Maybe Types.Energy)
  -> Maybe Types.Amount -- ^New stake/capital.
  -> Maybe Bool -- ^Select whether to restake earnings.
  -> Maybe Types.DelegationTarget -- ^Delegation target: baker or L-Pool.
  -> IO ()
processDelegatorConfigureCmd baseCfgDir verbose backend txOpts cdCapital cdRestakeEarnings cdDelegationTarget = do
  let intOpts = toInteractionOpts txOpts
  (txCfg, pl) <- transactionForDelegatorConfigure (ioConfirm intOpts)
  withClient backend $ do
    mapM_ (warnAboutBadCapital txCfg) cdCapital
    result <- sendAndTailTransaction txCfg pl intOpts
    warnAboutFailedResult result
  where
    warnAboutBadCapital txCfg capital = do
      let senderAddr = naAddr . esdAddress . tcEncryptedSigningData $ txCfg
      AccountInfoResult{..} <- getAccountInfoOrDie senderAddr
      warnIfCannotAfford txCfg capital airAmount
      -- TODO: Add airDelegator field and warn if capital is lowered (see configure delegator).

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
         ++ configureRestateLogMsg
         ++ configureDelegationTargetLogMsg)

      when confirm $ do
        confirmed <- askConfirmation Nothing
        unless confirmed exitTransactionCancelled
      when verbose $ do
        runPrinter $ printSelectedKeyConfig $ tcEncryptedSigningData
        putStrLn ""
      return (txCfg, payload)

    configureCapitalLogMsg =
      case cdCapital of
        Nothing -> []
        Just capital -> [printf "stake will be %s CCD" (Types.amountToString capital)]

    configureRestateLogMsg =
      case cdRestakeEarnings of
        Nothing -> []
        Just True -> ["rewards will be automatically added to the baking stake"]
        Just False -> ["rewards will _not_ be automatically added to the baking stake"]

    configureDelegationTargetLogMsg =
      case cdDelegationTarget of
        Nothing -> []
        Just Types.DelegateToLPool -> ["stake will be delegated to the L-pool"]
        Just (Types.DelegateToBaker bid) -> [printf "stake will be delegated to baker %s" (show bid)]


-- |Process a 'delegator ...' command.
processDelegatorCmd :: DelegatorCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processDelegatorCmd action baseCfgDir verbose backend =
  case action of
    DelegatorConfigure txOpts capital restake target ->
      processDelegatorConfigureCmd baseCfgDir verbose backend txOpts capital restake target
    DelegatorRemove txOpts ->
      processDelegatorConfigureCmd baseCfgDir verbose backend txOpts (Just 0) Nothing Nothing
    DelegatorAdd txOpts capital restake target ->
      processDelegatorConfigureCmd baseCfgDir verbose backend txOpts (Just capital) (Just restake) (Just target)

processIdentityCmd :: IdentityCmd -> Backend -> IO ()
processIdentityCmd action backend =
  case action of
    IdentityShow c -> processIdentityShowCmd c backend

processIdentityShowCmd :: IdentityShowCmd -> Backend -> IO ()
processIdentityShowCmd action backend =
  case action of
    IdentityShowIPs block -> do
      v <- withClientJson backend $ withBestBlockHash block getIdentityProviders
      case v of
        Nothing -> putStrLn "No response received from the gRPC server."
        Just a -> runPrinter $ printIdentityProviders a
    IdentityShowARs block -> do
      v <- withClientJson backend $ withBestBlockHash block $ getAnonymityRevokers
      case v of
        Nothing -> putStrLn "No response received from the gRPC server."
        Just a -> runPrinter $ printAnonymityRevokers a

-- |Process a "legacy" command.
processLegacyCmd :: LegacyCmd -> Backend -> IO ()
processLegacyCmd action backend =
  case action of
    SendTransaction fname nid -> do
      source <- handleReadFile BSL.readFile fname
      t <- withClient backend $ processTransaction source nid
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (getBlockItemHash t)
    GetConsensusInfo -> withClient backend $ getConsensusStatus >>= printJSON
    GetBlockInfo every block -> withClient backend $ withBestBlockHash block getBlockInfo >>= if every then loop else printJSON
    GetBlockSummary block -> withClient backend $ withBestBlockHash block getBlockSummary >>= printJSON
    GetBlocksAtHeight height gen restr -> withClient backend $ getBlocksAtHeight height gen restr >>= printJSON
    GetAccountList block -> withClient backend $ withBestBlockHash block getAccountList >>= printJSON
    GetInstances block -> withClient backend $ withBestBlockHash block getInstances >>= printJSON
    GetTransactionStatus txhash -> withClient backend $ getTransactionStatus txhash >>= printJSON
    GetTransactionStatusInBlock txhash block -> withClient backend $ getTransactionStatusInBlock txhash block >>= printJSON
    GetAccountInfo account block ->
      withClient backend $ withBestBlockHash block (getAccountInfo account) >>= printJSON
    GetAccountNonFinalized account ->
      withClient backend $ getAccountNonFinalizedTransactions account >>= printJSON
    GetNextAccountNonce account ->
      withClient backend $ getNextAccountNonce account >>= printJSON
    GetInstanceInfo account block ->
      withClient backend $ withBestBlockHash block (getInstanceInfo account) >>= printJSON
    GetRewardStatus block -> withClient backend $ withBestBlockHash block getRewardStatus >>= printJSON
    GetBirkParameters block ->
      withClient backend $ withBestBlockHash block getBirkParameters >>= printJSON
    GetModuleList block -> withClient backend $ withBestBlockHash block getModuleList >>= printJSON
    GetNodeInfo -> withClient backend $ getNodeInfo >>= printNodeInfo
    GetPeerData bootstrapper -> withClient backend $ getPeerData bootstrapper >>= printPeerData
    StartBaker -> withClient backend $ startBaker >>= printSuccess
    StopBaker -> withClient backend $ stopBaker >>= printSuccess
    PeerConnect ip port -> withClient backend $ peerConnect ip port >>= printSuccess
    PeerDisconnect ip port -> withClient backend $ peerDisconnect ip port >>= printSuccess
    GetPeerUptime -> withClient backend $ getPeerUptime >>= (liftIO . print)
    BanNode nodeId nodeIp -> withClient backend $ banNode nodeId nodeIp >>= printSuccess
    UnbanNode nodeId nodeIp -> withClient backend $ unbanNode nodeId nodeIp >>= printSuccess
    JoinNetwork netId -> withClient backend $ joinNetwork netId >>= printSuccess
    LeaveNetwork netId -> withClient backend $ leaveNetwork netId >>= printSuccess
    GetAncestors amount blockHash -> withClient backend $ withBestBlockHash blockHash (getAncestors amount) >>= printJSON
    GetBranches -> withClient backend $ getBranches >>= printJSON
    GetBannedPeers -> withClient backend $ getBannedPeers >>= (liftIO . print)
    Shutdown -> withClient backend $ shutdown >>= printSuccess
    DumpStart -> withClient backend $ dumpStart >>= printSuccess
    DumpStop -> withClient backend $ dumpStop >>= printSuccess
    GetIdentityProviders block -> withClient backend $ withBestBlockHash block getIdentityProviders >>= printJSON
    GetAnonymityRevokers block -> withClient backend $ withBestBlockHash block getAnonymityRevokers >>= printJSON
    GetCryptographicParameters block -> withClient backend $ withBestBlockHash block getCryptographicParameters >>= printJSON
    GetChainParameters -> do
      bs <- withClient backend $ getFromJson =<< withBestBlockHash Nothing getBlockSummary
      Queries.bsWithUpdates bs $ const $ printJSONValues . toJSON . Types._currentParameters
  where
    printSuccess (Left x)  = liftIO . putStrLn $ x
    printSuccess (Right x) = liftIO $ if x then putStrLn "OK" else putStrLn "FAIL"

-- |Look up block infos all the way to genesis.
loop :: Either String Value -> ClientMonad IO ()
loop v =
  case v of
    Left err       -> liftIO $ putStrLn err
    Right (AE.Object m) ->
      case HM.lookup "blockParent" m of
        Just (String parent) -> do
          printJSON v
          case HM.lookup "blockSlot" m of
            Just (AE.Number x) | x > 0 ->
              getBlockInfo parent >>= loop
            _ -> return () -- Genesis block reached.
        _ -> error "Unexpected return value for block parent."
    _ -> error "Unexptected return value for getBlockInfo."

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

printPeerData :: MonadIO m => Either String PeerData -> m ()
printPeerData epd =
  case epd of
    Left err -> liftIO $ putStrLn err
    Right PeerData{..} -> liftIO $ do
      putStrLn $ "Total packets sent: " ++ show totalSent
      putStrLn $ "Total packets received: " ++ show totalReceived
      putStrLn $ "Peer version: " ++ Text.unpack version
      putStrLn "Peer stats:"
      forM_ (peerStats ^. CF.peerstats) $ \ps -> do
        putStrLn $ "  Peer: " ++ Text.unpack (ps ^. CF.nodeId)
        putStrLn $ "    Packets sent: " ++ show (ps ^. CF.packetsSent)
        putStrLn $ "    Packets received: " ++ show (ps ^. CF.packetsReceived)
        putStrLn $ "    Latency: " ++ show (ps ^. CF.latency)
        putStrLn ""

      putStrLn $ "Peer type: " ++ Text.unpack (peerList ^. CF.peerType)
      putStrLn "Peers:"
      forM_ (peerList ^. CF.peers) $ \pe -> do
        putStrLn $ "  Node id: " ++ Text.unpack (pe ^. CF.nodeId . CF.value)
        putStrLn $ "    Port: " ++ show (pe ^. CF.port . CF.value)
        putStrLn $ "    IP: " ++ Text.unpack (pe ^. CF.ip . CF.value)
        putStrLn $ "    Catchup Status: " ++ showCatchupStatus (pe ^. CF.catchupStatus)
        putStrLn ""
  where showCatchupStatus =
          \case PeerElement'UPTODATE -> "Up to date"
                PeerElement'PENDING -> "Pending"
                PeerElement'CATCHINGUP -> "Catching up"
                _ -> "Unknown" -- this should not happen in well-formed responses


getPeerData :: Bool -> ClientMonad IO (Either String PeerData)
getPeerData bootstrapper = do
  totalSent' <- getPeerTotalSent
  totalReceived' <- getPeerTotalReceived
  version' <- getPeerVersion
  peerStats' <- getPeerStats bootstrapper
  peerList' <- getPeerList bootstrapper
  return $ do
    totalSent <- totalSent'
    totalReceived <- totalReceived'
    version <- version'
    peerStats <- peerStats'
    peerList <- peerList'
    return PeerData{..}

printNodeInfo :: MonadIO m => Either String NodeInfoResponse -> m ()
printNodeInfo mni =
  case mni of
    Left err -> liftIO (putStrLn err)
    Right ni -> liftIO $ do
      putStrLn $ "Node ID: " ++ show (ni ^. CF.nodeId . CF.value)
      putStrLn $ "Current local time: " ++ show (ni ^. CF.currentLocaltime)
      putStrLn $ "Baker ID: " ++ maybe "not a baker" show (ni ^? CF.maybe'consensusBakerId . _Just . CF.value)
      putStrLn $ "Peer type: " ++ show (ni ^. CF.peerType)
      putStrLn $ "Baker running: " ++ show (ni ^. CF.consensusBakerRunning)
      putStrLn $ "Consensus running: " ++ show (ni ^. CF.consensusRunning)
      putStrLn $ "Consensus type: " ++ show (ni ^. CF.consensusType)
      putStrLn $ "Baker committee member: " ++ show (ni ^. CF.consensusBakerCommittee)
      putStrLn $ "Finalization committee member: " ++ show (ni ^. CF.consensusFinalizerCommittee)

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
  getPeerList False <&> \case
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
processTransaction_ transaction networkId _verbose = do
  let accountKeys = CT.keys transaction
  tx <- do
    let header = metadata transaction
        sender = thSenderAddress header
    nonce <-
      case thNonce header of
        Nothing -> getBestBlockHash >>= getAccountNonce sender
        Just nonce -> return nonce
    txPayload <- convertTransactionJsonPayload $ payload transaction
    return $ encodeAndSignTransaction
      txPayload
      sender
      (thEnergyAmount header)
      nonce
      (thExpiry header)
      accountKeys

  sendTransactionToBaker tx networkId >>= \case
    Left err -> fail $ show err
    Right False -> fail "Transaction not accepted by the baker."
    Right True -> return tx

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
                  Right False -> fail "Transaction not accepted by the baker."
                  Right True -> return tx
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
