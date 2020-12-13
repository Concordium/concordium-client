{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Config where


import Concordium.Types as Types
import Concordium.ID.Types (addressFromText, KeyIndex)
import qualified Concordium.ID.Types as IDTypes
import Concordium.Client.Cli
import Concordium.Client.Commands
import Concordium.Client.Types.Account

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Except
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encode.Pretty as AE
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Either

import Data.Aeson ((.:),(.=))
import Concordium.Common.Version

import Data.Char
import Data.List as L
import Data.List.Split
import qualified Data.HashMap.Strict as M
import Data.String (IsString)
import Data.String.Interpolate (i, iii)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text, pack, strip, unpack)
import Data.Text.Encoding (decodeUtf8)
import System.Directory
import System.IO.Error
import System.FilePath
import Text.Printf
import Text.Read (readMaybe)

{- |
The layout of the config directory is shown in this example:

@
  <baseConfigDir>
  ├── accounts
  │   ├── names.map
  │   ├── <account1>
  │   │   ├── keypair0.json
  │   │   ├── keypair1.json
  │   │   ...
  │   │   └── encSecretKey.json
  │   ├── <account1>.threshold
  │   ├── <account2>
  │   │   ├── keypair0.json
  │   │   ├── keypair1.json
  │   │   ...
  │   │   └── encSecretKey.json
  │   └── <account2>.threshold
  └── contracts
      ├── contractNames.map
      └── moduleNames.map
@
-}
type BaseConfigDir = FilePath
type AccountConfigDir = FilePath
type ContractConfigDir = FilePath

-- JSON HELPERS

-- |Serialize to JSON and pretty-print.
showPrettyJSON :: AE.ToJSON a => a -> String
showPrettyJSON = unpack . decodeUtf8 . BSL.toStrict . AE.encodePretty

-- |Serialize to JSON, order by keys, and pretty-print.
showSortedPrettyJSON :: AE.ToJSON a => a -> String
showSortedPrettyJSON = unpack . decodeUtf8 . BSL.toStrict . AE.encodePretty' config
  where config = AE.defConfig { AE.confCompare = compare }

-- |Serialize to JSON, order by keys, and pretty-print without whitespace.
showCompactPrettyJSON :: AE.ToJSON a => a -> String
showCompactPrettyJSON = unpack . decodeUtf8 . BSL.toStrict . AE.encodePretty' config
  where config = AE.defConfig { AE.confIndent = AE.Spaces 0, AE.confCompare = compare }

-- |The default location of the config root directory.
getDefaultBaseConfigDir :: IO BaseConfigDir
getDefaultBaseConfigDir = getXdgDirectory XdgConfig "concordium"

-- ** Helper functions to construct paths to account keys storage.
accountConfigDir :: BaseConfigDir -> AccountConfigDir
accountConfigDir baseCfgDir = baseCfgDir </> "accounts"

-- |The default location of the data root directory.
getDefaultDataDir :: IO FilePath
getDefaultDataDir = getXdgDirectory XdgData "concordium"

-- |Get the path to the account names map file.
accountNameMapFile :: AccountConfigDir -> FilePath
accountNameMapFile accountCfgDir = accountCfgDir </> "names.map"

-- |Get the name of the directory with keys of an account.
accountKeysDir :: AccountConfigDir -> Types.AccountAddress -> FilePath
accountKeysDir accCfgDir addr = accCfgDir </> show addr

-- |Get the name of the file which contains the threshold for the amount of
-- signatures needed to sign a transaction.
accountThresholdFile :: AccountConfigDir -> Types.AccountAddress -> FilePath
accountThresholdFile accCfgDir addr = accCfgDir </> show addr <.> "threshold"

accountKeyFileExt :: String
accountKeyFileExt = "json"

accountKeyFilePrefix :: String
accountKeyFilePrefix = "keypair"

-- |Return file path of the key with the given index in the provided key directory.
accountKeyFile :: FilePath -> KeyIndex -> FilePath
accountKeyFile keysDir idx = keysDir </> accountKeyFilePrefix ++ show idx <.> accountKeyFileExt

-- |Return file path of the decryption key for encrypted amounts in the provided key directory.
accountEncryptionSecretKeyFile :: FilePath -> FilePath
accountEncryptionSecretKeyFile keysDir = keysDir </> "encSecretKey.json"

-- |For a filename (without directory but with extension) determine whether it is a valid name
-- of an account key file (as it would be produced by 'accountKeyFile').
parseAccountKeyFileName :: FilePath -> Maybe KeyIndex
parseAccountKeyFileName fileName =
  if takeExtension fileName == "." ++ accountKeyFileExt
  then readMaybe (drop (length accountKeyFilePrefix) $ takeBaseName fileName)
  else Nothing

-- |Name to use if no account name is provided.
defaultAccountName :: Text
defaultAccountName = "default"

-- |Get path to contracts config directory from the base config.
contractConfigDir :: BaseConfigDir -> ContractConfigDir
contractConfigDir = (</> "contracts")

-- |Get path to contractNames.map file.
contractNameMapFile :: ContractConfigDir -> FilePath
contractNameMapFile = (</> "contractNames.map")

-- |Get path to moduleNames.map file.
moduleNameMapFile :: ContractConfigDir -> FilePath
moduleNameMapFile = (</> "moduleNames.map")

-- |Mapping builder from a name to a provided type.
type NameMap = M.HashMap Text

-- |Mapping from account names to their addresses.
type AccountNameMap = NameMap Types.AccountAddress

-- |Mapping from contract names to their addresses.
type ContractNameMap = NameMap Types.ContractAddress

-- |Mapping from module names to their references.
type ModuleNameMap = NameMap Types.ModuleRef

-- |Base configuration consists of the account name mapping and location of
-- account keys to be loaded on demand.
data BaseConfig = BaseConfig
                  { bcVerbose :: Bool
                  , bcAccountNameMap :: AccountNameMap
                  , bcAccountCfgDir :: AccountConfigDir
                  , bcContractNameMap :: ContractNameMap
                  , bcModuleNameMap :: ModuleNameMap
                  , bcContractCfgDir :: ContractConfigDir}
                deriving (Show)

-- |Initialize an empty config structure and returns the corresponding base config.
initBaseConfig :: Maybe FilePath -> Verbose -> IO BaseConfig
initBaseConfig f verbose = do
  baseCfgDir <- getBaseConfigDir f
  when verbose $ logInfo [[i|initializing configuration structure in directory '#{baseCfgDir}'|]]

  baseCfgDirExists <- doesDirectoryExist baseCfgDir
  if baseCfgDirExists then
    when verbose $ logInfo [[i|skipping '#{baseCfgDir}': directory already exists|]]
  else
    -- Only explicitly handle a permission error since that is likely the most common one.
    handleJust (guard . isPermissionError)
               (\_ -> logFatal ["cannot create directory, permission denied"])
               (createDirectoryIfMissing True baseCfgDir)

  let accCfgDir = accountConfigDir baseCfgDir
      accMapFile = accountNameMapFile accCfgDir

      contrCfgDir = contractConfigDir baseCfgDir
      contrMapFile = contractNameMapFile contrCfgDir
      moduleMapFile = moduleNameMapFile contrCfgDir

  -- Create all map files
  mapM_ (uncurry $ handleWriteFile writeFile AllowOverwrite verbose)
    [(accMapFile, mempty), (contrMapFile, emptyMapContentJSON), (moduleMapFile, emptyMapContentJSON)]

  accNameMap <- loadAccountNameMap accMapFile
  contrNameMap <- loadNameMapFromJSON contrMapFile
  moduleNameMap <- loadNameMapFromJSON moduleMapFile

  logSuccess ["configuration initialized"]
  return BaseConfig
    { bcVerbose = False
    , bcAccountCfgDir = accCfgDir
    , bcAccountNameMap = accNameMap
    , bcContractCfgDir = contrCfgDir
    , bcContractNameMap = contrNameMap
    , bcModuleNameMap = moduleNameMap }

-- |Ensure the basic account config is initialized.
ensureAccountConfigInitialized :: BaseConfig -> IO ()
ensureAccountConfigInitialized baseCfg = do
  let accCfgDir = bcAccountCfgDir baseCfg
  ensureDirCreated False accCfgDir

-- |Ensure a directory is created and, if Verbose, logInfo actions.
ensureDirCreated :: Verbose -> FilePath -> IO ()
ensureDirCreated verbose dir = do
  when verbose $ logInfo [[i|creating directory '#{dir}'|]]
  dirExists <- doesDirectoryExist dir
  if dirExists then
    when verbose $ logInfo [[i|skipping '#{dir}': directory already exists|]]
  else
    createDirectoryIfMissing True dir

initAccountConfigEither :: BaseConfig
                        -> NamedAddress
                        -> Bool -- ^ True if we are in a cli environment
                        -> IO (Either String (BaseConfig, AccountConfig, Bool))
initAccountConfigEither baseCfg namedAddr inCLI = runExceptT $ do
  let NamedAddress { naAddr = addr, naName = name } = namedAddr
  case name of
    Nothing -> logInfo [printf "adding account %s without a name" (show addr)]
    Just n -> logInfo [printf "adding account %s with name '%s'" (show addr) n]

  -- Check if config has been initialized.
  let accCfgDir = bcAccountCfgDir baseCfg
      mapFile = accountNameMapFile accCfgDir
  liftIO $ ensureAccountConfigInitialized baseCfg

  -- Create keys directory.
  let keysDir = accountKeysDir accCfgDir addr
  keysDirExists <- liftIO $ doesDirectoryExist keysDir
  written <- if keysDirExists
    then
      if inCLI then do
        logWarn [printf "account is already initialized: directory '%s' exists." keysDir
                , "overwriting the directory would erase all the currently stored keys"
                , "This is a destructive operation and cannot be undone"
                , "Are you certain that you want to proceed with this operation?"]
        ovwt <- liftIO $ askConfirmation Nothing
        if ovwt
          then do
          liftIO $ removePathForcibly keysDir
          logInfo [printf "overwriting directory '%s'" keysDir]
          liftIO $ createDirectoryIfMissing False keysDir
          logSuccess ["overwrote key directory"]
          return True
          else do
          liftIO $ logInfo [printf "Account '%s' will not be imported" (show addr)]
          return False
      else do
        throwE $ printf "account is already initialized: directory '%s' exists, retry using the CLI for more options" keysDir
    else do
      logInfo [printf "creating directory '%s'" keysDir]
      liftIO $ createDirectoryIfMissing False keysDir
      logSuccess ["created key directory"]
      return True

  -- Add name mapping.
  baseCfg' <- if not written then return baseCfg else case name of
    Nothing -> return baseCfg
    Just n -> do
      let m = M.insert n addr $ bcAccountNameMap baseCfg
      liftIO $ writeNameMap True mapFile m
      logSuccess [[i|added name mapping: '#{n}' --> '#{addr}'|]]
      return baseCfg { bcAccountNameMap = m }

  return (baseCfg', AccountConfig
                    { acAddr = namedAddr
                    , acKeys = M.empty
                    , acThreshold = 1 -- minimum threshold
                    , acEncryptionKey = Nothing
                    }, written)

-- |Add an account to the configuration by creating its key directory and
-- optionally a name mapping.
initAccountConfig :: BaseConfig
                  -> NamedAddress
                  -> Bool -- ^ True if we are in a cli environment
                  -> IO (BaseConfig, AccountConfig, Bool)
initAccountConfig baseCfg namedAddr inCLI = do
  res <- initAccountConfigEither baseCfg namedAddr inCLI
  case res of
    Left err -> logFatal [err]
    Right config -> return config

-- |Remove a name from the account name map. If the name is not in use
-- |it does nothing
-- |Returns the potentially updated baseConfig
removeAccountNameAndWrite :: BaseConfig -> Text -> Verbose -> IO BaseConfig
removeAccountNameAndWrite baseCfg name verbose = do
  -- Check if config has been initialized.
  let accCfgDir = bcAccountCfgDir baseCfg
      mapFile = accountNameMapFile accCfgDir
  liftIO $ ensureAccountConfigInitialized baseCfg

  let m = M.delete name $ bcAccountNameMap baseCfg
  liftIO $ writeNameMap verbose mapFile m
  logSuccess ["removed name mapping"]
  return baseCfg { bcAccountNameMap = m }

-- |Add a name to the account name map. If the name is already in use it
-- |overwrites it
-- |Returns the updated baseConfig
-- |PRECONDITION: Account name is a valid account account name
addAccountNameAndWrite :: BaseConfig -> Text -> AccountAddress -> Verbose -> IO BaseConfig
addAccountNameAndWrite baseCfg name addr verbose = do
  -- Check if config has been initialized.
  let accCfgDir = bcAccountCfgDir baseCfg
      mapFile = accountNameMapFile accCfgDir
  liftIO $ ensureAccountConfigInitialized baseCfg

  let m = M.insert name addr $ bcAccountNameMap baseCfg
  liftIO $ writeNameMap verbose mapFile m
  logSuccess [[i|added name mapping: '#{name}' --> '#{addr}'|]]
  return baseCfg { bcAccountNameMap = m }

-- |Write the provided configuration to disk in the expected formats.
importAccountConfigEither :: BaseConfig -> [AccountConfig] -> Verbose -> IO (Either String BaseConfig)
importAccountConfigEither baseCfg accCfgs verbose = runExceptT $ foldM f baseCfg accCfgs
  where f bc ac = do
          (bc', _, t) <- ExceptT $ initAccountConfigEither bc (acAddr ac) False
          liftIO $ when t $ writeAccountKeys bc' ac verbose
          return bc'

-- |Write the provided configuration to disk in the expected formats.
importAccountConfig :: BaseConfig -> [AccountConfig] -> Verbose -> IO BaseConfig
importAccountConfig baseCfg accCfgs verbose = foldM f baseCfg accCfgs
  where f bc ac = do
          (bc', _, t) <- initAccountConfig bc (acAddr ac) True
          when t $ writeAccountKeys bc' ac verbose
          return bc'

removeAccountConfig :: BaseConfig -> NamedAddress -> IO BaseConfig
removeAccountConfig baseCfg@BaseConfig{..} NamedAddress{..} = do
  -- Remove the keys directory
  let keysDir = accountKeysDir bcAccountCfgDir naAddr
  liftIO $ removePathForcibly keysDir

  -- Remove the threshold file
  let thresholdFilePath = accountThresholdFile bcAccountCfgDir naAddr
  liftIO $ removePathForcibly thresholdFilePath

  -- If an alias was removed, write the new map
  when nameWasRemoved (liftIO $ writeNameMap True (accountNameMapFile bcAccountCfgDir) accountNameMap')

  return baseCfg{bcAccountNameMap = accountNameMap'}
  where
    (accountNameMap', nameWasRemoved) = case naName of
      Nothing ->
        (bcAccountNameMap, False)
      Just name ->
        -- Remove the alias, if it exists
        (M.delete name bcAccountNameMap, True)

-- |Add a contract name and write it to 'contractNames.map', or return a list of error messages.
-- Returns a non-empty list of error messages if the name could not be added.
-- This happens when:
--  - The name is already in use
--  - Or, the contract is already named
-- Can also log fatally, if 'contractNames.map' cannot be written to.
addContractNameAndWrite :: MonadIO m => Verbose -> BaseConfig -> Text -> ContractAddress -> m [String]
addContractNameAndWrite verbose baseCfg = addContractOrModuleNameAndWrite verbose mapFile nameMap showCompactPrettyJSON
  where mapFile = contractNameMapFile . bcContractCfgDir $ baseCfg
        nameMap = bcContractNameMap baseCfg

-- |Add a module name and write it to 'moduleNames.map'
-- Returns a non-empty list of error messages if the name could not be added.
-- This happens when:
--  - The name is already in use
--  - Or, the module is already named
-- Can also log fatally, if 'moduleNames.map' cannot be written to.
addModuleNameAndWrite :: MonadIO m => Verbose -> BaseConfig -> Text -> ModuleRef -> m [String]
addModuleNameAndWrite verbose baseCfg = addContractOrModuleNameAndWrite verbose mapFile nameMap show
  where mapFile = moduleNameMapFile . bcContractCfgDir $ baseCfg
        nameMap = bcModuleNameMap baseCfg

-- |Add a contract or module name and write it to the appropriate map file.
-- Returns a non-empty list of error messages if the name could not be added.
-- This happens when:
--  - The name is already in use
--  - Or, the module is already named
-- Can also log fatally, if the map file cannot be written to.
addContractOrModuleNameAndWrite :: (AE.ToJSON v, Eq v, MonadIO m) => Verbose -> FilePath -> NameMap v ->
                                   (v -> String) -> Text -> v -> m [String]
addContractOrModuleNameAndWrite verbose mapFile nameMap showVal name val = case validateContractOrModuleName name of
  Left err -> return [namingError, err]
  Right _ ->
    let updatedNameMap = case (M.member name nameMap, val `elem` M.elems nameMap) of
          (True, _) -> Left [namingError, [i|the name '#{name}' is already in use|]]
          (_, True) -> Left [namingError, [i|'#{showVal val}' is already named|]]
          _ -> Right $ M.insert name val nameMap
    in case updatedNameMap of
      Left err' -> return err'
      Right updatedNameMap' -> liftIO $ writeNameMapAsJSON verbose mapFile updatedNameMap' >> return mempty
  where namingError = "Name was not added:"

-- |A contract address along with an optional name.
data NamedContractAddress =
  NamedContractAddress { ncaAddr :: Types.ContractAddress -- ^ The contract address.
                       , ncaName :: Maybe Text            -- ^ The optional contract name.
                       }

instance Show NamedContractAddress where
  show NamedContractAddress{..} = case ncaName of
    Just ncaName' -> [i|#{ncaAddr'} (#{ncaName'})|]
    Nothing -> ncaAddr'
    where ncaAddr' = showCompactPrettyJSON ncaAddr


-- |A module reference along with an optional name.
data NamedModuleRef =
  NamedModuleRef { nmrRef  :: Types.ModuleRef -- ^ The module reference.
                 , nmrName :: Maybe Text      -- ^ The optional contract name.
                 }

instance Show NamedModuleRef where
  show NamedModuleRef {..} = case nmrName of
    Just nmrName' -> [i|'#{nmrRef}' (#{nmrName'})|]
    Nothing -> [i|'#{nmrRef}'|]

-- |Write the name map to a file in a pretty JSON format.
writeNameMapAsJSON :: AE.ToJSON v => Verbose -> FilePath -> NameMap v -> IO ()
writeNameMapAsJSON verbose file =  handledWriteFile file . showSortedPrettyJSON
  where handledWriteFile = handleWriteFile writeFile AllowOverwrite verbose

-- |Write the name map to a file in the expected format.
writeNameMap :: Show v => Verbose -> FilePath -> NameMap v -> IO ()
writeNameMap verbose file = handledWriteFile file . unlines . map f . sortOn fst . M.toList
  where f (name, val) = printf "%s = %s" name (show val)
        handledWriteFile = handleWriteFile writeFile AllowOverwrite verbose

-- Used in `handleWriteFile` to determine how already exisiting files should be handled.
data OverwriteSetting =
    PromptBeforeOverwrite -- ^ The user should be prompted to confirm before overwriting.
  | AllowOverwrite        -- ^ Simply overwrite the file.
  deriving Eq

-- |Write to a file with the provided function and handle IO errors with an appropriate logging of errors.
-- Also ensures that the parent folders are created if missing.
-- If `OverwriteSetting == PromptBeforeOverwrite` then the user is prompted to confirm.
handleWriteFile :: (FilePath -> s -> IO ()) -> OverwriteSetting -> Verbose -> FilePath -> s -> IO ()
handleWriteFile wrtFile overwriteSetting verbose file contents = do
  writeConfirmed <- case overwriteSetting of
    AllowOverwrite -> return True
    PromptBeforeOverwrite -> do
      fileExists <- doesFileExist file
      if fileExists
        then logWarn [[i|'#{file}' already exists.|]] >> askConfirmation (Just "overwrite it")
        else return True
  when writeConfirmed $ do
    catchIOError (do
      createDirectoryIfMissing True $ takeDirectory file
      when verbose $ logInfo [[i|writing file '#{file}'|]]
      wrtFile file contents) logFatalOnErrors
  where logFatalOnErrors e
          | isDoesNotExistError e = logFatal [[i|the file '#{file}' does not exist and cannot be created|]]
          | isPermissionError e   = logFatal [[i|you do not have permissions to write to the file '#{file}'|]]
          | otherwise             = logFatal [[i|something went wrong while writing to the file '#{file}'|]]

-- |Read a file with the provided function and handle IO errors with an appropriate logging of errors.
handleReadFile :: (FilePath -> IO s) -> FilePath -> IO s
handleReadFile rdFile file = catchIOError (rdFile file) logFatalOnErrors
  where logFatalOnErrors e
          | isDoesNotExistError e = logFatal [[i|the file '#{file}' does not exist and cannot be read|]]
          | isPermissionError e   = logFatal [[i|you do not have permissions to read the file '#{file}'|]]
          | otherwise             = logFatal [[i|something went wrong while reading the file '#{file}'|]]

-- |Write the account keys structure into the directory of the given account.
-- Each 'EncryptedAccountKeyPair' is written to a JSON file the name of which
-- is determined by 'accountKeyFile'.
writeAccountKeys :: BaseConfig -> AccountConfig -> Verbose -> IO ()
writeAccountKeys baseCfg accCfg verbose = do
  let accCfgDir = bcAccountCfgDir baseCfg
      keysDir = accountKeysDir accCfgDir $ acAddress accCfg
  forM_ (M.toList $ acKeys accCfg) $ \(idx, kp) -> do
    let file = accountKeyFile keysDir idx
    -- NOTE: This writes the JSON in a compact way. If we want human-readable JSON, we should use pretty encoding.
    handleWriteFile AE.encodeFile AllowOverwrite verbose file kp

  -- TODO: Avoid writing encryptionKey and threshold files when unaltered.

  case acEncryptionKey accCfg of
    Just k -> do
      let encKeyFile = accountEncryptionSecretKeyFile keysDir
      handleWriteFile AE.encodeFile AllowOverwrite verbose encKeyFile k
    Nothing -> logFatal [ printf "importing account without a secret encryption key provided" ]

  writeThresholdFile accCfgDir accCfg verbose
  logSuccess ["the keys were successfully written to disk"]

-- |Remove the account keys, i.e. keypair{idx}.json files, with the provided indices.
removeAccountKeys :: BaseConfig -> AccountConfig -> [KeyIndex] -> Verbose -> IO ()
removeAccountKeys baseCfg accCfg idxs verbose = do
  let accCfgDir = bcAccountCfgDir baseCfg
      keysDir = accountKeysDir accCfgDir $ acAddress accCfg
  forM_ idxs $ \idx -> do
    let file = accountKeyFile keysDir idx
    when verbose $ logInfo ["removing file '" ++ file ++ "'"]
    removeFile file

  writeThresholdFile accCfgDir accCfg verbose
  logSuccess ["the keys were successfully removed from disk"]

-- |Write the threshold as a JSON value to the file {accountName}.threshold.
writeThresholdFile :: BaseConfigDir -> AccountConfig -> Verbose -> IO ()
writeThresholdFile accCfgDir accCfg verbose = do
  -- Write the threshold as a JSON value. Since it is a simple numeric
  -- value this should look as expected.
  let thresholdFile = accountThresholdFile accCfgDir (acAddress accCfg)
  handleWriteFile AE.encodeFile AllowOverwrite verbose thresholdFile (acThreshold accCfg)

getBaseConfig :: Maybe FilePath -> Verbose -> IO BaseConfig
getBaseConfig f verbose = do
  cfgDir <- getBaseConfigDir f
  let accCfgDir = accountConfigDir cfgDir
      accMapFile = accountNameMapFile accCfgDir

      contrCfgDir = contractConfigDir cfgDir
      contrMapFile = contractNameMapFile contrCfgDir
      moduleMapFile = moduleNameMapFile contrCfgDir

  anm <- loadAccountNameMap accMapFile
  cnm <- loadNameMapFromJSON contrMapFile
  mnm <- loadNameMapFromJSON moduleMapFile

  return BaseConfig
      { bcVerbose = verbose
      , bcAccountCfgDir = accCfgDir
      , bcAccountNameMap = anm
      , bcContractCfgDir = contrCfgDir
      , bcContractNameMap = cnm
      , bcModuleNameMap = mnm }

getBaseConfigDir :: Maybe FilePath -> IO FilePath
getBaseConfigDir = \case
  Nothing -> getDefaultBaseConfigDir
  Just p -> return p

-- |Load a NameMap from a file in JSON format, or logFatal if file has invalid format.
loadNameMapFromJSON :: AE.FromJSON v => FilePath -> IO (NameMap v)
loadNameMapFromJSON mapFile = do
  -- Simply use an empty map if the reading fails
  content <- BSL.readFile mapFile `catch` (\(_ :: SomeException) -> return emptyMapContentJSON)
  case AE.eitherDecode content of
    Left err -> logFatal [[i|cannot parse name map file '#{mapFile}' as JSON: #{err}|]]
    Right nm -> return nm

-- |A string representing an empty hashmap in JSON format.
emptyMapContentJSON :: IsString s => s
emptyMapContentJSON = "{}"

-- |Load an AccountNameMap from a file in the format specified by `parseAccountNameMap`.
-- LogFatal if the file format is invalid.
loadAccountNameMap :: FilePath -> IO AccountNameMap
loadAccountNameMap mapFile = do
  -- Simply use an empty map if the reading fails
  content <- readFile mapFile `catch` (\(_ :: SomeException) -> return mempty)
  case parseAccountNameMap $ lines content of
    Left err -> logFatal [[i|cannot parse account name map file '#{content}': #{err}|]]
    Right m -> return m

-- |Parse an AccountNamepMap from zero or more entries, as specificied in `parseAccoutNameMapEntry`.
parseAccountNameMap :: (MonadError String m) => [String] -> m AccountNameMap
parseAccountNameMap ls = M.fromList <$> mapM parseAccountNameMapEntry ls'
  where ls' = filter (not . L.all isSpace) ls

-- |Parse a line representing a single name-account mapping of the format "<name> = <address>".
-- Leading and trailing whitespaces around name and address are ignored.
parseAccountNameMapEntry :: (MonadError String m) => String -> m (Text, Types.AccountAddress)
parseAccountNameMapEntry line =
  case splitOn "=" line of
    [k, v] -> do
      let name = strip $ pack k
      validateAccountName name
      addr <- case strip $ pack v of
                "" -> throwError "empty address"
                addr -> case addressFromText addr of
                          Left err -> throwError $ printf "invalid address '%s': %s" addr err
                          Right a -> return a
      return (name, addr)
    _ -> throwError $ printf "invalid mapping format '%s' (should be '<name> = <address>')" line

-- |Check whether the given text is a valid name.
isValidAccountName :: Text -> Bool
isValidAccountName n = not (T.null n) && not (isSpace $ T.head n) && not (isSpace $ T.last n) && T.all supportedChar n
  where supportedChar c = isAlphaNum c || c `elem` supportedSpecialChars
        supportedSpecialChars = "-_,.!? " :: String

-- |Check whether the given text is a valid contract or module name.
-- Is different from account names in that it must start with a letter.
isValidContractOrModuleName :: Text -> Bool
isValidContractOrModuleName n = not (T.null n) && not (isSpace $ T.head n) && isAlpha (T.head n) &&
                                not (isSpace $ T.last n) && T.all supportedChar n
  where supportedChar c = isAlphaNum c || c `elem` supportedSpecialChars
        supportedSpecialChars = "-_,.!? " :: String

-- |Prompt the user to input an account name validating the input
promptAccountName :: MonadIO m => String -> m Text
promptAccountName prompt = liftIO $ do
  putStr $ prettyMsg ": " prompt
  input <- T.getLine >>= ensureValidName
  return input
  where
    ensureValidName name =
      case validateAccountName name of
        Left err -> do
          logError [err]
          putStr "Input valid name: "
          T.getLine >>= ensureValidName
        Right () -> return name

-- |Check whether the given text is a valid name and fail with an error message if it is not.
validateAccountName :: (MonadError String m) => Text -> m ()
validateAccountName name =
  unless (isValidAccountName name) $
  throwError [iii|invalid name '#{name}' (should not be empty or start/end with whitespace
                  and consist of letters, numbers, space, '.', ',', '!',
                  '?', '-', and '_' only)|]

-- |Check whether the given text is a valid contract or module name and fail with an error message if it is not.
validateContractOrModuleName :: (MonadError String m) => Text -> m ()
validateContractOrModuleName name =
  unless (isValidContractOrModuleName name) $
  throwError [iii|invalid name '#{name}' (should start with a letter and not end with whitespace
                  and should otherwise consist of letters, numbers, space, '.', ',', '!',
                  '?', '-', and '_' only)|]

-- |Current version of accountconfig, for configbackup export/import compatability
accountConfigVersion :: Version
accountConfigVersion = 1

data AccountConfig =
  AccountConfig
  { acAddr :: !NamedAddress
  , acKeys :: EncryptedAccountKeyMap
  , acThreshold :: !IDTypes.SignatureThreshold
  -- | FIXME: This is a Maybe because the setup with account init being a
  -- special thing is such that we need to be able to initialize a dummy config.
  -- However this makes no practical sense, and that should be reworked so that
  -- you just import an account and don't initialize a dummy config first. But
  -- that is for the future, and for now we just have to deal with null pointers.
  , acEncryptionKey :: !(Maybe EncryptedAccountEncryptionSecretKey)
  } deriving(Show, Eq)

instance AE.ToJSON AccountConfig where
   toJSON AccountConfig{..} = 
     AE.object ["address" .= acAddr,
                "accountKeys" .= acKeys,
                "threshold" .= acThreshold,
                "accountEncryptionKey" .= acEncryptionKey]

instance AE.FromJSON AccountConfig where
  parseJSON = AE.withObject "AccountConfig" $ \v -> do
    acAddr <- v .: "address"
    acKeys <- v .: "accountKeys"
    acThreshold <- v .: "threshold"
    acEncryptionKey <- v .: "accountEncryptionKey"
    return AccountConfig{..}


-- | Whether to automatically initialize the account configuration or not.
data AutoInit = AutoInit | AssumeInitialized
    deriving(Eq, Show)

acAddress :: AccountConfig -> AccountAddress
acAddress = naAddr . acAddr

accountSigningDataFromConfig :: AccountConfig -> AccountKeyMap -> AccountSigningData
accountSigningDataFromConfig AccountConfig{..} keys =
  AccountSigningData { asdAddress = naAddr acAddr
                     , asdKeys = keys
                     , asdThreshold = acThreshold
                     }

getAccountConfig :: Maybe Text
                 -- ^Name or address of the account, defaulting to 'defaultAccountName' if not present.
                 -> BaseConfig
                 -- ^Configuration settings.
                 -> Maybe FilePath
                 -- ^Optional path to the key directory. Defaulting to directory derived from
                 -- 'BaseConfig' if absent.
                 -> Maybe EncryptedAccountKeyMap
                 -- ^Explicit keys. If provided they take precedence over other parameters,
                 -- otherwise the function attempts to lookup them up from the key directory.
                 -- If explicit keys are provided it is assumed that the
                 -- signature threshold for the account is less than the amount
                 -- of keys provided and all keys will be used to, e.g., sign the transaction.
                 -> Maybe EncryptedAccountEncryptionSecretKey
                 -- ^Explicit secret encryption key. If provided it takes precedence over other
                 -- parameters, otherwise the function attemps to lookup it ip from the key directory.
                 -> AutoInit
                 -- ^Whether to initialize the account config when it does not exist.
                 -- This will also ask the user for an optional name mapping.
                 -> IO (BaseConfig, AccountConfig)
getAccountConfig account baseCfg keysDir keyMap encKey autoInit = do
  account' <- case account of
    Nothing -> do
      logInfo [printf "account reference not provided; using \"%s\"" defaultAccountName]
      return defaultAccountName
    Just a -> return a
  namedAddr <- case getAccountAddress (bcAccountNameMap baseCfg) account' of
                 Left err -> logFatal [err]
                 Right v -> return v

  (bc, km, t) <-
    case keyMap of
      Nothing -> do
        let accCfgDir = bcAccountCfgDir baseCfg
            addr = naAddr namedAddr
            dir = fromMaybe (accountKeysDir accCfgDir addr) keysDir
        m <- autoinit baseCfg dir addr
        case m of
          Just b -> return (b, M.empty, 1)
          Nothing -> do
            (km, acThreshold) <-
              handleJust
              (guard . isDoesNotExistError)
              (\_ -> logFatal [ printf "key directory for account '%s' not found" (show addr)
                             , "did you forget to add the account (using 'config account add')?"])
              (do km <- loadKeyMap dir
                  let file = accountThresholdFile accCfgDir addr
                  t <- loadThreshold file $ fromIntegral (M.size km)
                  return (km, t))

            return (baseCfg, km, acThreshold)

      Just km -> return (baseCfg, km, fromIntegral (M.size km))

  (bc', enc) <-
    case encKey of
      Nothing -> do
        let accCfgDir = bcAccountCfgDir baseCfg
            addr = naAddr namedAddr
            dir = fromMaybe (accountKeysDir accCfgDir addr) keysDir
        m <- autoinit bc dir addr
        case m of
          Just b -> return (b, Nothing)
          Nothing -> (bc,) . Just <$> loadEncryptionSecretKey dir
      e@(Just _) -> return (bc, e)

  return (bc', AccountConfig {
    acAddr = namedAddr,
    acKeys = km,
    acThreshold = t,
    acEncryptionKey = enc
    })

  where
    autoinit :: BaseConfig -> FilePath -> AccountAddress -> IO (Maybe BaseConfig)
    autoinit cfg dir addr = do
      dirExists <- doesDirectoryExist dir
      if not dirExists && autoInit == AutoInit
        then do
          logInfo [[i|Account '#{addr}' is not yet initialized, creating key directory|]]
          let nameMap = bcAccountNameMap baseCfg
              nameOpt = find (\(_, addr') -> addr == addr') (M.toList nameMap)
          case nameOpt of 
            Just (name, _) -> do
              let namedAddr' = NamedAddress { naAddr = addr, naName = Just name }
              (a, _, _) <- initAccountConfig cfg namedAddr' False
              return . Just $ a
            Nothing -> do
              logInfo [[i|No name associated with account '#{addr}'.|]]
              printf "Enter name of account or leave empty to initialize without a name (use ^C to cancel): "
              name <- strip <$> T.getLine >>= \case
                "" -> return Nothing
                s -> do 
                  logFatalOnError $ validateAccountName s
                  return $ Just s
              let namedAddr' = NamedAddress { naAddr = addr, naName = name }
              (a, _, _) <- initAccountConfig cfg namedAddr' False
              return . Just $ a
        else do
          return Nothing

-- |Look up an account by name or address:
-- If input is a well-formed account address, try to (reverse) look up its name in the map.
-- If this lookup fails, return the address with name Nothing.
-- Otherwise assume the input is a local name of an account, and try to look up its address.
-- If this lookup fails, return Nothing.
resolveAccountAddress :: AccountNameMap -> Text -> Maybe NamedAddress
resolveAccountAddress m input = do
  -- Try parsing input as account address.
  (n, a) <- case addressFromText input of
              Left _ -> do
                -- Assume input is a name. Look it up in the map.
                a <- M.lookup input m
                return (Just input, a)
              Right a -> do
                -- Input is an address. Try to look up its name in the map.
                let name = lookupByValue m a
                return (name, a)
  return NamedAddress { naName = n, naAddr = a }

-- |Lookup by value from a map. Returns first entry found.
lookupByValue :: Eq v => NameMap v -> v -> Maybe Text
lookupByValue m input = fst <$> find ((== input) . snd) (M.toList m)

-- |Look up an account by name or address. See doc for 'resolveAccountAddress'.
-- If the lookup fails, an error is thrown.
getAccountAddress :: (MonadError String m) => AccountNameMap -> Text -> m NamedAddress
getAccountAddress m input =
  case resolveAccountAddress m input of
    Nothing -> throwError $ printf "the identifier '%s' is neither the address nor the name of an account" input
    Just a -> return a

-- |Return all 'AccountConfig's from the config.
getAllAccountConfigs :: BaseConfig -> IO [AccountConfig]
getAllAccountConfigs cfg = do
  let dir = bcAccountCfgDir cfg
  fs <- safeListDirectory dir
  fs' <- filterM (isDirectory dir) $ filter isValidAccountAddress fs
  forM fs' $ \f -> snd <$> getAccountConfig (Just $ pack f) cfg Nothing Nothing Nothing AssumeInitialized
  where
    isValidAccountAddress = isRight . addressFromText . pack
    isDirectory dir f =
      doesDirectoryExist $ joinPath [dir, f]

-- |Return all key pairs from the provided directory.
loadKeyMap :: FilePath -> IO EncryptedAccountKeyMap
loadKeyMap keysDir = do
  filesInKeyDir <- listDirectory keysDir
  let keyIdxs = mapMaybe parseAccountKeyFileName filesInKeyDir
  keys :: [(Maybe KeyIndex, EncryptedAccountKeyPair)] <- forM keyIdxs $ \idx -> do
    let file = accountKeyFile keysDir idx
    kp <- AE.eitherDecodeFileStrict' file `withLogFatalIO` (\e -> "cannot load key file " ++ file ++ " " ++ e)
    return (Just idx, kp)
  return $ foldl' insertAccountKey M.empty keys

loadEncryptionSecretKey :: FilePath -> IO EncryptedAccountEncryptionSecretKey
loadEncryptionSecretKey keysDir = do
  let file = accountEncryptionSecretKeyFile keysDir
  AE.eitherDecodeFileStrict' file `withLogFatalIO` (\e -> "cannot load encryption secret key file " ++ file ++ " " ++ e)

-- |Insert key pair on the given index. If no index is given, use the first available one.
insertAccountKey :: EncryptedAccountKeyMap -> (Maybe KeyIndex, EncryptedAccountKeyPair) -> EncryptedAccountKeyMap
insertAccountKey km (idx,kp) =
  let idx' = case idx of
            Nothing -> nextUnusedIdx 1 (sort $ M.keys km)
            Just idx'' -> idx''
  in M.insert idx' kp km

-- |Compute the next index not already in use, starting from the one provided
-- (which is assumed to be less than or equal to the first element of the list).
nextUnusedIdx :: KeyIndex -> [KeyIndex] -> KeyIndex
nextUnusedIdx idx sortedKeys = case sortedKeys of
                               [] -> idx
                               k:ks | k==idx -> nextUnusedIdx (idx+1) ks
                                    | otherwise -> idx

type KeyName = String
data KeyType = Sign | Verify deriving (Eq, Show)
type KeyContents = Text

-- |Load threshold from a file or return the provided default if the file does not exist.
loadThreshold :: FilePath -> IDTypes.SignatureThreshold -> IO IDTypes.SignatureThreshold
loadThreshold file defaultThreshold = do
  handleJust (guard . isDoesNotExistError) (const (return defaultThreshold)) $
    AE.eitherDecodeFileStrict' file >>= \case
      Left err -> logFatal [printf "corrupt threshold file '%s': %s" file err]
      Right t -> return t

-- |Return list of files in the directory or the empty list if it does not exist.
safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory dir = do
  e <- doesDirectoryExist dir
  if e then listDirectory dir else return []
