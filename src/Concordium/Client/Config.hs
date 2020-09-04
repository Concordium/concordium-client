{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Concordium.Client.Config where

import Concordium.Types as Types
import Concordium.ID.Types (addressFromText, KeyIndex)
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Crypto.FFIDataTypes as IDTypes
import Concordium.Client.Cli
import Concordium.Client.Commands
import Concordium.Client.Types.Account

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Maybe
import Data.Either
import qualified Data.Aeson as AE

import Data.Char
import Data.List as L
import Data.List.Split
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text, pack, strip)
import System.Directory
import System.IO.Error
import System.FilePath
import Text.Printf
import Text.Read (readMaybe)

type BaseConfigDir = FilePath

-- |The default location of the config root directory.
getDefaultBaseConfigDir :: IO BaseConfigDir
getDefaultBaseConfigDir = getXdgDirectory XdgConfig "concordium"

-- ** Helper functions to construct paths to account keys storage.
accountConfigDir :: BaseConfigDir -> FilePath
accountConfigDir baseCfgDir = baseCfgDir </> "accounts"

-- |The default location of the data root directory.
getDefaultDataDir :: IO FilePath
getDefaultDataDir = getXdgDirectory XdgData "concordium"

accountNameMapFile :: FilePath -> FilePath
accountNameMapFile accountCfgDir = accountCfgDir </> "names.map"

-- |Get the name of the directory with keys of an account.
accountKeysDir :: FilePath -> Types.AccountAddress -> FilePath
accountKeysDir accCfgDir addr = accCfgDir </> show addr

-- |Get the name of the file which contains the threshold for the amount of
-- signatures needed to sign a transaction.
accountThresholdFile :: FilePath -> Types.AccountAddress -> FilePath
accountThresholdFile accCfgDir addr = accCfgDir </> show addr <.> "threshold"

accountKeyFileExt :: String
accountKeyFileExt = "json"

accountKeyFilePrefix :: String
accountKeyFilePrefix = "keypair"

-- |Return file path of the key with the given index in the provided key directory.
accountKeyFile :: FilePath -> KeyIndex -> FilePath
accountKeyFile keysDir idx = keysDir </> accountKeyFilePrefix ++ show idx <.> accountKeyFileExt

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

-- |Mapping from account name to their address.
type AccountNameMap = M.HashMap Text Types.AccountAddress

-- |Base configuration consists of the account name mapping and location of
-- account keys to be loaded on demand.
data BaseConfig = BaseConfig
                  { bcVerbose :: Bool
                  , bcAccountNameMap :: AccountNameMap
                  , bcAccountCfgDir :: FilePath }
                deriving (Show)

-- |Initialize an empty config structure and returns the corresponding base config.
initBaseConfig :: Maybe FilePath -> IO BaseConfig
initBaseConfig f = do
  baseCfgDir <- getBaseConfigDir f
  logInfo [printf "initializing configuration structure in directory '%s'" baseCfgDir]

  baseCfgDirExists <- doesDirectoryExist baseCfgDir
  if baseCfgDirExists then
    logInfo [printf "skipping '%s': directory already exists" baseCfgDir]
  else
    -- Only explicitly handle a permission error since that is likely the most common one.
    handleJust (guard . isPermissionError)
               (\_ -> logFatal [printf "cannot create directory, permission denied"])
               (createDirectoryIfMissing True baseCfgDir)

  let accCfgDir = accountConfigDir baseCfgDir
      mapFile = accountNameMapFile accCfgDir

  logInfo [printf "creating directory '%s'" accCfgDir]
  accCfgDirExists <- doesDirectoryExist accCfgDir
  if accCfgDirExists then
    logInfo [printf "skipping '%s': directory already exists" accCfgDir]
  else
    createDirectoryIfMissing False accCfgDir

  logInfo [printf "creating file '%s'" mapFile]
  mapFileExists <- doesFileExist mapFile
  anm <- if mapFileExists then do
           logInfo [printf "skipping '%s': file already exists" mapFile]
           loadAccountNameMap mapFile
         else do
           writeFile mapFile ""
           return M.empty

  logSuccess ["configuration initialized"]
  return BaseConfig
    { bcVerbose = False
    , bcAccountCfgDir = accCfgDir
    , bcAccountNameMap = anm }

-- |Ensure the basic account config is initialized.
ensureAccountConfigInitialized :: BaseConfig -> IO ()
ensureAccountConfigInitialized baseCfg = do
  let accCfgDir = bcAccountCfgDir baseCfg
  createDirectoryIfMissing True accCfgDir

-- |Add an account to the configuration by creating its key directory and
-- optionally a name mapping.
initAccountConfig :: BaseConfig
                  -> NamedAddress
                  -> IO (BaseConfig, AccountConfig)
initAccountConfig baseCfg namedAddr = do
  let NamedAddress { naAddr = addr, naName = name } = namedAddr
  case name of
    Nothing -> logInfo [printf "adding account %s without a name" (show addr)]
    Just n -> logInfo [printf "adding account %s with name '%s'" (show addr) n]

  -- Check if config has been initialized.
  let accCfgDir = bcAccountCfgDir baseCfg
      mapFile = accountNameMapFile accCfgDir
  ensureAccountConfigInitialized baseCfg

  -- Create keys directory.
  let keysDir = accountKeysDir accCfgDir addr
  keysDirExists <- doesDirectoryExist keysDir
  if keysDirExists
    then logFatal [printf "account is already initialized: directory '%s' exists" keysDir]
    else do
      logInfo [printf "creating directory '%s'" keysDir]
      catch @ IOError (createDirectoryIfMissing False keysDir) $
          \e -> logFatal [printf "cannot create account directory: %s" (show e)]
      logSuccess ["created key directory"]

  -- Add name mapping.
  baseCfg' <- case name of
    Nothing -> return baseCfg
    Just n -> do
      let m = M.insert n addr $ bcAccountNameMap baseCfg
      logInfo [printf "writing file '%s'" mapFile]
      writeAccountNameMap mapFile m
      logSuccess ["added name mapping"]
      return baseCfg { bcAccountNameMap = m }
  return (baseCfg', AccountConfig
                    { acAddr = namedAddr
                    , acKeys = M.empty
                    , acThreshold = 1 -- minimum threshold
                    , acEncryptionKey = error "Unimplemented."
                    })

-- |Write the provided configuration to disk in the expected formats.
importAccountConfig :: BaseConfig -> [AccountConfig] -> IO BaseConfig
importAccountConfig baseCfg accCfgs = foldM f baseCfg accCfgs
  where f bc ac = do
          (bc', _) <- initAccountConfig bc (acAddr ac)
          writeAccountKeys bc' ac
          return bc'

-- |Write the account name map to a file in the expected format.
writeAccountNameMap :: FilePath -> AccountNameMap -> IO ()
writeAccountNameMap file = writeFile file . unlines . map f . sortOn fst . M.toList
  where f (name, addr) = printf "%s = %s" name (show addr)

-- |Write the account keys structure into the directory of the given account.
-- Each 'EncryptedAccountKeyPair' is written to a JSON file the name of which is determined
-- by 'accountKeyFile'.
writeAccountKeys :: BaseConfig -> AccountConfig -> IO ()
writeAccountKeys baseCfg accCfg = do
  let accCfgDir = bcAccountCfgDir baseCfg
      keysDir = accountKeysDir accCfgDir $ acAddress accCfg
  keysDirExists <- doesDirectoryExist keysDir
  unless keysDirExists $ logFatal [ printf "account keys directory '%s' does not exist" keysDir
                                  , "did you run 'config account add ...' yet?" ]

  -- TODO Check for duplicates and print warning.
  -- TODO Reject overwrite: Require key to be removed first.
  -- TODO Don't write unchanged files.

  forM_ (M.toList $ acKeys accCfg) $ \(idx, kp) -> do
    let file = accountKeyFile keysDir idx
    logInfo [printf "writing file '%s'" file]
    -- NOTE: This writes the JSON in a compact way. If we want human-readable JSON, we should use pretty encoding.
    AE.encodeFile file kp

  -- Write the threshold as a JSON value. Since it is a simple numeric
  -- value this should look as expected.
  let thresholdFile = accountThresholdFile accCfgDir (acAddress accCfg)
  logInfo [printf "writing file '%s'" thresholdFile]
  AE.encodeFile thresholdFile (acThreshold accCfg)

  logSuccess ["wrote key and threshold files"]

getBaseConfig :: Maybe FilePath -> Verbose -> Bool -> IO BaseConfig
getBaseConfig f verbose autoInit = do
  cfgDir <- getBaseConfigDir f
  let accCfgDir = accountConfigDir cfgDir
      mapFile = accountNameMapFile accCfgDir

  cfgDirExists <- doesDirectoryExist cfgDir
  if not cfgDirExists && autoInit then
    initBaseConfig f
  else do
    m <- runExceptT $ do
      unless cfgDirExists $ throwE [ printf "config directory '%s' not found" cfgDir
                                   , "run 'config init' to remove this warning" ]

      accCfgDirExists <- liftIO $ doesDirectoryExist accCfgDir
      unless accCfgDirExists $ throwE [printf "account config directory '%s' not found" accCfgDir]

      mapFileExists <- liftIO $ doesFileExist mapFile
      unless mapFileExists $ throwE [printf "account name map file '%s' not found" mapFile]

      liftIO $ loadAccountNameMap mapFile

    let baseCfg = BaseConfig
                  { bcVerbose = verbose
                  , bcAccountCfgDir = accCfgDir
                  , bcAccountNameMap = M.empty }

    case m of
      Left warns -> logWarn warns >> return baseCfg
      Right m' -> return baseCfg { bcAccountNameMap = m' }

getBaseConfigDir :: Maybe FilePath -> IO FilePath
getBaseConfigDir = \case
  Nothing -> getDefaultBaseConfigDir
  Just p -> return p

loadAccountNameMap :: FilePath -> IO AccountNameMap
loadAccountNameMap f = do
  c <- readFile f
  case parseAccountNameMap $ lines c of
    Left err -> logFatal [printf "cannot parse account name map file '%s': %s" f err]
    Right m -> return m

parseAccountNameMap :: (MonadError String m) => [String] -> m AccountNameMap
parseAccountNameMap ls = M.fromList <$> mapM parseAccountNameMapEntry ls'
  where ls' = filter (not . L.all isSpace) ls

-- | Parse a line representing a single name-account mapping of the format "<name> = <address>".
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

-- | Check whether the given text is a valid account name.
isValidAccountName :: Text -> Bool
isValidAccountName n = not (T.null n) && not (isSpace $ T.head n) && not (isSpace $ T.last n) && T.all supportedChar n
  where supportedChar c = isAlphaNum c || c `elem` supportedSpecialChars
        supportedSpecialChars = "-_,.!? " :: String

-- | Check whether the given text is a valid account name and fail with an error message if it is not.
validateAccountName :: (MonadError String m) => Text -> m ()
validateAccountName name =
  unless (isValidAccountName name) $
  throwError $ printf "invalid account name '%s' (should not be empty or start/end with whitespace and consist of letters, numbers, space, '.', ',', '!', '?', '-', and '_' only)" name

data AccountConfig =
  AccountConfig
  { acAddr :: !NamedAddress
  , acKeys :: EncryptedAccountKeyMap
  , acThreshold :: !IDTypes.SignatureThreshold
  , acEncryptionKey :: !IDTypes.ElgamalSecretKey
  }

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
                 -> Bool
                 -- ^Whether to initialize the account config when it does not exist.
                 -- This will also ask the user for an optional name mapping.
                 -> IO (BaseConfig, AccountConfig)
getAccountConfig account baseCfg keysDir keyMap autoInit = do
  account' <- case account of
    Nothing -> do
      logInfo [printf "account reference not provided; using \"%s\"" defaultAccountName]
      return defaultAccountName
    Just a -> return a
  namedAddr <- case getAccountAddress (bcAccountNameMap baseCfg) account' of
                 Left err -> logFatal [err]
                 Right v -> return v
  case keyMap of
    Nothing -> do
      let accCfgDir = bcAccountCfgDir baseCfg
          addr = naAddr namedAddr
          dir = fromMaybe (accountKeysDir accCfgDir addr) keysDir
      dirExists <- doesDirectoryExist dir
      if not dirExists && autoInit then do
        printf "Account '%s' is not yet initialized.\n" (show addr)
        printf "Enter name of account or leave empty to initialize without a name (use ^C to cancel): "
        name <- strip <$> T.getLine >>= \case
                  "" -> return Nothing
                  s -> return $ Just s
        let namedAddr' = NamedAddress { naAddr = addr, naName = name }
        initAccountConfig baseCfg namedAddr'
      else do
        (km, acThreshold) <-
          handleJust
                (guard . isDoesNotExistError)
                (\_ -> logFatal [ printf "key directory for account '%s' not found" (show addr)
                                , "did you forget to add the account (using 'config account add')?"])
                (do km <- loadKeyMap dir
                    let file = accountThresholdFile accCfgDir addr
                    t <- loadThreshold file $ fromIntegral (M.size km)
                    return (km, t))

        return (baseCfg, AccountConfig
                         { acAddr = namedAddr
                         , acKeys = km
                         , acEncryptionKey = error "Unimplemented."
                         ,..})

    Just km -> return (baseCfg, AccountConfig
                                { acAddr = namedAddr
                                , acKeys = km
                                , acThreshold = fromIntegral (M.size km)
                                , acEncryptionKey = error "Unimplemented." })

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
                let name = fst <$> find ((== a) . snd) (M.toList m)
                return (name, a)
  return NamedAddress { naName = n, naAddr = a }

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
  forM fs' $ \f -> snd <$> getAccountConfig (Just $ pack f) cfg Nothing Nothing False
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
    kp <- AE.eitherDecodeFileStrict' file `withLogFatalIO` (("cannot load key file " ++ file ++ " ") ++)
    return (Just idx, kp)
  return $ foldl' insertAccountKey M.empty keys

-- |Insert key pair on the given index. If no index is given, use the first available one.
insertAccountKey :: EncryptedAccountKeyMap -> (Maybe KeyIndex, EncryptedAccountKeyPair) -> EncryptedAccountKeyMap
insertAccountKey km (idx,kp) =
  let idx' = case idx of
            Nothing -> nextUnusedIdx 1 (sort $ M.keys km)
            Just i -> i
  in M.insert idx' kp km

-- |Compute the next index not already in use, starting from the one provided
-- (which is assumed to be less than or equal to the first element of the list).
nextUnusedIdx :: KeyIndex -> [KeyIndex] -> KeyIndex
nextUnusedIdx i sortedKeys = case sortedKeys of
                               [] -> i
                               k:ks | k==i -> nextUnusedIdx (i+1) ks
                                    | otherwise -> i

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
