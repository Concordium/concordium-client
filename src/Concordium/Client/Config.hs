{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Concordium.Client.Config where

import qualified Concordium.Crypto.SignatureScheme as S
import Concordium.Types as Types
import Concordium.ID.Types (addressFromText, KeyIndex)
import qualified Concordium.ID.Types as IDTypes
import Concordium.Client.Cli
import Concordium.Client.Commands
import Concordium.Client.Parse
import Concordium.Client.Types.Transaction

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Maybe
import Data.Aeson
import Data.Char
import Data.List as L
import Data.List.Split
import qualified Data.HashMap.Strict as M
import Data.Text as T (Text, pack, strip, all)
import qualified Data.Text.IO as T
import System.Directory
import System.IO.Error
import System.FilePath
import Text.Printf

type BaseConfigDir = FilePath

getDefaultBaseConfigDir :: IO BaseConfigDir
getDefaultBaseConfigDir = getXdgDirectory XdgConfig "concordium"

-- ** Helper functions to construct paths to account keys storage.
accountConfigDir :: BaseConfigDir -> FilePath
accountConfigDir baseCfgDir = baseCfgDir </> "accounts"

accountNameMapFile :: FilePath -> FilePath
accountNameMapFile accountCfgDir = accountCfgDir </> "names.map"

-- |Get the name of the directory with keys of an account.
accountKeysDir :: FilePath -> Types.AccountAddress -> FilePath
accountKeysDir accCfgDir addr = accCfgDir </> show addr

-- |Get the name of the file which contains the threshold for the amount of
-- signatures needed to sign a transaction.
accountThresholdFile :: FilePath -> Types.AccountAddress -> FilePath
accountThresholdFile accCfgDir addr = accCfgDir </> show addr <.> "threshold"

data KeyPairFiles = KeyPairFiles { kpfSign :: FilePath , kpfVerify :: FilePath }

accountKeyFiles :: FilePath -> KeyIndex -> KeyPairFiles
accountKeyFiles keysDir idx = KeyPairFiles { kpfSign = addExt "sign" , kpfVerify = addExt "verify" }
  where addExt ext = keysDir </> show idx <.> ext

defaultAccountName :: Text
defaultAccountName = "default"

type AccountNameMap = M.HashMap Text Types.AccountAddress

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

  baseCfgDirExists <- doesPathExist baseCfgDir
  when baseCfgDirExists $ logFatal [printf "path already exists"]

  -- Create directories and map file.
  -- We only explicitly handle a permission error since that is likely the most
  -- common one.
  handleJust (guard . isPermissionError)
             (\_ -> logFatal [printf "cannot create directory, permission denied"])
             (createDirectoryIfMissing True baseCfgDir)

  let accCfgDir = accountConfigDir baseCfgDir
      mapFile = accountNameMapFile accCfgDir

  logInfo [printf "creating directory '%s'" accCfgDir]
  createDirectoryIfMissing False accCfgDir
  logInfo [printf "creating file '%s'" mapFile]
  writeFile mapFile ""
  logSuccess ["configuration initialized"]
  return BaseConfig
    { bcVerbose = False
    , bcAccountCfgDir = accCfgDir
    , bcAccountNameMap = M.empty }

-- |Ensure the basic account config is initialized
ensureAccountConfigInitialized :: BaseConfig -> IO ()
ensureAccountConfigInitialized baseCfg = do
  let accCfgDir = bcAccountCfgDir baseCfg
  ex <- doesDirectoryExist accCfgDir
  unless ex $ logFatal [ printf "account directory '%s' does not exist" accCfgDir
                       , "did you run 'config init' yet?" ]

-- |Add an account to the configuration by creating its key directory and
-- optionally a name mapping.
initAccountConfig :: BaseConfig
                  -> NamedAddress
                  -> Bool
                  -- ^Whether existence of the account in the config is fatal or not.
                  -> IO (BaseConfig, AccountConfig)
initAccountConfig baseCfg namedAddr existFatal = do
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
    then (if existFatal then logFatal else logWarn) [printf "account is already initialized: directory '%s' exists" keysDir]
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
                    })

importAccountConfig :: BaseConfig -> AccountConfig -> IO ()
importAccountConfig baseCfg accountCfg = do
  void (initAccountConfig baseCfg (acAddr accountCfg) True)
  writeAccountKeys baseCfg accountCfg

writeAccountNameMap :: FilePath -> AccountNameMap -> IO ()
writeAccountNameMap file = writeFile file . unlines . map f . sortOn fst . M.toList
  where f (name, addr) = printf "%s = %s" name (show addr)

-- |Write the account keys structure into the directory of the given account.
-- Each key pair is written into two separate files.
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
    let KeyPairFiles {..} = accountKeyFiles keysDir idx
        sk = S.signKey kp
        vk = S.verifyKey kp
    logInfo [printf "writing file '%s'" kpfSign]
    writeFile kpfSign $ show sk ++ "\n"
    logInfo [printf "writing file '%s'" kpfVerify]
    writeFile kpfVerify $ show vk ++ "\n"

  -- Write the threshold as a JSON value. Since it is a simple numeric
  -- value this should look as expected.
  let thresholdFile = accountThresholdFile accCfgDir (acAddress accCfg)
  logInfo [printf "writing file '%s'" thresholdFile]
  encodeFile thresholdFile (acThreshold accCfg)
  
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

parseAccountNameMapEntry :: (MonadError String m) => String -> m (Text, Types.AccountAddress)
parseAccountNameMapEntry line =
  case splitOn "=" line of
    [k, v] -> do
      name <- validateAccountName $ pack k
      addr <- case strip $ pack v of
                "" -> throwError "empty address"
                addr -> case addressFromText addr of
                          Left err -> throwError $ printf "invalid address '%s': %s" addr err
                          Right a -> return a
      return (name, addr)
    _ -> throwError $ printf "invalid mapping format '%s' (should be '<name> = <address>')" line

validateAccountName :: (MonadError String m) => Text -> m Text
validateAccountName name =
  case strip name of
    "" -> throwError "empty name"
    n | T.all supportedChar n -> return n
      | otherwise -> throwError $ printf "invalid name '%s' (should consist of letters, numbers, '-', and '_' only)" n
  where supportedChar c = isAlphaNum c || c == '-' || c == '_'

data AccountConfig = AccountConfig
                     { acAddr :: !NamedAddress
                     , acKeys :: !KeyMap
                     , acThreshold :: !IDTypes.SignatureThreshold}

acAddress :: AccountConfig -> AccountAddress
acAddress = naAddr . acAddr

getAccountConfig :: Maybe Text
                 -- ^Name of the account, defaulting to 'defaultAccountName' if not present.
                 -> BaseConfig
                 -- ^Configuration settings.
                 -> Maybe FilePath
                 -- ^Optional path to the key directory. Defaulting to directory derived from
                 -- 'BaseConfig' if absent.
                 -> Maybe KeyMap
                 -- ^Explicit keys. If provided they take precedence over other parameters,
                 -- otherwise the function attempts to lookup them up from the key directory.
                 -- If explicit keys are provided it is assume that the
                 -- signature threshold for the account is less than the amount
                 -- of keys provided and all keys will be used to, e.g., sign the transaction.
                 -> Bool
                 -- ^Whether to auto initialize the base config directory or not.
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
        initAccountConfig baseCfg namedAddr' True
      else do
        (km, acThreshold) <-
          handleJust
                (guard . isDoesNotExistError)
                (\_ -> logFatal [ printf "key directory for account '%s' not found" (show addr)
                                , "did you forget to add the account (using 'config account add')?"])
                (loadKeyMap dir)
        return (baseCfg, AccountConfig
                         { acAddr = namedAddr
                         , acKeys = km
                         ,..})

    Just km -> return (baseCfg, AccountConfig
                                { acAddr = namedAddr
                                , acKeys = km
                                , acThreshold = fromIntegral (M.size km) })

-- |Look up an account.
-- The second argument can be either an account address or a name of an account.
-- If it is a valid account address (in the sense that it is formatted correctly) then
-- it is assumed that the input is an address, and we try to look up whether we have it
-- stored in the config
-- Otherwise we assume the input is a local name of an account, and we try to look up its address.
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

getAccountAddress :: (MonadError String m) => AccountNameMap -> Text -> m NamedAddress
getAccountAddress m input =
  case resolveAccountAddress m input of
    Nothing -> throwError $ printf "the identifier '%s' is neither the address nor the name of an account" input
    Just a -> return a

getAllAccountConfigs :: BaseConfig -> IO [AccountConfig]
getAllAccountConfigs cfg = do
  let dir = bcAccountCfgDir cfg
  fs <- safeListDirectory dir
  fs' <- filterM (isDirectory dir) $ filter isValidAccountName fs
  forM fs' $ \f -> snd <$> getAccountConfig (Just $ pack f) cfg Nothing Nothing False
  where
    isValidAccountName a =
      case addressFromText $ pack a of
        Left _ -> False
        Right _ -> True
    isDirectory dir f =
      doesDirectoryExist $ joinPath [dir, f]

loadKeyMap :: FilePath -> IO (KeyMap, IDTypes.SignatureThreshold)
loadKeyMap keysDir = do 
  keyFilenames <- listDirectory keysDir
  let rawKeys = rawKeysFromFiles keysDir keyFilenames
  rawKeyMap <- loadRawKeyMap rawKeys
  threshold <- loadThreshold keysDir
  case keyMapFromRaw rawKeyMap of
    Left err -> logFatal [printf "cannot load keys: %s" err]
    Right km -> return (km, fromMaybe (fromIntegral (M.size km)) threshold)

insertAccountKey :: Maybe KeyIndex -> S.KeyPair -> KeyMap -> KeyMap
insertAccountKey idx kp km =
  let idx' = case idx of
            Nothing -> nextUnusedIdx 1 (sort $ M.keys km)
            Just i -> i
  in M.insert idx' kp km

-- Compute the next index not already in use, starting from the one provided
-- (which is assumed to be less than or equal to the first element of the list).
nextUnusedIdx :: KeyIndex -> [KeyIndex] -> KeyIndex
nextUnusedIdx i sortedKeys = case sortedKeys of
                               [] -> i
                               k:ks | k==i -> nextUnusedIdx (i+1) ks
                                    | otherwise -> i

type KeyName = String
data KeyType = Sign | Verify deriving (Eq, Show)
type KeyContents = Text

type RawKeyId = (KeyName, KeyType)
type RawKeyMap = M.HashMap KeyName (Maybe KeyContents, Maybe KeyContents)

rawKeysFromFiles :: FilePath -> [FilePath] -> [(FilePath, RawKeyId)]
rawKeysFromFiles keysDir = foldr f []
  where f file rks = case rawKeyIdFromFile file of
                       Nothing -> rks
                       Just k -> (joinPath [keysDir, file], k) : rks

rawKeyIdFromFile :: FilePath -> Maybe RawKeyId
rawKeyIdFromFile file = do
  let (name, ext) = splitExtension file
  t <- case ext of
         ".sign" -> Just Sign
         ".verify" -> Just Verify
         _ -> Nothing
  return (name, t)

keyMapFromRaw :: (MonadError String m) => RawKeyMap -> m KeyMap
keyMapFromRaw rkm = foldM f M.empty $ M.toList rkm
  where
    f m (name, keyPair) =
      case keyPair of
        (Just rsk, Just rvk) -> do
          -- Both sign and verify keys exist.
          n <- case reads name of
                 [(n, "")] -> return n
                 _ -> throwError $ printf "invalid key index '%s'" name
          sk <- parseSignKey rsk
          vk <- parseVerifyKey rvk
          let kp = S.KeyPairEd25519
                   { S.signKey = sk
                   , S.verifyKey = vk }
          return $ M.insert n kp m
        (Nothing, Just _) ->
          throwError $ printf "incomplete key pair '%s': sign key is missing" name
        (Just _, Nothing) ->
          throwError $ printf "incomplete key pair '%s': verify key is missing" name
        (Nothing, Nothing) ->
          -- Never happens the way keys are currently loaded.
          throwError $ printf "missing key pair '%s'" name

insertRawKey :: RawKeyId -> KeyContents -> RawKeyMap -> RawKeyMap
insertRawKey (name, t) contents m =
  let (sk, pk) = M.lookupDefault (Nothing, Nothing) name m
      keyPair = case t of
               Sign -> (Just contents, pk)
               Verify -> (sk, Just contents)
  in M.insert name keyPair m

loadRawKeyMap :: [(FilePath, RawKeyId)] -> IO RawKeyMap
loadRawKeyMap = foldM f M.empty
  where f rawKeyMap (file, rk) = do
          c <- readFile file
          return $ insertRawKey rk (strip $ pack c) rawKeyMap

loadThreshold :: FilePath -> IO (Maybe IDTypes.SignatureThreshold)
loadThreshold fp = 
  handleJust (guard . isDoesNotExistError) (const (return Nothing)) $ 
    eitherDecodeFileStrict' fp >>= \case
      Left err ->
        logFatal [printf "threshold file exists but is corrupt: %s" err]
      Right x -> return (Just x)

safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory dir = do
  e <- doesDirectoryExist dir
  if e then listDirectory dir else return []
