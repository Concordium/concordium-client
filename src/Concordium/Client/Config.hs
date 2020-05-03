{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Concordium.Client.Config where

import qualified Concordium.Crypto.SignatureScheme as S
import Concordium.Types as Types
import Concordium.ID.Types (addressFromText, KeyIndex)
import Concordium.Client.Cli
import Concordium.Client.Commands
import Concordium.Client.Parse
import Concordium.Client.Types.Transaction

import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Except
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

getDefaultBaseConfigDir :: IO FilePath
getDefaultBaseConfigDir = getXdgDirectory XdgConfig "concordium"

accountConfigDir :: FilePath -> FilePath
accountConfigDir baseCfgDir = joinPath [baseCfgDir, "accounts"]

accountNameMapFile :: FilePath -> FilePath
accountNameMapFile accountCfgDir = joinPath [accountCfgDir, "names.map"]

accountKeysDir :: FilePath -> Types.AccountAddress -> FilePath
accountKeysDir accCfgDir addr = joinPath [accCfgDir, show addr]

data KeyPairFiles = KeyPairFiles { kpfSign :: FilePath , kpfVerify :: FilePath }

accountKeyFiles :: FilePath -> KeyIndex -> KeyPairFiles
accountKeyFiles keysDir idx = KeyPairFiles { kpfSign = p "sign" , kpfVerify = p "verify" }
  where p ext = joinPath [keysDir, printf "%s.%s" (show idx) (ext :: String)]

defaultAccountName :: Text
defaultAccountName = "default"

type AccountNameMap = M.HashMap Text Types.AccountAddress

data BaseConfig = BaseConfig
                  { bcVerbose :: Bool
                  , bcAccountNameMap :: AccountNameMap
                  , bcAccountCfgDir :: FilePath }
                deriving (Show)

-- Initialize an empty config structure and returns the corresponding base config.
initBaseConfig :: Maybe FilePath -> IO BaseConfig
initBaseConfig f = do
  baseCfgDir <- getBaseConfigDir f
  logInfo [printf "initializing configuration structure in directory '%s'" baseCfgDir]

  baseCfgDirExists <- doesDirectoryExist baseCfgDir
  when baseCfgDirExists $ logFatal [printf "directory already exists"]

  -- Create directories and map file.
  createDirectoryIfMissing True baseCfgDir
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

-- Add an account to the configuration by creating its key directory and
-- optionally a name mapping.
initAccountConfig :: BaseConfig -> NamedAddress -> IO (BaseConfig, AccountConfig)
initAccountConfig baseCfg namedAddr = do
  let NamedAddress { naAddr = addr, naName = name } = namedAddr
  case name of
    Nothing -> logInfo [printf "adding account %s without a name" (show addr)]
    Just n -> logInfo [printf "adding account %s with name '%s'" (show addr) n]

  -- Check if config has been initialized.
  let accCfgDir = bcAccountCfgDir baseCfg
      mapFile = accountNameMapFile accCfgDir
  accCfgDirExists <- doesDirectoryExist accCfgDir
  unless accCfgDirExists $ logFatal [ printf "account directory '%s' doesn't exist" accCfgDir
                                    , "did you run 'config init' yet?" ]
  -- Create keys directory.
  let keysDir = accountKeysDir accCfgDir addr
  keysDirExists <- doesDirectoryExist keysDir
  if keysDirExists
    then logWarn [printf "account is already initialized: directory '%s' exists" keysDir]
    else do
      logInfo [printf "creating directory '%s'" keysDir]
      createDirectoryIfMissing False keysDir
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
                    , acKeys = M.empty })

writeAccountNameMap :: FilePath -> AccountNameMap -> IO ()
writeAccountNameMap file = writeFile file . unlines . map f . sortOn fst . M.toList
  where f (name, addr) = printf "%s = %s" name (show addr)

writeAccountKeys :: BaseConfig -> AccountConfig -> IO ()
writeAccountKeys baseCfg accCfg = do
  let accCfgDir = bcAccountCfgDir baseCfg
      keysDir = accountKeysDir accCfgDir $ naAddr $ acAddr accCfg
  keysDirExists <- doesDirectoryExist keysDir
  unless keysDirExists $ logFatal [ printf "account keys directory '%s' doesn't exist" keysDir
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
  logSuccess ["updated key files"]

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
                     { acAddr :: NamedAddress
                     , acKeys :: KeyMap }

getAccountConfig :: Maybe Text -> BaseConfig -> Maybe FilePath -> Maybe KeyMap -> Bool -> IO (BaseConfig, AccountConfig)
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
          dir = case keysDir of
                  Nothing -> accountKeysDir accCfgDir addr
                  Just p -> p
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
        km <- handleJust
                (guard . isDoesNotExistError)
                (\_ -> logFatal [ printf "key directory for account '%s' not found" (show addr)
                                , "did you forget to add the account (using 'config account add')?"])
                (loadKeyMap dir)
        return (baseCfg, AccountConfig
                         { acAddr = namedAddr
                         , acKeys = km })

    Just km -> return (baseCfg, AccountConfig
                                { acAddr = namedAddr
                                , acKeys = km })

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

loadKeyMap :: FilePath -> IO KeyMap
loadKeyMap keysDir = do
  keyFilenames <- listDirectory keysDir
  let rawKeys = rawKeysFromFiles keysDir keyFilenames
  rawKeyMap <- loadRawKeyMap rawKeys
  case keyMapFromRaw rawKeyMap of
    Left err -> logFatal [printf "cannot load keys: %s" err]
    Right km -> return km

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

safeListDirectory :: FilePath -> IO [FilePath]
safeListDirectory dir = do
  e <- doesDirectoryExist dir
  if e then listDirectory dir else return []
