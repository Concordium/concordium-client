{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Concordium.Client.Config where

import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.Crypto.SignatureScheme as S
import Concordium.Types as Types
import Concordium.ID.Types as IDTypes
import Concordium.Client.Cli
import Concordium.Client.Commands
import Concordium.Client.Types.Transaction

import Control.Exception
import Control.Monad.Except
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.HashMap.Strict as M
import Data.Text (Text, pack, unpack, strip)
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

defaultAccountName :: Text
defaultAccountName = "default"

type AccountNameMap = M.HashMap Text Types.AccountAddress

data BaseConfig = BaseConfig
                  { bcVerbose :: Bool
                  , bcAccountNameMap :: AccountNameMap
                  , bcAccountCfgDir :: FilePath }
                deriving (Show)

getBaseConfig :: Maybe FilePath -> Verbose -> IO BaseConfig
getBaseConfig f verbose = do
  cfgDir <- getBaseConfigDir f
  cfgDirExists <- doesDirectoryExist cfgDir
  unless cfgDirExists $ logWarn [printf "config directory '%s' not found" cfgDir]

  let accCfgDir = accountConfigDir cfgDir
  accCfgDirExists <- doesDirectoryExist accCfgDir
  unless accCfgDirExists $ logWarn [printf "account config directory '%s' not found" accCfgDir]

  let mapFile = accountNameMapFile accCfgDir
  mapFileExists <- doesFileExist mapFile
  m <- if mapFileExists then
         loadAccountNameMap mapFile
       else do
         logWarn [printf "account name map file '%s' not found" mapFile]
         return M.empty
  return BaseConfig
    { bcVerbose = verbose
    , bcAccountCfgDir = accCfgDir
    , bcAccountNameMap = m }

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
  where ls' = filter (not . all isSpace) ls

parseAccountNameMapEntry :: (MonadError String m) => String -> m (Text, Types.AccountAddress)
parseAccountNameMapEntry line =
  case splitOn "=" line of
    [k, v] -> do
      name <- case strip $ pack k of
                "" -> throwError "empty name"
                n | all supportedChar $ unpack n -> return n
                  | otherwise -> throwError $ printf "invalid name '%s' (should consist of letters, numbers, '-', and '_' only)" n
      addr <- case strip $ pack v of
                "" -> throwError "empty address"
                addr -> case IDTypes.addressFromText addr of
                       Left err -> throwError $ printf "invalid address '%s': %s" addr err
                       Right a -> return a
      return (name, addr)
    _ -> throwError $ printf "invalid mapping format '%s' (should be '<name> = <address>')" line
  where supportedChar c = isAlphaNum c || c == '-' || c == '_'

data AccountConfig = AccountConfig
                     { acAddr :: NamedAddress
                     , acKeys :: KeyMap }

getAccountConfig :: Maybe Text -> BaseConfig -> Maybe FilePath -> Maybe KeyMap -> IO AccountConfig
getAccountConfig account cfg keysDir keyMap = do
  account' <- case account of
    Nothing -> do
      logInfo [printf "account reference not provided; using \"%s\"" defaultAccountName]
      return defaultAccountName
    Just a -> return a
  namedAddr <- case getAccountAddress (bcAccountNameMap cfg) account' of
                 Left err -> logFatal [err]
                 Right v -> return v
  km <- case keyMap of
    Nothing -> do
      let accCfgDir = bcAccountCfgDir cfg
          addr = naAddr namedAddr
          dir = case keysDir of
                  Nothing -> joinPath [accCfgDir, show addr]
                  Just p -> p
      handleJust
        (guard . isDoesNotExistError)
        (\_ -> logFatal [printf "keys for account '%s' not found: directory '%s' doesn't exist" (show addr) dir])
        (loadKeyMap dir)
    Just km -> return km
  return AccountConfig
    { acAddr = namedAddr
    , acKeys = km }

resolveAccountAddress :: AccountNameMap -> Text -> Maybe NamedAddress
resolveAccountAddress m input = do
  -- Try parsing input as account address
  (n, a) <- case IDTypes.addressFromText input of
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
  forM fs' $ \f -> getAccountConfig (Just $ pack f) cfg Nothing Nothing
  where
    isValidAccountName a =
      case IDTypes.addressFromText $ pack a of
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
          sk <- case BSH.deserializeBase16 rsk of
            Nothing -> throwError $ printf "invalid sign key '%s' (should be base-16 string of length 64)" rsk
            Just v -> return v
          vk <- case BSH.deserializeBase16 rvk of
            Nothing -> throwError $ printf "invalid verify key '%s' (should be base-16 string of length 64)" rvk
            Just v -> return v
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
