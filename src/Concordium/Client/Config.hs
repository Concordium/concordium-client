{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Concordium.Client.Config where

import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.Crypto.SignatureScheme as S
import Concordium.Types as Types
import Concordium.ID.Types as IDTypes
import Concordium.Client.Commands
import Concordium.Client.Types.Transaction

import Control.Exception
import Control.Monad.Except
import Data.Char
import Data.List.Split
import qualified Data.HashMap.Strict as M
import Data.Text (Text, pack, unpack, strip)
import System.Directory
import System.IO.Error
import System.Exit (die)
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
  when (not cfgDirExists) $ printf "Warning: Config directory '%s' not found.\n" cfgDir

  let accCfgDir = accountConfigDir cfgDir
  accCfgDirExists <- doesDirectoryExist accCfgDir
  when (not accCfgDirExists) $ printf "Warning: Account config directory '%s' not found.\n" accCfgDir

  let mapFile = accountNameMapFile accCfgDir
  mapFileExists <- doesFileExist mapFile
  m <- if mapFileExists then
         loadAccountNameMap mapFile
       else do
         printf "Warning: Account name map file '%s' not found.\n" mapFile
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
      Left err -> die $ printf "Cannot parse account name map file '%s': %s." f err
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
                       Left err -> throwError $ printf "invalid address '%s': %s" v err
                       Right a -> return a
      return (name, addr)
    _ -> throwError $ printf "invalid mapping format '%s' (should be '<name> = <address>')" line
  where supportedChar c = isAlphaNum c || c == '-' || c == '_'

data AccountConfig = AccountConfig
                     { acName :: Maybe Text
                     , acAddr :: Types.AccountAddress
                     , acKeys :: KeyMap }
                   deriving (Show)

getAccountConfig :: Maybe Text -> BaseConfig -> Maybe FilePath -> Maybe KeyMap -> IO AccountConfig
getAccountConfig account cfg keysDir keyMap = do
  account' <- case account of
    Nothing -> do
      printf "Account reference not provided; using \"%s\".\n" defaultAccountName
      return defaultAccountName
    Just a -> return a
  (name, addr) <- getAccountAddress (bcAccountNameMap cfg) account'
  km <- case keyMap of
    Nothing -> do
      let accCfgDir = bcAccountCfgDir cfg
          dir = case keysDir of
                  Nothing -> joinPath [accCfgDir, show addr]
                  Just p -> p
      km <- try $ loadKeyMap dir
      case km of
        Left err | isDoesNotExistError err -> die $ printf "keys for account '%s' not found: directory '%s' doesn't exist" (show addr) dir
                 | otherwise -> throw err
        Right k -> return k
    Just km -> return km
  return AccountConfig
    { acName = name
    , acAddr = addr
    , acKeys = km }

resolveAccountAddress :: AccountNameMap -> Text -> Maybe (Maybe Text, Types.AccountAddress)
resolveAccountAddress m input =
  -- Try parsing input as account address
  case IDTypes.addressFromText input of
    Left _ -> do
      -- Assume input is a name. Look it up in map.
      a <- M.lookup input m
      Just (Just input, a)
    Right a -> Just (Nothing, a)

getAccountAddress :: AccountNameMap -> Text -> IO (Maybe Text, Types.AccountAddress)
getAccountAddress m input = do
  case resolveAccountAddress m input of
    Nothing -> die $ printf "The identifier '%s' is neither the address nor the name of an account." input
    Just a -> return a

loadKeyMap :: FilePath -> IO KeyMap
loadKeyMap keysDir = do
  keyFilenames <- listDirectory keysDir
  let rawKeys = rawKeysFromFiles keysDir keyFilenames
  rawKeyMap <- loadRawKeyMap rawKeys
  case keyMapFromRaw rawKeyMap of
    Left err -> die $ printf "cannot load keys: %s" err
    Right km -> return km

type KeyName = String
data KeyType = Sign | Verify deriving (Eq, Show)
type KeyContents = Text

type RawKeyId = (KeyName, KeyType)
type RawKeyMap = M.HashMap KeyName (Maybe KeyContents, Maybe KeyContents)

rawKeysFromFiles :: FilePath -> [FilePath] -> [(FilePath, RawKeyId)]
rawKeysFromFiles keysDir keyFilenames = foldr f [] keyFilenames
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
