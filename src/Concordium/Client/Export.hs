{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Concordium.Client.Export where

import qualified Concordium.ID.Types as IDTypes

import Concordium.Client.Cli
import Concordium.Client.Config
import Concordium.Client.Encryption
import Concordium.Client.Types.Account
import Concordium.Client.Utils

import Control.Exception
import Control.Monad.Except
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import Data.Aeson ((.:),(.:?),(.!=),(.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.HashMap.Strict as Map
import Data.Maybe (maybeToList)
import Data.Text as T
import Data.String.Interpolate ( i )
import qualified Data.Text.IO as T
import Text.Printf
import Concordium.Common.Version

-- | An export format used by wallets including accounts and identities.
data WalletExport =
  WalletExport
  { wepAccounts :: ![WalletExportAccount] }
  deriving (Show)

instance AE.FromJSON WalletExport where
  parseJSON = AE.withObject "Wallet Export" $ \v -> do
    -- Validate type and version.
    t <- v .: "type"
    version <- v .: "v"
    unless (t == "concordium-mobile-wallet-data") $
      fail $ printf "unsupported type '%s'" (t :: String)
    unless (version == 1) $
      fail $ printf "unsupported version '%s'" (version :: Int)

    val <- v .: "value"
    ids <- val .: "identities"
    return WalletExport { wepAccounts = weiAccounts =<< ids }

-- | Used for parsing 'WalletExport'.
newtype WalletExportIdentity =
  WalletExportIdentity
  { weiAccounts :: [WalletExportAccount] }
  deriving (Show)

instance AE.FromJSON WalletExportIdentity where
  parseJSON = AE.withObject "Identity" $ \v -> do
    weiAccounts <- v .: "accounts"
    return WalletExportIdentity {..}

data WalletExportAccount =
  WalletExportAccount
  { weaName :: !Text
  , weaKeys :: !AccountSigningData
  , weaEncryptionKey :: !ElgamalSecretKey }
  deriving (Show)

instance AE.FromJSON WalletExportAccount where
  parseJSON = AE.withObject "Account" $ \v -> do
    name <- v .: "name"
    addr <- v .: "address"
    e <- v .: "encryptionSecretKey"
    (keys, th) <- v .: "accountData" >>= AE.withObject "Account data" (\w -> do
      keys <- w .: "keys"
      th <- w .: "threshold"
      return (keys, th))
    return WalletExportAccount
      { weaName = name
      , weaKeys = AccountSigningData
                  { asdAddress = addr
                  , asdKeys = keys
                  , asdThreshold = th }
      , weaEncryptionKey = e }

-- | Decode, decrypt and parse a mobile wallet export, reencrypting the singing keys with the same password.
decodeMobileFormattedAccountExport
  :: BS.ByteString -- ^ JSON with encrypted accounts and identities,
                   -- which must include the fields of an 'EncryptedJSON WalletExport'.
  -> Maybe Text -- ^ Only return the account with the given name (if it exists, otherwise return none).
  -> Password -- ^ Password to decrypt the export and to encrypt the sign keys.
  -> IO (Either String [AccountConfig]) -- ^ A list of resulting 'AccountConfig's or an error message on failure.
decodeMobileFormattedAccountExport json accountName password = runExceptT $ do
  we :: EncryptedJSON WalletExport <- AE.eitherDecodeStrict json `embedErr` printf "cannot decode wallet export JSON: %s"
  WalletExport{..} <- decryptJSON we password `embedErr` (("cannot decrypt wallet export: " ++) . displayException)
  accountCfgsFromWalletExportAccounts wepAccounts accountName password

-- | Convert one or all wallet export accounts to regular account configs.
-- This encrypts all signing keys with the provided password.
-- If name is provided, only the account with matching name (if any) is converted.
-- Otherwise they all are. Names to be imported are checked to be valid.
accountCfgsFromWalletExportAccounts :: [WalletExportAccount] -> Maybe Text -> Password -> ExceptT String IO [AccountConfig]
accountCfgsFromWalletExportAccounts weas name pwd = do
  selectedAccounts <-
        case name of
          Nothing -> return weas
          Just n -> case Prelude.filter ((==n) . weaName) weas of
              [] -> let possibleNames = showNameList . Prelude.map weaName $ weas
                    in throwError [i|An account named '#{n}' does not exist. Possible account names: #{possibleNames}.|]
              namesFound -> return namesFound
  forM selectedAccounts $ accountCfgFromWalletExportAccount pwd

-- |Convert a wallet export account to a regular account config.
-- This checks whether the name provided by the export is a valid account name.
-- This encrypts all signing keys with the provided password.
accountCfgFromWalletExportAccount :: Password -> WalletExportAccount -> ExceptT String IO AccountConfig
accountCfgFromWalletExportAccount pwd WalletExportAccount { weaKeys = AccountSigningData{..}, .. } = do
  name <- liftIO $ ensureValidName weaName
  acKeys <- liftIO $ encryptAccountKeyMap pwd asdKeys
  acEncryptionKey <- Just <$> (liftIO $ encryptAccountEncryptionSecretKey pwd weaEncryptionKey)
  return $ AccountConfig
    { acAddr = NamedAddress { naNames = [name], naAddr = asdAddress }
    , acThreshold = asdThreshold
    , ..
    }
  where
    ensureValidName name =
      let trimmedName = strip name
      in case validateAccountName trimmedName of
          Left err -> do
            logError [err]
            putStr "Input valid replacement name: "
            T.getLine >>= ensureValidName
          Right () -> return trimmedName

-- |Decode and parse a genesis account into a named account config.
-- All signing keys are encrypted with the given password.
decodeGenesisFormattedAccountExport
  :: BS.ByteString -- ^ JSON with the account information.
  -> Maybe Text -- ^ Optionally, how to name the account. It is checked whether the name is valid.
  -> Password -- ^ Password to encrypt the signing keys with.
  -> IO (Either String AccountConfig) -- ^ The resulting 'AccountConfig' or an error message on failure.
decodeGenesisFormattedAccountExport payload name pwd = runExceptT $ do
  mapM_ validateAccountName name
  val <- AE.eitherDecodeStrict payload `embedErr` printf "cannot decode wallet export JSON: %s"
  action <- AE.parseEither accountParser val `embedErr` printf "cannot parse JSON: %s"
  liftIO action
  where accountParser = AE.withObject "Account data" $ \v -> do
          addr <- v .: "address"
          aks <- v .: "accountKeys"
          accEncryptionKey <- v .: "encryptionSecretKey"
          accCredMap <- aks .: "keys"
          unless (Map.size accCredMap == 1) $ fail "Currently only accounts with a single credential are supported."
          case Map.lookup (0 :: IDTypes.CredentialIndex) accCredMap of
            Nothing -> fail "Currently only accounts with a single credential (0) are supported."
            Just ad -> do
              accKeyMap <- ad .: "keys"
              acThreshold <- ad .:? "threshold" .!= fromIntegral (Map.size accKeyMap)
              return $ do
                acKeys <- encryptAccountKeyMap pwd accKeyMap
                acEncryptionKey <- Just <$> (liftIO $ encryptAccountEncryptionSecretKey pwd accEncryptionKey)
                return AccountConfig { acAddr = NamedAddress{naNames = maybeToList name, naAddr = addr}
                                 , .. }


---- Code for instantiating, exporting and importing config backups

-- | An export format used for config backups.
data ConfigBackup =
  ConfigBackup
  { cbAccounts :: [AccountConfig]
  , cbContractNameMap :: ContractNameMap
  , cbModuleNameMap :: ModuleNameMap
  } deriving (Eq, Show)

instance AE.FromJSON ConfigBackup where
  parseJSON v = do
    vObj <- AE.parseJSON v
    unless (vVersion vObj == configBackupVersion) $
      fail [i|Unsupported account config version : #{vVersion vObj}|]
    let (cbAccounts, cbContractNameMap, cbModuleNameMap) = vValue vObj
    return ConfigBackup{..}

instance AE.ToJSON ConfigBackup where
   toJSON ConfigBackup{..} = AE.toJSON $ Versioned configBackupVersion (cbAccounts, cbContractNameMap, cbModuleNameMap)

-- | Currently supported version of the config backup.
-- Imports from older versions might be possible, but exports are going to use only this version.
configBackupVersion :: Version
configBackupVersion = 1

-- | Encode and encrypt the config Json, for writing to a file, optionally protected under a password.
-- The output artifact is an object with two fields, "type" and "contents", with "type" being either unencrypted or encrypted
-- and contents being the actual data, in either encrypted or unencrypted formats.
configExport  :: ConfigBackup -- ^ The contents to back up.
  -> Maybe Password -- ^ Password to decrypt the export.
  -> IO BS.ByteString -- ^ JSON with the backed up config.
configExport cb pwd = do
    case pwd of
      Just password -> do 
        contents <- encryptJSON AES256 PBKDF2SHA256 vcb password
        return . LazyBS.toStrict . AE.encode $ AE.object ["type" .= AE.String "encrypted", "contents" .= contents]
      Nothing -> do
        return . LazyBS.toStrict . AE.encode $ AE.object ["type" .= AE.String "unencrypted", "contents" .= vcb]
 where vcb = Versioned configBackupVersion cb

-- |Decrypt and decode an exported config, optionally protected under a password.
configImport
  :: BS.ByteString
  -> IO Password -- ^ Action to obtain the password if necessary.
  -> IO (Either String ConfigBackup)
configImport json pwdAction = runExceptT $ do
  vconfigBackup <- AE.eitherDecodeStrict json `embedErr` (\err -> [i|The input file is not valid JSON: #{err}|])
  case AE.parseEither importParser vconfigBackup of
    Left err -> throwError [i|Failed reading the input file: #{err}|]
    Right (Left encrypted) -> do
      pwd <- liftIO pwdAction
      (Versioned _ cb) <- decryptJSON encrypted pwd
        `embedErr` (("Failed to decrypt Config Backup using the supplied password: " ++) . displayException)
      return cb
    Right (Right (Versioned _ cb)) -> return cb
  where
    importParser = AE.withObject "ConfigBackup" $ \v -> do
      ty <- v .: "type"
      case ty of 
        "encrypted" -> do
          -- Parse as EncryptedJSON ConfigBackup
          cnt <- v .: "contents"
          return (Left cnt)
        "unencrypted" -> do
          -- Parse as ConfigBackup
          cnt <- v .: "contents"
          return (Right cnt)
        _ -> fail $ "Unsupported config backup type: " ++ ty
