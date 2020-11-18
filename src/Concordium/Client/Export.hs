{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Concordium.Client.Export where

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
accountCfgsFromWalletExportAccounts weas name pwd =
  let selectedAccounts =
        case name of
          Nothing -> weas
          Just n -> Prelude.filter ((==n) . weaName) weas
  in forM selectedAccounts $ accountCfgFromWalletExportAccount pwd

-- |Convert a wallet export account to a regular account config.
-- This checks whether the name provided by the export is a valid account name.
-- This encrypts all signing keys with the provided password.
accountCfgFromWalletExportAccount :: Password -> WalletExportAccount -> ExceptT String IO AccountConfig
accountCfgFromWalletExportAccount pwd WalletExportAccount { weaKeys = AccountSigningData{..}, .. } = do
  name <- liftIO $ ensureValidName weaName
  acKeys <- liftIO $ encryptAccountKeyMap pwd asdKeys
  acEncryptionKey <- Just <$> (liftIO $ encryptAccountEncryptionSecretKey pwd weaEncryptionKey)
  return $ AccountConfig
    { acAddr = NamedAddress { naName = Just name, naAddr = asdAddress }
    , acThreshold = asdThreshold
    , ..
    }
  where
    ensureValidName name =
      case validateAccountName name of
        Left err -> do
          logError [err]
          putStr "Input valid replacement name: "
          T.getLine >>= ensureValidName
        Right () -> return name

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
          ad <- v .: "accountData"
          accKeyMap <- ad .: "keys"
          accEncryptionKey <- v .: "encryptionSecretKey"
          acThreshold <- ad .:? "threshold" .!= fromIntegral (Map.size accKeyMap)
          return $ do
            acKeys <- encryptAccountKeyMap pwd accKeyMap
            acEncryptionKey <- Just <$> (liftIO $ encryptAccountEncryptionSecretKey pwd accEncryptionKey)
            return AccountConfig { acAddr = NamedAddress{naName = name, naAddr = addr}
                                 , .. }


---- Code for instantiating, exporting and importing config backups
-- | An export format used for config backups.
data ConfigBackup =
  ConfigBackup
  { cbuAccounts :: Versioned [AccountConfig] }

configBackupVersion :: Version
configBackupVersion = 1

instance AE.ToJSON ConfigBackup where
   toJSON (ConfigBackup cbuAccounts) = AE.object ["accounts" .= cbuAccounts]

instance AE.FromJSON ConfigBackup where
  parseJSON = AE.withObject "ConfigBackup" $ \v -> do
    cbuAccounts <- v .: "accounts"
    version <- cbuAccounts .: "v"
    unless (version == 1) $
      fail $ [i|Unsupported accountconfig encoding version, : #{version}|]
    accountConfigs <- cbuAccounts .: "value"
    return ConfigBackup{cbuAccounts = (Versioned version) accountConfigs}

-- | Encode and encrypt the config Json, for writing to a file, optionally protected under a password.
configExport  :: [AccountConfig] -- ^ A list of 'AccountConfig's
  -> Maybe Password -- ^ Password to decrypt the export.
  -> IO (BS.ByteString) -- ^ JSON with encrypted accounts and identities,
configExport accounts pwd = case pwd of
  Just password -> LazyBS.toStrict . AE.encode <$> (Versioned configBackupVersion) <$> (encryptJSON AES256 PBKDF2SHA256 ConfigBackup{cbuAccounts = (Versioned accountConfigVersion) accounts} password)
  Nothing -> return $ LazyBS.toStrict $ AE.encode $ (Versioned configBackupVersion) ConfigBackup{cbuAccounts = (Versioned accountConfigVersion) accounts} 


-- |Decrypt and decode an exported config, optionally protected under a password.
configImport
  :: BS.ByteString
  -> Maybe Password
  -> IO (Either String [AccountConfig])
configImport json pwd = runExceptT $ do
  case pwd of
    Just password -> do 
      vconfigbackup :: Versioned AE.Value <- AE.eitherDecodeStrict json `embedErr` (\err -> [i|Failed to decode version number of input file: #{err}|])
      case vVersion vconfigbackup of
        1 -> do
          cbu :: EncryptedJSON ConfigBackup <- resToEither (AE.fromJSON (vValue vconfigbackup)) `embedErr` decodeError 
          ConfigBackup{..} <- decryptJSON cbu password `embedErr` (("Failed to decrypt Config Backup using the supplied password: " ++) . displayException)
          return $ vValue cbuAccounts
        v -> throwError [i|Unsupported configbackup encoding version, : #{v}|]
    Nothing -> do
      vconfigbackup :: Versioned AE.Value <- AE.eitherDecodeStrict json `embedErr` (\err -> [i|Failed to decode version number of input file: #{err}|])
      case vVersion vconfigbackup of
        1 -> do
          ConfigBackup{..} <- resToEither (AE.fromJSON (vValue vconfigbackup)) `embedErr` decodeError
          return $ vValue cbuAccounts
        v -> throwError [i|Unsupported configbackup encoding version, : #{v}|]
  where
    resToEither :: AE.Result a -> Either String a
    resToEither res = case res of
      (AE.Success a) -> Right a
      (AE.Error err) -> Left err
    decodeError err = case err of 
              "key \"accounts\" not found" -> [i|Failed to decode input file to ConfigBackup JSON. File may be password encrypted: #{err}|]
              "key \"metadata\" not found" -> [i|Failed to decode input file to ConfigBackup JSON. File may not be password encrypted: #{err}|]
              _ -> [i|Failed to decode input file to ConfigBackup JSON: #{err}|]