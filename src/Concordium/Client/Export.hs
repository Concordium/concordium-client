{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.Client.Export where

import Concordium.Client.Cli
import Concordium.Client.Config
import Concordium.Client.Types.Account
import Concordium.Client.Utils
import Concordium.Utils.Encryption

import Concordium.Common.Version
import Concordium.Crypto.SignatureScheme (KeyPair)
import qualified Concordium.ID.Types as IDTypes

import Control.Exception
import Control.Monad.Except
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Map.Strict as OrdMap
import Data.Maybe (maybeToList)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf

-- | Format of keys in genesis per credential.
data GenesisCredentialKeys = GenesisCredentialKeys
    { gckKeys :: !(OrdMap.Map IDTypes.KeyIndex KeyPair),
      gckThreshold :: !IDTypes.SignatureThreshold
    }

-- | Format of keys in a genesis account.
data GenesisAccountKeys = GenesisAccountKeys
    { gakKeys :: OrdMap.Map IDTypes.CredentialIndex GenesisCredentialKeys,
      gakThreshold :: !IDTypes.AccountThreshold
    }

-- | Credentials for genesis accounts.
newtype GenesisCredentials = GenesisCredentials {gcCredentials :: OrdMap.Map IDTypes.CredentialIndex IDTypes.AccountCredential}
    deriving newtype (AE.FromJSON, AE.ToJSON)

instance AE.FromJSON GenesisCredentialKeys where
    parseJSON = AE.withObject "Genesis Credential Keys" $ \obj -> do
        gckKeys <- obj AE..: "keys"
        gckThreshold <- obj AE..: "threshold"
        return GenesisCredentialKeys{..}

instance AE.FromJSON GenesisAccountKeys where
    parseJSON = AE.withObject "Genesis Account Keys" $ \obj -> do
        gakKeys <- obj AE..: "keys"
        gakThreshold <- obj AE..: "threshold"
        return GenesisAccountKeys{..}

-- | Get the list of keys suitable for signing. This will respect the thresholds
--  so that the lists are no longer than the threshold that is specified.
toKeysList :: GenesisAccountKeys -> [(IDTypes.CredentialIndex, [(IDTypes.KeyIndex, KeyPair)])]
toKeysList GenesisAccountKeys{..} = take (fromIntegral gakThreshold) . fmap toKeysListCred . OrdMap.toAscList $ gakKeys
  where
    toKeysListCred (ci, GenesisCredentialKeys{..}) = (ci, take (fromIntegral gckThreshold) . OrdMap.toAscList $ gckKeys)

-- | Environment for the export, e.g., staging, testnet, mainnet or something else.
type Environment = Text

-- | An export format used by wallets including accounts and identities.
data WalletExport = WalletExport
    { wepAccounts :: ![WalletExportAccount],
      -- | Environment for the accounts in the export, e.g., staging, testnet, mainnet, or something else.
      wepEnvironment :: !Text
    }
    deriving (Show)

-- | Parse export from the wallet. The data that is exported depends a bit on
--  which wallet it is, so the parser requires some context which is why this is
--  a separate function, and not a @FromJSON@ instance.
parseWalletExport ::
    -- | The name of the account to import. The old mobile wallet export does not
    --  require this, but for the new mobile wallet export it is required, and parsing
    --  will fail if this is not provided.
    Maybe Text ->
    -- | The JSON value to be parsed.
    AE.Value ->
    AE.Parser WalletExport
parseWalletExport mName = AE.withObject "Wallet Export" $ \v -> do
    -- Validate type and version.
    t <- v .: "type"
    case t of
        "concordium-mobile-wallet-data" -> do
            version <- v .: "v"
            unless (version == 1) $
                fail $
                    printf "unsupported mobile wallet export version '%s'" (version :: Int)
            val <- v .: "value"
            ids <- val .: "identities"
            -- We are trying to be explicit about the reason for failure here since the older format
            -- did not have the environment field, and we want the error message to be better than "missing field".
            wepEnvironment <-
                v .:? "environment" >>= \case
                    Nothing -> fail "Missing 'environment' field. Most likely this means the export file was produced by an older, unsupported, wallet version."
                    Just env -> return env
            return WalletExport{wepAccounts = weiAccounts =<< ids, ..}
        "concordium-browser-wallet-account" -> do
            weaName <- case mName of
                Nothing -> fail "To import an account from the browser wallet you have to provide a name using the `--name` option."
                Just n -> return n
            version <- v .: "v"
            unless (version == 0) $
                fail $
                    printf "unsupported browser extension export version '%s'" (version :: Int)
            val <- v .: "value"
            wepEnvironment <- v .: "environment"
            keys <- val .: "accountKeys"
            asdKeys <- mapM parseCredKeys =<< (keys .: "keys")
            asdThreshold <- keys .: "threshold"
            weaCredMap <- val .: "credentials"
            asdAddress <- val .: "address"
            let weaKeys = AccountSigningData{..}
            return WalletExport{wepAccounts = [WalletExportAccount{weaEncryptionKey = Nothing, ..}], ..}
        other -> fail $ printf "unsupported type '%s'" (other :: String)
  where
    parseCredKeys = AE.withObject "Credential keys" (.: "keys")

-- | Used for parsing 'WalletExport'.
newtype WalletExportIdentity = WalletExportIdentity
    {weiAccounts :: [WalletExportAccount]}
    deriving (Show)

instance AE.FromJSON WalletExportIdentity where
    parseJSON = AE.withObject "Identity" $ \v -> do
        weiAccounts <- v .: "accounts"
        return WalletExportIdentity{..}

data WalletExportAccount = WalletExportAccount
    { weaName :: !Text,
      weaKeys :: !AccountSigningData,
      weaCredMap :: !(OrdMap.Map IDTypes.CredentialIndex IDTypes.CredentialRegistrationID),
      weaEncryptionKey :: !(Maybe ElgamalSecretKey)
    }
    deriving (Show)

instance AE.FromJSON WalletExportAccount where
    parseJSON = AE.withObject "Account" $ \v -> do
        name <- v .: "name"
        addr <- v .: "address"
        e <- v .: "encryptionSecretKey"
        (keys, th) <-
            v .: "accountKeys"
                >>= AE.withObject
                    "Account keys"
                    ( \w -> do
                        credentials <- w .: "keys"
                        keys <- forM credentials $ AE.withObject "Credential keys" (.: "keys")
                        th <- w .: "threshold"
                        return (keys, th)
                    )
        -- the credentials are stored inside a credentials field in the exact format that they are sent to the chain,
        -- which is a (versioned) object with two fields "messageExpiry" and "credential"
        -- since the mobile only supports single-credential accounts by definition the credential is the
        -- credential with index 0
        credential :: IDTypes.AccountCredentialWithProofs <- (vValue <$> (v .: "credential")) >>= AE.withObject "Credential" (.: "credential")
        return
            WalletExportAccount
                { weaName = name,
                  weaKeys =
                    AccountSigningData
                        { asdAddress = addr,
                          asdKeys = keys,
                          asdThreshold = th
                        },
                  weaCredMap = OrdMap.singleton 0 (IDTypes.credId credential),
                  weaEncryptionKey = e
                }

-- | Decode, potentially decrypt and parse a wallet export. The function asks
-- for the password when it is needed. If importing the old mobile wallet export
-- then the keys are encrypted using the same password that is used for
-- decrypting the export.w
decodeMobileFormattedAccountExport ::
    -- | JSON with accounts and identities, this can either be encrypted or not. If it is encrypted it must be encrypted
    -- using the format expected of an 'EncryptedJSON WalletExport'.
    BS.ByteString ->
    -- | Only return the account with the given name (if it exists, otherwise return none).
    Maybe Text ->
    -- | Action to ask for password to decrypt the export or to encrypt the sign keys.
    IO Password ->
    -- | A list of resulting 'AccountConfig's and their environment, or an error message on failure.
    IO (Either String ([AccountConfig], Environment))
decodeMobileFormattedAccountExport jsonBS accountName passwordAct = runExceptT $ do
    jsonValue <- AE.eitherDecodeStrict jsonBS `embedErr` printf "cannot decode wallet export: %s"
    if isLikelyEncrypted jsonValue
        then do
            we :: EncryptedJSON WalletExport <- AE.eitherDecodeStrict jsonBS `embedErr` printf "cannot decode wallet export JSON: %s"
            password <- liftIO passwordAct
            WalletExport{..} <- decryptJSONWith we password (parseWalletExport accountName) `embedErr` (("cannot decrypt wallet export: " ++) . displayException)
            (,wepEnvironment) <$> accountCfgsFromWalletExportAccounts wepAccounts accountName password
        else do
            case AE.parseEither (parseWalletExport accountName) jsonValue of
                Left err -> throwError [i|Cannot decode wallet export: #{err}|]
                Right WalletExport{..} -> do
                    password <- liftIO passwordAct
                    (,wepEnvironment) <$> accountCfgsFromWalletExportAccounts wepAccounts accountName password

-- | Convert one or all wallet export accounts to regular account configs.
-- This encrypts all signing keys with the provided password.
-- If name is provided, only the account with matching name (if any) is converted.
-- Otherwise they all are. Names to be imported are checked to be valid.
accountCfgsFromWalletExportAccounts :: [WalletExportAccount] -> Maybe Text -> Password -> ExceptT String IO [AccountConfig]
accountCfgsFromWalletExportAccounts weas name pwd = do
    selectedAccounts <-
        case name of
            Nothing -> return weas
            Just n -> case Prelude.filter ((== n) . weaName) weas of
                [] ->
                    let possibleNames = showNameList . Prelude.map weaName $ weas
                    in  throwError [i|An account named '#{n}' does not exist. Possible account names: #{possibleNames}.|]
                namesFound -> return namesFound
    forM selectedAccounts $ accountCfgFromWalletExportAccount pwd

-- | Convert a wallet export account to a regular account config.
--  This checks whether the name provided by the export is a valid account name.
--  This encrypts all signing keys with the provided password.
accountCfgFromWalletExportAccount :: Password -> WalletExportAccount -> ExceptT String IO AccountConfig
accountCfgFromWalletExportAccount pwd WalletExportAccount{weaKeys = AccountSigningData{..}, ..} = do
    name <- liftIO $ ensureValidName weaName
    acKeys <- liftIO $ encryptAccountKeyMap pwd asdKeys
    acEncryptionKey <- case weaEncryptionKey of
        Nothing -> return Nothing
        Just ek -> liftIO . fmap Just $ encryptAccountEncryptionSecretKey pwd ek
    return $
        AccountConfig
            { acAddr = NamedAddress{naNames = [name], naAddr = asdAddress},
              acCids = weaCredMap,
              ..
            }
  where
    ensureValidName name =
        let trimmedName = T.strip name
        in  case validateAccountName trimmedName of
                Left err -> do
                    logError [err]
                    putStr "Input valid replacement name: "
                    T.getLine >>= ensureValidName
                Right () -> return trimmedName

-- | Decode and parse a genesis account into a named account config.
--  All signing keys are encrypted with the given password.
decodeGenesisFormattedAccountExport ::
    -- | JSON with the account information.
    BS.ByteString ->
    -- | Optionally, how to name the account. It is checked whether the name is valid.
    Maybe Text ->
    -- | Password to encrypt the signing keys with.
    Password ->
    -- | The resulting 'AccountConfig' or an error message on failure.
    IO (Either String AccountConfig)
decodeGenesisFormattedAccountExport payload name pwd = runExceptT $ do
    mapM_ validateAccountName name
    val <- AE.eitherDecodeStrict payload `embedErr` printf "cannot decode wallet export JSON: %s"
    action <- AE.parseEither accountParser val `embedErr` printf "cannot parse JSON: %s"
    liftIO action
  where
    accountParser = AE.withObject "Account data" $ \v -> do
        addr <- v .: "address"
        aks <- v .: "accountKeys"
        accEncryptionKey <- v .: "encryptionSecretKey"
        let accCredMap = gakKeys aks
        -- the genesis credentials
        cmap <- v .: "credentials"
        return $ do
            acKeys <- encryptAccountKeyMap pwd (gckKeys <$> accCredMap)
            acEncryptionKey <- Just <$> (liftIO $ encryptAccountEncryptionSecretKey pwd accEncryptionKey)
            return
                AccountConfig
                    { acAddr = NamedAddress{naNames = maybeToList name, naAddr = addr},
                      acCids = IDTypes.credId <$> (gcCredentials . vValue $ cmap),
                      ..
                    }

---- Code for instantiating, exporting and importing config backups

-- | An export format used for config backups.
data ConfigBackup = ConfigBackup
    { cbAccounts :: [AccountConfig],
      cbContractNameMap :: ContractNameMap,
      cbModuleNameMap :: ModuleNameMap
    }
    deriving (Eq, Show)

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
configExport ::
    -- | The contents to back up.
    ConfigBackup ->
    -- | Password to decrypt the export.
    Maybe Password ->
    -- | JSON with the backed up config.
    IO BS.ByteString
configExport cb pwd = do
    case pwd of
        Just password -> do
            contents <- encryptJSON AES256 PBKDF2SHA256 vcb password
            return . LazyBS.toStrict . AE.encode $ AE.object ["type" .= AE.String "encrypted", "contents" .= contents]
        Nothing -> do
            return . LazyBS.toStrict . AE.encode $ AE.object ["type" .= AE.String "unencrypted", "contents" .= vcb]
  where
    vcb = Versioned configBackupVersion cb

-- | Decrypt and decode an exported config, optionally protected under a password.
configImport ::
    BS.ByteString ->
    -- | Action to obtain the password if necessary.
    IO Password ->
    IO (Either String ConfigBackup)
configImport json pwdAction = runExceptT $ do
    vconfigBackup <- AE.eitherDecodeStrict json `embedErr` (\err -> [i|The input file is not valid JSON: #{err}|])
    case AE.parseEither importParser vconfigBackup of
        Left err -> throwError [i|Failed reading the input file: #{err}|]
        Right (Left encrypted) -> do
            pwd <- liftIO pwdAction
            (Versioned _ cb) <-
                decryptJSON encrypted pwd
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
