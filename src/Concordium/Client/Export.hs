{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.Client.Export where

import Concordium.Client.Utils
import Concordium.Client.Config
import Concordium.Client.Encryption
import Concordium.Client.Types.Account

import Control.Monad.Except
import Control.Exception
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import Data.Aeson ((.:),(.:?),(.!=))
import Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Map
import Text.Printf

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
    return WalletExport { wepAccounts = join $ weiAccounts <$> ids }

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
  validateAccountName weaName
  acKeys <- liftIO $ encryptAccountKeyMap pwd asdKeys
  acEncryptionKey <- Just <$> (liftIO $ encryptAccountEncryptionSecretKey pwd weaEncryptionKey)
  return $ AccountConfig
    { acAddr = NamedAddress { naName = Just weaName, naAddr = asdAddress }
    , acThreshold = asdThreshold
    , ..
    }

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
