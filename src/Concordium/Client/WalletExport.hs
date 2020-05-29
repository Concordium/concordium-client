{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Concordium.Client.WalletExport where

import Concordium.Client.Cli
import Concordium.Client.Config

import Control.Monad.Except
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.KDF.PBKDF2
import Crypto.Error
import Data.Aeson as AE
import Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString(ByteString)
import Text.Printf

data WalletExport =
  WalletExport
  { weMetadata :: !WalletExportMetadata
  , weCipherText :: !Text }
  deriving (Show)

instance AE.FromJSON WalletExport where
  parseJSON = withObject "Wallet Export" $ \v -> do
    weMetadata <- v .: "metadata"
    weCipherText <- v .: "cipherText"
    return WalletExport {..}

data SupportedEncryptionMethod = AES256
    deriving(Show)

data SupportedKeyDerivationMethod = PBKDF2SHA256
    deriving(Show)

instance FromJSON SupportedEncryptionMethod where
  parseJSON = withText "Encryption method" $ \t ->
    if t == "AES-256" then return AES256
    else fail $ "Unsupported encryption method: " ++ T.unpack t

instance FromJSON SupportedKeyDerivationMethod where
  parseJSON = withText "Key derivation method" $ \t ->
    if t == "PBKDF2WithHmacSHA256" then return PBKDF2SHA256
    else fail $ "Unsupported key derivation method: " ++ T.unpack t

data WalletExportMetadata =
  WalletExportMetadata
  { wemEncryptionMethod :: !SupportedEncryptionMethod
  , wemKeyDerivationMethod :: !SupportedKeyDerivationMethod
  , wemIterations :: !Int
  , wemSalt :: !Text
  , wemInitializationVector :: !Text }
  deriving (Show)

instance AE.FromJSON WalletExportMetadata where
  parseJSON = withObject "Wallet Export Metadata" $ \v -> do
    wemEncryptionMethod <- v .: "encryptionMethod"
    wemKeyDerivationMethod <- v .: "keyDerivationMethod"
    case (wemEncryptionMethod, wemKeyDerivationMethod) of
      (AES256, PBKDF2SHA256) -> do
        wemIterations <- v .: "iterations"
        wemSalt <- v .: "salt"
        wemInitializationVector <- v .: "initializationVector"
        return WalletExportMetadata {..}

data WalletExportPayload =
  WalletExportPayload
  { wepAccounts :: ![WalletExportAccount] }
  deriving (Show)

instance AE.FromJSON WalletExportPayload where
  parseJSON = withObject "Wallet Export Payload" $ \v -> do
    -- Validate type and version.
    t <- v .: "type"
    version <- v .: "v"
    unless (t == "concordium-mobile-wallet-data") $
      fail $ printf "unsupported type '%s'" (t :: String)
    unless (version == 1) $
      fail $ printf "unsupported version '%s'" (version :: Int)

    val <- v .: "value"
    ids <- val .: "identities"
    return WalletExportPayload { wepAccounts = join $ weiAccounts <$> ids }

newtype WalletExportIdentity =
  WalletExportIdentity
  { weiAccounts :: [WalletExportAccount] }
  deriving (Show)

instance AE.FromJSON WalletExportIdentity where
  parseJSON = withObject "Identity" $ \v -> do
    weiAccounts <- v .: "accounts"
    return WalletExportIdentity {..}

data WalletExportAccount =
  WalletExportAccount
  { weaName :: !Text
  , weaKeys :: !AccountKeys }
  deriving (Show)

instance AE.FromJSON WalletExportAccount where
  parseJSON = withObject "Account" $ \v -> do
    name <- v .: "name"
    addr <- v .: "address"
    (keys, th) <- v .: "accountData" >>= withObject "Account data" (\w -> do
      keys <- w .: "keys"
      th <- w .: "threshold"
      return (keys, th))
    return WalletExportAccount
      { weaName = name
      , weaKeys = AccountKeys
                  { akAddress = addr
                  , akKeys = keys
                  , akThreshold = th } }

-- |Using the provided password, decrypt the payload from the export according to the parameters in the accompanying metadata.
decryptWalletExport :: (MonadError String m) => WalletExport -> ByteString -> m WalletExportPayload
decryptWalletExport e password = do
  let md = weMetadata e

  case (wemEncryptionMethod md, wemKeyDerivationMethod md) of
    (AES256, PBKDF2SHA256) -> do
      salt <- decodeBase64 "salt" $ wemSalt md
      initVec <- decodeBase64 "initialization vector" $ wemInitializationVector md
      cipher <- decodeBase64 "cipher" $ weCipherText e

      iv <- case makeIV initVec of
              Nothing -> throwError "cannot make initialization vector"
              Just iv -> return iv

      let iterations = wemIterations md
          outputLen = 32

      let key = fastPBKDF2_SHA256 (Parameters iterations outputLen) password salt :: ByteString

      aes <- case cipherInit key of
               CryptoFailed err -> throwError $ printf "cipher initialization failed: %s" (show err)
               CryptoPassed a -> return a

      let unpadded = unpad (PKCS7 1) (cbcDecrypt (aes :: AES256) iv cipher :: ByteString)
      case unpadded of
        Nothing -> throwError "incorrect password"
        Just v -> case eitherDecodeStrict v of
                    Left err -> throwError err
                    Right p -> return p
  where decodeBase64 name s = case Base64.decode $ encodeUtf8 s of
          Left err -> throwError $ printf "cannot decode '%s': %s" (name :: String) err
          Right v -> return v

-- |Convert one or all wallet export accounts to regular account configs.
-- If name is provided, only the account with mathing name (if any) is converted.
-- Otherwise they all are.
accountCfgsFromWalletExportAccounts :: [WalletExportAccount] -> Maybe Text -> [AccountConfig]
accountCfgsFromWalletExportAccounts weas name = case name of
  Nothing -> accountCfgFromWalletExportAccount <$> weas
  Just n -> accountCfgsFromWalletExportAccounts (Prelude.filter (f n) weas) Nothing
  where f n wea = weaName wea == n

-- |Convert wallet export account to regular account config.
accountCfgFromWalletExportAccount :: WalletExportAccount -> AccountConfig
accountCfgFromWalletExportAccount wea =
  let WalletExportAccount
        { weaName = name
        , weaKeys = AccountKeys
                    { akAddress = addr
                    , akKeys = keys
                    , akThreshold = th } }
        = wea
  in AccountConfig
     { acAddr = NamedAddress {naName = Just name, naAddr = addr }
     , acKeys = keys
     , acThreshold = th }
