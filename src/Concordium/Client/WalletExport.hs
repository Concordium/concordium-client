{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Concordium.Client.WalletExport where

import Concordium.Client.Utils
import Concordium.Client.Cli
import Concordium.Client.Config
import Concordium.Client.Encryption

import Control.Monad.Except
import Control.Exception
import Data.Aeson as AE
import Data.Text as T
import Text.Printf

type WalletExport = EncryptedText

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

data DecryptWalletExportFailure
  -- | Decryption failed.
  = DecryptionFailure DecryptionFailure
  -- | The decrypted export is not a valid JSON object. If there is no data corruption, this indicates that a wrong password was given.
  | IncorrectJSON String
  deriving Show

instance Exception DecryptWalletExportFailure where
  displayException e = "cannot decrypt wallet export: " ++
    case e of
      DecryptionFailure df -> displayException df
      IncorrectJSON err -> "cannot parse JSON: " ++ err


-- |Using the provided password, decrypt the payload from the export according to the parameters in the accompanying metadata.
decryptWalletExport
  :: (MonadError DecryptWalletExportFailure m)
  => WalletExport
  -> Password
  -> m WalletExportPayload
decryptWalletExport walletExport password = do
  payloadJSON <- decryptText walletExport password `embedErr` DecryptionFailure
  AE.eitherDecodeStrict payloadJSON `embedErr` IncorrectJSON

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
