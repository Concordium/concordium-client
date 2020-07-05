{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Concordium.Client.WalletExport where

import Concordium.Client.Cli
import Concordium.Client.Config

import Control.Monad.Except
import Data.Aeson as AE
import Data.Text as T
import Text.Printf

data WalletExport =
  WalletExport
  { wepAccounts :: ![WalletExportAccount] }
  deriving (Show)

instance AE.FromJSON WalletExport where
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
    return WalletExport { wepAccounts = join $ weiAccounts <$> ids }

-- | Used for parsing 'WalletExport'.
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

-- |Convert one or all wallet export accounts to regular account configs.
-- If name is provided, only the account with matching name (if any) is converted.
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
