{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.Client.Types.Account where

import Control.Monad
import Control.Monad.Except
import Control.Exception

import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Aeson as AE
import Data.Aeson ((.=),(.:),(.:?))

import Concordium.Client.Utils
import Concordium.Client.Encryption

import qualified Concordium.ID.Types as ID
import qualified Concordium.Types as Types
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.Ed25519Signature as Ed25519


-- * Accounts

data NamedAddress = NamedAddress { naName :: Maybe Text, naAddr :: ID.AccountAddress }
  deriving (Show, Eq)

-- * Account keys

-- ** Encrypted and unencrypted account keys

type AccountKeyPair = SigScheme.KeyPair

data EncryptedAccountKeyPair
  = EncryptedAccountKeyPair
    { verifyKey :: !Ed25519.VerifyKey
    , encryptedSignKey :: !(EncryptedJSON Ed25519.SignKey)
    }

instance AE.ToJSON EncryptedAccountKeyPair where
  toJSON EncryptedAccountKeyPair{..} =
    AE.object [ "schemeId" .= SigScheme.Ed25519
              , "verifyKey" .= verifyKey
              , "encryptedSignKey" .= encryptedSignKey
              ]

instance AE.FromJSON EncryptedAccountKeyPair where
  parseJSON = AE.withObject "EncryptedAccountKeyPair" $ \v -> do
    verifyKey <- v .: "verifyKey"
    encryptedSignKey <- v .: "encryptedSignKey"
    maybeSchemeId <- v .:? "schemeId"
    -- NB: Currently there is no need for the signature scheme to be part of 'EncryptedAccountKeyPair'
    -- because we only support one. We check however that if a signature scheme is specified it
    -- is the only allowed value (see 'FromJSON' for 'SigScheme.SchemeId').
    case maybeSchemeId of
      Just SigScheme.Ed25519 -> return ()
      Nothing -> return ()
    return EncryptedAccountKeyPair{..}

type AccountKeyMap = Map.HashMap ID.KeyIndex AccountKeyPair
type EncryptedAccountKeyMap = Map.HashMap ID.KeyIndex EncryptedAccountKeyPair


-- |Information about a given account sufficient to sign transactions.
-- This includes the plain signing keys.
data AccountSigningData =
  AccountSigningData
  { asdAddress :: Types.AccountAddress
  , asdKeys :: AccountKeyMap
  , asdThreshold :: ID.SignatureThreshold }
  deriving (Show)

-- | Test whether the givin key pair is valid by encrypting and decrypting a test value.
-- TODO implement
checkAccountKeyPair :: AccountKeyPair -> IO Bool
checkAccountKeyPair SigScheme.KeyPairEd25519{..} = return True

decryptAccountKeyPair
  :: Password
  -> ID.KeyIndex -- ^ The key index that belongs to the key pair, only used in error message.
  -> EncryptedAccountKeyPair
  -> ExceptT String IO AccountKeyPair -- ^ The decrypted 'AccountKeyPair' or an error message on failure.
decryptAccountKeyPair pwd keyIndex EncryptedAccountKeyPair{..} = do
  signKey <- decryptJSON encryptedSignKey pwd `embedErr`
    ((("cannot decrypt signing key with index " ++ show keyIndex ++ ": ") ++ ) . displayException)
  let kp :: AccountKeyPair = SigScheme.KeyPairEd25519{..}
  -- Now test whether the obtained key pair is valid by testing encryption/decryption.
  -- If this fails and the files were not corrupted, most likely the password was wrong.
  keyPairCheck <- liftIO $ checkAccountKeyPair kp
  unless keyPairCheck $ throwError "wrong password"
  return kp

encryptAccountKeyPair :: Password -> AccountKeyPair -> IO EncryptedAccountKeyPair
encryptAccountKeyPair pwd SigScheme.KeyPairEd25519{..} = do
  encryptedSignKey <- encryptJSON AES256 PBKDF2SHA256 signKey pwd
  return EncryptedAccountKeyPair{..}

encryptAccountKeyMap :: AccountKeyMap -> Password -> IO EncryptedAccountKeyMap
encryptAccountKeyMap keyMap pwd = forM keyMap $ encryptAccountKeyPair pwd


-- | Decrypt the given encrypted account keys using the same password for each key.
decryptAccountKeyMap
  :: EncryptedAccountKeyMap
  -> Password
  -> IO (Either String AccountKeyMap)
decryptAccountKeyMap encryptedKeyMap pwd =
  runExceptT $ sequence $ Map.mapWithKey (decryptAccountKeyPair pwd) encryptedKeyMap

-- * Account exports

data AccountExportFormat = FormatMobile | FormatWeb
  deriving (Show)
