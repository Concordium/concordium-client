{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.Client.Types.Account (
  module Concordium.Client.Types.Account,
  CryptoFFI.ElgamalSecretKey
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Exception

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Aeson as AE
import Data.Aeson ((.=),(.:),(.:?), (.!=))

import Concordium.Client.Utils
import Concordium.Client.Encryption

import qualified Concordium.Crypto.FFIDataTypes as CryptoFFI
import qualified Concordium.ID.Types as ID
import qualified Concordium.Types as Types
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.Ed25519Signature as Ed25519
import Data.Serialize (decode, encode)
import Data.ByteString (ByteString)


-- * Accounts

data NamedAddress = NamedAddress { naNames :: [Text], naAddr :: ID.AccountAddress }
  deriving (Show, Eq)

instance AE.ToJSON NamedAddress where
  toJSON (NamedAddress naNames naAddr) = AE.object ["names" .= naNames, "address" .= naAddr]

instance AE.FromJSON NamedAddress where
  parseJSON = AE.withObject "NamedAddress" $ \v -> do
    naNames <- v .: "names"
    naAddr <- v .: "address"
    return NamedAddress{..}

-- * Account keys

-- ** Encrypted and unencrypted account keys

type AccountKeyPair = SigScheme.KeyPair

data EncryptedAccountKeyPair
  = EncryptedAccountKeyPairEd25519
    { verifyKey :: !Ed25519.VerifyKey
    , encryptedSignKey :: !(EncryptedJSON Ed25519.SignKey)
    } deriving(Show, Eq)

instance AE.ToJSON EncryptedAccountKeyPair where
  toJSON EncryptedAccountKeyPairEd25519{..} =
    AE.object [ "schemeId" .= SigScheme.Ed25519
              , "verifyKey" .= verifyKey
              , "encryptedSignKey" .= encryptedSignKey
              ]


instance AE.FromJSON EncryptedAccountKeyPair where
  parseJSON = AE.withObject "EncryptedAccountKeyPair" $ \v -> do
    verifyKey <- v .: "verifyKey"
    encryptedSignKey <- v .: "encryptedSignKey"
    schemeId <- v .:? "schemeId" .!= SigScheme.Ed25519
    case schemeId of
      SigScheme.Ed25519 -> return EncryptedAccountKeyPairEd25519{..}

-- |Full map of plaintext account signing keys.
type AccountKeyMap = Map.Map ID.CredentialIndex (Map.Map ID.KeyIndex AccountKeyPair)
-- |Encrypted analogue of 'AccountKeyMap'
type EncryptedAccountKeyMap = Map.Map ID.CredentialIndex (Map.Map ID.KeyIndex EncryptedAccountKeyPair)
type EncryptedAccountEncryptionSecretKey = EncryptedText

-- |Get the number of keys in the key map.
mapNumKeys :: Map.Map ID.CredentialIndex (Map.Map ID.KeyIndex a) -> Int
mapNumKeys = sum . fmap Map.size

-- |Information about a given account sufficient to sign transactions.
-- This includes the plain signing keys.
data AccountSigningData =
  AccountSigningData
  { asdAddress :: !Types.AccountAddress
  , asdKeys :: !AccountKeyMap
  , asdThreshold :: !ID.AccountThreshold }
  deriving (Show)


-- |Selected keys resolved from the account config for the specific interaction.
-- In contrast to the account config this will only contain the keys the user selected.
-- The keys are still encrypted. They will only be decrypted when they will be used.
data EncryptedSigningData =
  EncryptedSigningData
  { esdAddress :: !NamedAddress
  , esdKeys :: !EncryptedAccountKeyMap
  , esdEncryptionKey :: !(Maybe EncryptedAccountEncryptionSecretKey) }
  deriving (Show)

-- | Test whether the given keypair passes a basic sanity check, signing and
-- verifying the signature with the keypair should succeed.
checkAccountKeyPair :: AccountKeyPair -> IO Bool
checkAccountKeyPair kp = do
  let sgn = SigScheme.sign kp "challenge"
  return $ SigScheme.verify (SigScheme.correspondingVerifyKey kp) "challenge" sgn

decryptAccountKeyPair
  :: Password
  -> ID.KeyIndex -- ^ The key index that belongs to the key pair, only used in error message.
  -> EncryptedAccountKeyPair
  -> ExceptT String IO AccountKeyPair -- ^ The decrypted 'AccountKeyPair' or an error message on failure.
decryptAccountKeyPair pwd keyIndex EncryptedAccountKeyPairEd25519{..} = do
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
  return EncryptedAccountKeyPairEd25519{..}

encryptAccountKeyMap :: Password -> AccountKeyMap -> IO EncryptedAccountKeyMap
encryptAccountKeyMap pwd akmap = mapM (mapM (encryptAccountKeyPair pwd)) akmap

-- | Decrypt the given encrypted account keys using the same password for each key.
decryptAccountKeyMap
  :: EncryptedAccountKeyMap
  -> Password
  -> IO (Either String AccountKeyMap)
decryptAccountKeyMap encryptedKeyMap pwd =
  runExceptT $ sequence $ Map.map (sequence . Map.mapWithKey (decryptAccountKeyPair pwd)) encryptedKeyMap

-- |Encrypt, with the given password, the secret key for decrypting encrypted amounts
encryptAccountEncryptionSecretKey :: Password -> CryptoFFI.ElgamalSecretKey -> IO EncryptedAccountEncryptionSecretKey
encryptAccountEncryptionSecretKey pwd secret = encryptText AES256 PBKDF2SHA256 (encode secret) pwd

-- |Attempt to decrypt, with the given password, the secret key for decrypting encrypted amounts
decryptAccountEncryptionSecretKey :: Password -> EncryptedAccountEncryptionSecretKey -> IO (Either String CryptoFFI.ElgamalSecretKey)
decryptAccountEncryptionSecretKey pwd secret =
  either (Left . displayException) decode <$> runExceptT (decryptText secret pwd :: ExceptT DecryptionFailure IO ByteString)

-- * Account exports

data AccountExportFormat = FormatMobile | FormatGenesis
  deriving (Show)
