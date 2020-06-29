{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

{-
Encryption utilities.
-}
module Concordium.Client.Encryption where

import Control.Monad.Except
import Control.Exception
import qualified System.IO as IO

import qualified Data.Aeson as AE
import Data.Aeson ((.:),(.=))
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Crypto.Random
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Data.Padding
import Crypto.KDF.PBKDF2
import Crypto.Error

-- | A wrapper for 'ByteString' to be used for passwords.
-- Using this dedicated type is supposed to reduce the risk of accidentally exposing passwords.
newtype Password = Password { getPassword :: ByteString }

data SupportedEncryptionMethod = AES256
  deriving(Show)

instance AE.FromJSON SupportedEncryptionMethod where
  parseJSON = AE.withText "Encryption method" $ \t ->
    if t == "AES-256" then return AES256
    else fail $ "Unsupported encryption method: " ++ T.unpack t

instance AE.ToJSON SupportedEncryptionMethod where
  toJSON AES256 = AE.String "AES-256"

data SupportedKeyDerivationMethod = PBKDF2SHA256
  deriving(Show)

instance AE.ToJSON SupportedKeyDerivationMethod where
  toJSON PBKDF2SHA256 = AE.String "PBKDF2WithHmacSHA256"

instance AE.FromJSON SupportedKeyDerivationMethod where
  parseJSON = AE.withText "Key derivation method" $ \t ->
    if t == "PBKDF2WithHmacSHA256" then return PBKDF2SHA256
    else fail $ "Unsupported key derivation method: " ++ T.unpack t

-- | Meta data for an encrypted text. Needed for decryption.
data EncryptionMetadata = EncryptionMetadata
  { emEncryptionMethod :: !SupportedEncryptionMethod
  , emKeyDerivationMethod :: !SupportedKeyDerivationMethod
  , emIterations :: !Word
  , emSalt :: !Text
  , emInitializationVector :: !Text
  }
  deriving (Show)

instance AE.ToJSON EncryptionMetadata where
  toJSON EncryptionMetadata{..} =
    case (emEncryptionMethod, emKeyDerivationMethod) of
      (AES256, PBKDF2SHA256) ->
        AE.object [ "encryptionMethod" .= emEncryptionMethod
                  , "keyDerivationMethod" .= emKeyDerivationMethod
                  , "iterations" .= emIterations
                  , "salt" .= emSalt
                  , "initializationVector" .= emInitializationVector
                  ]

instance AE.FromJSON EncryptionMetadata where
  parseJSON = AE.withObject "EncryptionMetadata" $ \v -> do
    emEncryptionMethod <- v .: "encryptionMethod"
    emKeyDerivationMethod <- v .: "keyDerivationMethod"
    case (emEncryptionMethod, emKeyDerivationMethod) of
      (AES256, PBKDF2SHA256) -> do
        emIterations <- v .: "iterations"
        emSalt <- v .: "salt"
        emInitializationVector <- v .: "initializationVector"
        return EncryptionMetadata {..}

-- | An encrypted text with meta data needed for decryption.
data EncryptedText = EncryptedText
  { etMetadata :: !EncryptionMetadata
  , etCipherText :: !Text
  }

instance AE.ToJSON EncryptedText where
  toJSON EncryptedText{..} =
    AE.object [ "metadata" .= etMetadata
              , "cipherText" .= etCipherText
              ]

instance AE.FromJSON EncryptedText where
  parseJSON = AE.withObject "EncryptedText" $ \v -> do
    etMetadata <- v .: "metadata"
    etCipherText <- v .: "cipherText"
    return EncryptedText {..}

-- | Possible decryption failures for 'AES256' with 'PBKDF2SHA256'.
data DecryptionFailure
  -- | Base64 decoding failed.
  = DecodeError { -- | Parameter for which decoding failed.
                  deParam :: String
                  -- | The decoding error.
                , deErr :: String
                }
  -- | Creating the initialization vector in the cryptonite library failed.
  -- This happens when it does not have the right length (16 bytes for AES).
  | MakingInitializationVectorFailed
  -- | Cipher initialization in the cryptonite library failed.
  | CipherInitializationFailed CryptoError
  -- | Unpadding after decryption failed. If there is no data corruption, this indicates that a wrong password was given.
  | UnpaddingFailed
  deriving Show

instance Exception DecryptionFailure where
  displayException e = "decryption failure: " ++
    case e of
      DecodeError field err -> "cannot decode " ++ field ++ ": " ++ err
      MakingInitializationVectorFailed -> "making initialization vector failed"
      CipherInitializationFailed err -> "cipher initialization failed: " ++ show err
      UnpaddingFailed -> "wrong password"


-- | Ask for a password on standard input not showing what is typed.
askPassword
  :: Text
  -> IO Password
askPassword text = do
  T.putStr text
  -- Get the password from command line, not showing what is typed by temporarily disabling echo.
  password <- bracket_ (IO.hSetEcho IO.stdin False) (IO.hSetEcho IO.stdin True) BS.getLine
  putStrLn ""
  return (Password password)

-- | Decrypt an 'EncryptedText' where cipher, initialization vector and salt are Base64-encoded.
decryptText :: (MonadError DecryptionFailure m)
 => EncryptedText
 -> Password
 -> m ByteString
decryptText EncryptedText{etMetadata=EncryptionMetadata{..},..} pwd = do
  case (emEncryptionMethod, emKeyDerivationMethod) of
    (AES256, PBKDF2SHA256) -> do
      salt <- decodeBase64 "salt" emSalt
      initVec <- decodeBase64 "initializationVector" emInitializationVector
      cipher <- decodeBase64 "cipherText" etCipherText

      iv <- case makeIV initVec of
              Nothing -> throwError MakingInitializationVectorFailed
              Just iv -> return iv
      let keyLen = 32

      -- NB: fromIntegral is safe to do as too large Word values will result in negative Int values, which should be rejected.
      let key = fastPBKDF2_SHA256 (Parameters (fromIntegral emIterations) keyLen) (getPassword pwd) salt :: ByteString
      (aes :: AES256) <- case cipherInit key of
                           CryptoFailed err -> throwError $ CipherInitializationFailed err
                           CryptoPassed a -> return a

      let decrypted = cbcDecrypt aes iv cipher :: ByteString
      -- Unpadding for 16 byte block size.
      let unpadded = unpad (PKCS7 16) decrypted
      case unpadded of
        Nothing -> throwError UnpaddingFailed
        Just text -> return text

  where decodeBase64 :: MonadError DecryptionFailure m => String -> Text -> m ByteString
        -- Decoding fails if the input is not valid base64.
        decodeBase64 name s = case Base64.decode $ T.encodeUtf8 s of
          Left err -> throwError $ DecodeError name err
          Right v -> return v

-- | Encrypt a 'ByteString' with the given password, using the given encryption and key derivation
-- method. Initialization vector and salt are generated randomly using 'getRandomBytes' from
-- @MonadRandom IO@ and included in the returned meta data. Cipher, initialization vector and salt
-- are Base64-encoded in the output.
encryptText
  :: SupportedEncryptionMethod
  -> SupportedKeyDerivationMethod
  -> ByteString
  -> Password
  -> IO EncryptedText
encryptText emEncryptionMethod emKeyDerivationMethod text pwd =
  case (emEncryptionMethod, emKeyDerivationMethod) of
    -- See also specification RFC2898 (https://tools.ietf.org/html/rfc2898) and
    -- recommendations from NIST in SP800-132 (2010 publication, still available:
    -- https://csrc.nist.gov/publications/detail/sp/800-132/final).
    (AES256, PBKDF2SHA256) -> do
      -- NOTE: The initialization vector should only be used once for the same key.
      initVec <- getRandomBytes 16 -- Length must be block size, which is 128bit for AES.
      iv :: IV AES256 <- case makeIV initVec of
              -- NB: This should not happen because we generate a valid initialization vector above.
              Nothing -> fail "Encryption error: making initialization vector failed."
              Just iv -> return iv
      -- RFC2898 section 4.2 recommends minimum 1000 iterations; NIST as many as feasible;
      -- IOS4 uses 10000 according to https://en.wikipedia.org/wiki/Pbkdf2#Purpose_and_operation.
      -- Current wallet export (app version 0.5.8) uses 100000.
      let emIterations = 100000
      -- RFC2898 section 4.1 recommends at least 64bits; NIST recommends 128bit.
      salt <- getRandomBytes 16
      let keyLen = 32

            -- NB: fromIntegral is safe to do as too large Word values will result in negative Int values, which should be rejected.
      let key = fastPBKDF2_SHA256 (Parameters (fromIntegral emIterations) keyLen) (getPassword pwd) salt :: ByteString

      (aes :: AES256) <- case cipherInit key of
                           -- NB: This should not happen because we generate a valid key above.
                           CryptoFailed err -> fail $ "Encryption error: cipher initialization failed: " ++ show err
                           CryptoPassed a -> return a

      -- Padding for 16 byte block size.
      let paddedText = pad (PKCS7 16) text
      let cipher = cbcEncrypt aes iv paddedText
      return EncryptedText { etCipherText = encodeBase64 cipher
                           , etMetadata = EncryptionMetadata
                                          { emSalt = encodeBase64 salt
                                          , emInitializationVector = encodeBase64 initVec
                                          , ..
                                          }
                           }

  where encodeBase64 :: ByteString -> Text
        -- NB: T.decodeUtf8 expects valid UTF8 and will throw an exception otherwise;
        -- the base64 encoding however produces valid UTF8.
        encodeBase64 = T.decodeUtf8 . Base64.encode
