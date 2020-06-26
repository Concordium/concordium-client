{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module SimpleClientTests.EncryptionSpec where

import qualified Data.ByteString as BS
import Data.ByteString(ByteString)
import qualified Data.Aeson as AE

import Control.Monad
import Control.Exception hiding (assert)

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec
import Test.Hspec.QuickCheck

import Concordium.Client.Encryption

-- Needed for QuickCheck only.
deriving instance Show Password

genByteString :: Gen ByteString
genByteString = sized $ \n -> BS.pack <$> vector n

genEncryptionInput :: Gen (ByteString, Password)
genEncryptionInput = sized $ \n -> do
  text <- resize n $ genByteString
  pwdLen :: Int <- elements [1..100]
  pwd <- resize pwdLen $ genByteString
  return (text, Password pwd)

testEncryptionDecryption :: Int -> Spec
testEncryptionDecryption size = do
  specify (show size) $
    forAll (resize size $ genEncryptionInput) $ \(text, pwd) -> monadicIO $ do
      encrypted <- run $ encryptText AES256 PBKDF2SHA256 text pwd
      decrypted <- run $ either (fail . displayException) return $ decryptText encrypted pwd
      assert $ decrypted == text

testToFromJSON :: Int -> Spec
testToFromJSON size = do
  specify (show size) $
    forAll (resize size $ genEncryptionInput) $ \(text, pwd) -> monadicIO $ do
      encrypted <- run $ encryptText AES256 PBKDF2SHA256 text pwd
      let json = AE.toJSON encrypted
      let encryptedFromJSON = AE.fromJSON json
      case encryptedFromJSON of
        AE.Success res -> assert $ json == res
        AE.Error err -> fail err

tests :: Spec
tests = do
  let sizes = [1,7,63,64,65,200,444,5000]
  describe "Encryption" $ do
    -- NB: With the current iteartion count of 100000 for key derivation,
    -- the average for one of the 40 encryption/decryption tests takes already around 0.1s.
    describe "Encryption and decryption of a random ByteString of length ..." $
      modifyMaxSuccess (const 5) $ forM_ sizes $ testEncryptionDecryption
    describe "Conversion of an EncryptedText of a ByteString of length ... to/from JSON" $
      modifyMaxSuccess (const 5) $ forM_ sizes $ testToFromJSON

