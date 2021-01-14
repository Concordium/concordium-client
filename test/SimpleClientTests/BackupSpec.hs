{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleClientTests.BackupSpec where

import Concordium.Client.Config
import Concordium.Client.Encryption
import Concordium.Client.Export
import Concordium.Client.Types.Account
import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import Concordium.Types.HashableTo (getHash)

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import Test.Hspec

exampleAccountAddress1 :: IDTypes.AccountAddress
Right exampleAccountAddress1 = IDTypes.addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"

-- |dummy accountconfig, for testing export/import
exampleAccountConfigWithKeysAndName :: AccountConfig
exampleAccountConfigWithKeysAndName =
  AccountConfig
  { acAddr = NamedAddress { naName = Just "name" , naAddr = exampleAccountAddress1 }
  , acKeys = M.fromList [ (11,
                           EncryptedAccountKeyPairEd25519 {
                              verifyKey=vk1
                              , encryptedSignKey = EncryptedJSON (EncryptedText {
                                                                     etMetadata = EncryptionMetadata {
                                                                         emEncryptionMethod = AES256,
                                                                           emKeyDerivationMethod = PBKDF2SHA256,
                                                                           emIterations = 100000,
                                                                           emSalt = "sQ8NG/fBLdLuuLd1ARlAqw==",
                                                                           emInitializationVector = "z6tTcT5ko8vS2utlwwNvbw=="},
                                                                       etCipherText = "9ltKSJtlkiBXY/kU8huA4GoCaGNjy8M2Ym2SOtlg1ay6lfI9o95sXJ1cjcQ2b8gV+WddwS7ile8ZhIr8es58pTaM8PczlLbKBCSJ11R2iqw="})
                              })
                        , ( 2,
                            EncryptedAccountKeyPairEd25519 {
                              verifyKey=vk2
                              , encryptedSignKey = EncryptedJSON (EncryptedText {
                                                                     etMetadata = EncryptionMetadata {
                                                                         emEncryptionMethod = AES256,
                                                                         emKeyDerivationMethod = PBKDF2SHA256,
                                                                         emIterations = 100000,
                                                                         emSalt = "slzkcKo8IPymU5t7jamGQQ==",
                                                                         emInitializationVector = "NXbbI8Cc3AXtaG/go+L+FA=="},
                                                                     etCipherText = "hV5NemYi36f3erxCE8sC/uUdHKe1+2OrP3JVYVtBeUqn3QrOm8dlJcAd4mk7ufogJVyv0OR56w/oKqQ7HG8/UycDYtBlubGRHE0Ym4LCoqY="})
                              })]
  , acThreshold = 2
  , acEncryptionKey = Just EncryptedText {
      etMetadata = EncryptionMetadata {
          emEncryptionMethod = AES256,
          emIterations = 100000,
          emSalt = "w7pmsDi1K4bWf+zkLCuzVw==",
          emInitializationVector = "EXhd7ctFeqKvaA0P/oB8wA==",
          emKeyDerivationMethod = PBKDF2SHA256
          },
      etCipherText = "pYvIywCAMLhvag1EJmGVuVezGsNvYn24zBnB6TCTkwEwOH50AOrx8NAZnVuQteZMQ7k7Kd7a1RorSxIQI1H/WX+Usi8f3VLnzdZFJmbk4Cme+dcgAbI+wWr0hisgrCDl"
      }}
  where -- s1 = "6d00a10ccac23d2fd0bea163756487288fd19ff3810e1d3f73b686e60d801915"
        v1 = "c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027"
        -- s2 = "9b301aa72d991d720750935de632983f1854d701ada3e5b763215d0802d5541c"
        v2 = "f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213"
        -- (Just sk1) = BSH.deserializeBase16 s1
        (Just vk1) = BSH.deserializeBase16 v1
        -- (Just sk2) = BSH.deserializeBase16 s2
        (Just vk2) = BSH.deserializeBase16 v2

exampleContractNameMap :: ContractNameMap
exampleContractNameMap = M.fromList [("contrA", mkContrAddr 0 0), ("contrB", mkContrAddr 42 0), ("contrC", mkContrAddr 42 4200)]
  where mkContrAddr index subindex = Types.ContractAddress (Types.ContractIndex index) (Types.ContractSubindex subindex)

exampleModuleNameMap :: ModuleNameMap
exampleModuleNameMap = M.fromList [("modA", modRef1), ("modB", modRef2)]
  where
    modRef1 = Types.ModuleRef $ getHash ("ref1" :: BS.ByteString) -- Hash: de0cd794099a5e03c2131d662d423164111d3b78d5122970197cd7e1937ed0e4
    modRef2 = Types.ModuleRef $ getHash ("ref2" :: BS.ByteString) -- Hash: 3bdc9752a50026c173ce5e1e344b09bc131b04ba15e9f870e23c53490a51b840

exampleConfigBackup :: ConfigBackup
exampleConfigBackup = ConfigBackup
  { cbAccounts = [exampleAccountConfigWithKeysAndName]
  , cbContractNameMap = exampleContractNameMap
  , cbModuleNameMap = exampleModuleNameMap
  }

-- | Json generated by exporting exampleAccountConfigWithKeysAndName with v1 serialisation and no password
unencryptedBackupv1 :: BS.ByteString
unencryptedBackupv1 = "{\"contents\":{\"value\":{\"value\":[[{\"address\":{\"address\":\"2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6\",\"name\":\"name\"},\"accountEncryptionKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"w7pmsDi1K4bWf+zkLCuzVw==\",\"initializationVector\":\"EXhd7ctFeqKvaA0P/oB8wA==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"pYvIywCAMLhvag1EJmGVuVezGsNvYn24zBnB6TCTkwEwOH50AOrx8NAZnVuQteZMQ7k7Kd7a1RorSxIQI1H/WX+Usi8f3VLnzdZFJmbk4Cme+dcgAbI+wWr0hisgrCDl\"},\"threshold\":2,\"accountKeys\":{\"2\":{\"encryptedSignKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"slzkcKo8IPymU5t7jamGQQ==\",\"initializationVector\":\"NXbbI8Cc3AXtaG/go+L+FA==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"hV5NemYi36f3erxCE8sC/uUdHKe1+2OrP3JVYVtBeUqn3QrOm8dlJcAd4mk7ufogJVyv0OR56w/oKqQ7HG8/UycDYtBlubGRHE0Ym4LCoqY=\"},\"verifyKey\":\"f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213\",\"schemeId\":\"Ed25519\"},\"11\":{\"encryptedSignKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"sQ8NG/fBLdLuuLd1ARlAqw==\",\"initializationVector\":\"z6tTcT5ko8vS2utlwwNvbw==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"9ltKSJtlkiBXY/kU8huA4GoCaGNjy8M2Ym2SOtlg1ay6lfI9o95sXJ1cjcQ2b8gV+WddwS7ile8ZhIr8es58pTaM8PczlLbKBCSJ11R2iqw=\"},\"verifyKey\":\"c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027\",\"schemeId\":\"Ed25519\"}}}],{\"contrA\":{\"subindex\":0,\"index\":0},\"contrB\":{\"subindex\":0,\"index\":42},\"contrC\":{\"subindex\":4200,\"index\":42}},{\"modB\":\"3bdc9752a50026c173ce5e1e344b09bc131b04ba15e9f870e23c53490a51b840\",\"modA\":\"de0cd794099a5e03c2131d662d423164111d3b78d5122970197cd7e1937ed0e4\"}],\"v\":1},\"v\":1},\"type\":\"unencrypted\"}"

-- | Json generated by exporting exampleAccountConfigWithKeysAndName with v1 serialisation and password "testpassword"
encryptedBackupv1 :: BS.ByteString
encryptedBackupv1 = "{\"contents\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"0hek4Ks5NlXLnVwmjrQI2Q==\",\"initializationVector\":\"JKr0X3zNIxE/IO+GRATg9Q==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"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\"},\"type\":\"encrypted\"}"

testPassword :: Password
testPassword = Password{getPassword = "testpassword"}

testPassword2 :: Password
testPassword2 = Password{getPassword = "anotherTestPassword"}

backupSpec :: Spec
backupSpec = describe "Testing backup import/export" $ do
    specify "JSON decode is inverse of decode for ConfigBackup" $ do
      (AE.eitherDecode . AE.encode $ exampleConfigBackup) `shouldBe` Right exampleConfigBackup
    specify "export then import without password" $ do
        exported <- configExport exampleConfigBackup Nothing
        x' <- configImport exported (return testPassword)
        x' `shouldBe` (Right exampleConfigBackup)
    specify "export then import with password" $ do
        exported <- configExport exampleConfigBackup $ Just testPassword
        x' <- configImport exported $ return testPassword
        x' `shouldBe` (Right exampleConfigBackup)
    specify "import unencrypted backup with version 1" $ do
        -- Tests that we can import a configbackup with v1.0 serialisation
        x' <- configImport unencryptedBackupv1 (return testPassword)
        x' `shouldBe` (Right exampleConfigBackup)
    specify "import encrypted backup with version 1" $ do
        -- Tests that we can import an encrypted configbackup with v1.0 serialisation
        x' <- configImport encryptedBackupv1 (return testPassword)
        x' `shouldBe` (Right exampleConfigBackup)
