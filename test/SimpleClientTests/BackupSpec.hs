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
  { acAddr = NamedAddress { naNames = ["name"] , naAddr = exampleAccountAddress1 }
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
unencryptedBackupv1 = "{\"contents\":{\"value\":{\"value\":[[{\"address\":{\"address\":\"2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6\",\"names\":[\"name\"]},\"accountEncryptionKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"w7pmsDi1K4bWf+zkLCuzVw==\",\"initializationVector\":\"EXhd7ctFeqKvaA0P/oB8wA==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"pYvIywCAMLhvag1EJmGVuVezGsNvYn24zBnB6TCTkwEwOH50AOrx8NAZnVuQteZMQ7k7Kd7a1RorSxIQI1H/WX+Usi8f3VLnzdZFJmbk4Cme+dcgAbI+wWr0hisgrCDl\"},\"threshold\":2,\"accountKeys\":{\"2\":{\"encryptedSignKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"slzkcKo8IPymU5t7jamGQQ==\",\"initializationVector\":\"NXbbI8Cc3AXtaG/go+L+FA==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"hV5NemYi36f3erxCE8sC/uUdHKe1+2OrP3JVYVtBeUqn3QrOm8dlJcAd4mk7ufogJVyv0OR56w/oKqQ7HG8/UycDYtBlubGRHE0Ym4LCoqY=\"},\"verifyKey\":\"f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213\",\"schemeId\":\"Ed25519\"},\"11\":{\"encryptedSignKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"sQ8NG/fBLdLuuLd1ARlAqw==\",\"initializationVector\":\"z6tTcT5ko8vS2utlwwNvbw==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"9ltKSJtlkiBXY/kU8huA4GoCaGNjy8M2Ym2SOtlg1ay6lfI9o95sXJ1cjcQ2b8gV+WddwS7ile8ZhIr8es58pTaM8PczlLbKBCSJ11R2iqw=\"},\"verifyKey\":\"c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027\",\"schemeId\":\"Ed25519\"}}}],{\"contrA\":{\"subindex\":0,\"index\":0},\"contrB\":{\"subindex\":0,\"index\":42},\"contrC\":{\"subindex\":4200,\"index\":42}},{\"modB\":\"3bdc9752a50026c173ce5e1e344b09bc131b04ba15e9f870e23c53490a51b840\",\"modA\":\"de0cd794099a5e03c2131d662d423164111d3b78d5122970197cd7e1937ed0e4\"}],\"v\":1},\"v\":1},\"type\":\"unencrypted\"}"

-- | Json generated by exporting exampleAccountConfigWithKeysAndName with v1 serialisation and password "testpassword"
encryptedBackupv1 :: BS.ByteString
encryptedBackupv1 = "{\"contents\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"br9zqwaapkpO1ycZ8td6mg==\",\"initializationVector\":\"oI5k2f+dWuGMKJu2gw7pXw==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"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\"},\"type\":\"encrypted\"}"

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
