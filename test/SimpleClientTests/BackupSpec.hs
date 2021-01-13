{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleClientTests.BackupSpec where

import Concordium.Client.Config
import Concordium.Client.Encryption
import Concordium.Client.Export
import Concordium.Client.Types.Account
import Concordium.Common.Version (Versioned(..))
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

versionedExampleConfigBackup :: VersionedConfigBackup
versionedExampleConfigBackup = VersionedConfigBackup (Versioned 1 exampleConfigBackup)

-- | Json generated by exporting exampleAccountConfigWithKeysAndName with v1 serialisation and no password
unencryptedBackupv1 :: BS.ByteString
unencryptedBackupv1 = "{\"contents\":{\"value\":{\"accounts\":[{\"address\":{\"address\":\"2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6\",\"name\":\"name\"},\"accountEncryptionKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"w7pmsDi1K4bWf+zkLCuzVw==\",\"initializationVector\":\"EXhd7ctFeqKvaA0P/oB8wA==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"pYvIywCAMLhvag1EJmGVuVezGsNvYn24zBnB6TCTkwEwOH50AOrx8NAZnVuQteZMQ7k7Kd7a1RorSxIQI1H/WX+Usi8f3VLnzdZFJmbk4Cme+dcgAbI+wWr0hisgrCDl\"},\"threshold\":2,\"accountKeys\":{\"2\":{\"encryptedSignKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"slzkcKo8IPymU5t7jamGQQ==\",\"initializationVector\":\"NXbbI8Cc3AXtaG/go+L+FA==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"hV5NemYi36f3erxCE8sC/uUdHKe1+2OrP3JVYVtBeUqn3QrOm8dlJcAd4mk7ufogJVyv0OR56w/oKqQ7HG8/UycDYtBlubGRHE0Ym4LCoqY=\"},\"verifyKey\":\"f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213\",\"schemeId\":\"Ed25519\"},\"11\":{\"encryptedSignKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"sQ8NG/fBLdLuuLd1ARlAqw==\",\"initializationVector\":\"z6tTcT5ko8vS2utlwwNvbw==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"9ltKSJtlkiBXY/kU8huA4GoCaGNjy8M2Ym2SOtlg1ay6lfI9o95sXJ1cjcQ2b8gV+WddwS7ile8ZhIr8es58pTaM8PczlLbKBCSJ11R2iqw=\"},\"verifyKey\":\"c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027\",\"schemeId\":\"Ed25519\"}}}],\"moduleNameMap\":{\"modB\":\"3bdc9752a50026c173ce5e1e344b09bc131b04ba15e9f870e23c53490a51b840\",\"modA\":\"de0cd794099a5e03c2131d662d423164111d3b78d5122970197cd7e1937ed0e4\"},\"contractNameMap\":{\"contrA\":{\"subindex\":0,\"index\":0},\"contrB\":{\"subindex\":0,\"index\":42},\"contrC\":{\"subindex\":4200,\"index\":42}}},\"v\":1},\"type\":\"unencrypted\"}"

-- | Json generated by exporting exampleAccountConfigWithKeysAndName with v1 serialisation and password "testpassword"
encryptedBackupv1 :: BS.ByteString
encryptedBackupv1 = "{\"contents\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"gf6rfO5+Lt8FmZ07cBGbaQ==\",\"initializationVector\":\"bkybTp04EfiNdNUxd/TcRQ==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"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\"},\"type\":\"encrypted\"}"

testPassword :: Password
testPassword = Password{getPassword = "testpassword"}

testPassword2 :: Password
testPassword2 = Password{getPassword = "anotherTestPassword"}

backupSpec :: Spec
backupSpec = describe "Testing backup import/export" $ do
    specify "JSON decode is inverse of decode for VersionedConfigBackup" $ do
      (AE.eitherDecode . AE.encode $ versionedExampleConfigBackup) `shouldBe` Right versionedExampleConfigBackup
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
