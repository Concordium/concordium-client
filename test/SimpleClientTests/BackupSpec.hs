{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleClientTests.BackupSpec where

import Concordium.Client.Config
import Concordium.Client.Export
import Concordium.Client.Types.Account
import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import Concordium.Types.HashableTo (getHash)
import Concordium.Utils.Encryption

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.Map.Strict as Map
import Test.Hspec

fromBase64 :: BS.ByteString -> Base64ByteString
fromBase64 bs =
  case BS64.decode bs of
    Right x -> Base64ByteString x
    Left e -> error $ "Error decoding base64 string: " ++ e


exampleAccountAddress1 :: IDTypes.AccountAddress
Right exampleAccountAddress1 = IDTypes.addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"

-- some value that has the right format, it does not matter what it is.
someCredId :: IDTypes.CredentialRegistrationID
Just someCredId = AE.decode "\"96f89a557352b0aa7596b12f3ccf4cc5973066e31e2c57a8b9dc096fdcff6dd8967e27a7a6e9d41fcc0d553b62650148\""

-- |dummy accountconfig, for testing export/import
exampleAccountConfigWithKeysAndName :: AccountConfig
exampleAccountConfigWithKeysAndName =
  AccountConfig
  { acAddr = NamedAddress { naNames = ["name"] , naAddr = exampleAccountAddress1 }
  , acCids = Map.singleton 0 someCredId
  , acKeys = Map.singleton 0 $ Map.fromList [ (11,
                           EncryptedAccountKeyPairEd25519 {
                              verifyKey=vk1
                              , encryptedSignKey = EncryptedJSON (EncryptedText {
                                                                     etMetadata = EncryptionMetadata {
                                                                         emEncryptionMethod = AES256,
                                                                           emKeyDerivationMethod = PBKDF2SHA256,
                                                                           emIterations = 100000,
                                                                           emSalt = fromBase64 "sQ8NG/fBLdLuuLd1ARlAqw==",
                                                                           emInitializationVector = fromBase64 "z6tTcT5ko8vS2utlwwNvbw=="},
                                                                       etCipherText = fromBase64 "9ltKSJtlkiBXY/kU8huA4GoCaGNjy8M2Ym2SOtlg1ay6lfI9o95sXJ1cjcQ2b8gV+WddwS7ile8ZhIr8es58pTaM8PczlLbKBCSJ11R2iqw="})
                              })
                        , ( 2,
                            EncryptedAccountKeyPairEd25519 {
                              verifyKey=vk2
                              , encryptedSignKey = EncryptedJSON (EncryptedText {
                                                                     etMetadata = EncryptionMetadata {
                                                                         emEncryptionMethod = AES256,
                                                                         emKeyDerivationMethod = PBKDF2SHA256,
                                                                         emIterations = 100000,
                                                                         emSalt = fromBase64 "slzkcKo8IPymU5t7jamGQQ==",
                                                                         emInitializationVector = fromBase64 "NXbbI8Cc3AXtaG/go+L+FA=="},
                                                                     etCipherText = fromBase64 "hV5NemYi36f3erxCE8sC/uUdHKe1+2OrP3JVYVtBeUqn3QrOm8dlJcAd4mk7ufogJVyv0OR56w/oKqQ7HG8/UycDYtBlubGRHE0Ym4LCoqY="})
                              })]
  , acEncryptionKey = Just EncryptedText {
      etMetadata = EncryptionMetadata {
          emEncryptionMethod = AES256,
          emIterations = 100000,
          emSalt = fromBase64 "w7pmsDi1K4bWf+zkLCuzVw==",
          emInitializationVector = fromBase64 "EXhd7ctFeqKvaA0P/oB8wA==",
          emKeyDerivationMethod = PBKDF2SHA256
          },
      etCipherText = fromBase64 "pYvIywCAMLhvag1EJmGVuVezGsNvYn24zBnB6TCTkwEwOH50AOrx8NAZnVuQteZMQ7k7Kd7a1RorSxIQI1H/WX+Usi8f3VLnzdZFJmbk4Cme+dcgAbI+wWr0hisgrCDl"
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
exampleContractNameMap = Map.fromList [("contrA", mkContrAddr 0 0), ("contrB", mkContrAddr 42 0), ("contrC", mkContrAddr 42 4200)]
  where mkContrAddr index subindex = Types.ContractAddress (Types.ContractIndex index) (Types.ContractSubindex subindex)

exampleModuleNameMap :: ModuleNameMap
exampleModuleNameMap = Map.fromList [("modA", modRef1), ("modB", modRef2)]
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
unencryptedBackupv1 = "{\"contents\":{\"value\":{\"value\":[[{\"credentialIds\":{\"0\":\"96f89a557352b0aa7596b12f3ccf4cc5973066e31e2c57a8b9dc096fdcff6dd8967e27a7a6e9d41fcc0d553b62650148\"},\"address\":{\"address\":\"2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6\",\"names\":[\"name\"]},\"accountEncryptionKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"w7pmsDi1K4bWf+zkLCuzVw==\",\"initializationVector\":\"EXhd7ctFeqKvaA0P/oB8wA==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"pYvIywCAMLhvag1EJmGVuVezGsNvYn24zBnB6TCTkwEwOH50AOrx8NAZnVuQteZMQ7k7Kd7a1RorSxIQI1H/WX+Usi8f3VLnzdZFJmbk4Cme+dcgAbI+wWr0hisgrCDl\"},\"accountKeys\":{\"0\":{\"2\":{\"encryptedSignKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"slzkcKo8IPymU5t7jamGQQ==\",\"initializationVector\":\"NXbbI8Cc3AXtaG/go+L+FA==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"hV5NemYi36f3erxCE8sC/uUdHKe1+2OrP3JVYVtBeUqn3QrOm8dlJcAd4mk7ufogJVyv0OR56w/oKqQ7HG8/UycDYtBlubGRHE0Ym4LCoqY=\"},\"verifyKey\":\"f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213\",\"schemeId\":\"Ed25519\"},\"11\":{\"encryptedSignKey\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"sQ8NG/fBLdLuuLd1ARlAqw==\",\"initializationVector\":\"z6tTcT5ko8vS2utlwwNvbw==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"9ltKSJtlkiBXY/kU8huA4GoCaGNjy8M2Ym2SOtlg1ay6lfI9o95sXJ1cjcQ2b8gV+WddwS7ile8ZhIr8es58pTaM8PczlLbKBCSJ11R2iqw=\"},\"verifyKey\":\"c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027\",\"schemeId\":\"Ed25519\"}}}}],{\"contrA\":{\"subindex\":0,\"index\":0},\"contrB\":{\"subindex\":0,\"index\":42},\"contrC\":{\"subindex\":4200,\"index\":42}},{\"modB\":\"3bdc9752a50026c173ce5e1e344b09bc131b04ba15e9f870e23c53490a51b840\",\"modA\":\"de0cd794099a5e03c2131d662d423164111d3b78d5122970197cd7e1937ed0e4\"}],\"v\":1},\"v\":1},\"type\":\"unencrypted\"}"

-- | Json generated by exporting exampleAccountConfigWithKeysAndName with v1 serialisation and password "testpassword"
encryptedBackupv1 :: BS.ByteString
encryptedBackupv1 = "{\"contents\":{\"metadata\":{\"encryptionMethod\":\"AES-256\",\"iterations\":100000,\"salt\":\"ePetEp61CQ77FWktGvJKEA==\",\"initializationVector\":\"hE5YI/k726AsjgPYUYoFog==\",\"keyDerivationMethod\":\"PBKDF2WithHmacSHA256\"},\"cipherText\":\"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\"},\"type\":\"encrypted\"}"

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
