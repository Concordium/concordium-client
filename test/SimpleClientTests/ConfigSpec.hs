module SimpleClientTests.ConfigSpec where

import Concordium.Client.Config
import Concordium.Client.Output
import Concordium.Client.Types.Account
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Data.ByteString.Base64 as BS64
import qualified Concordium.Types as Types
import Concordium.Types.HashableTo (getHash)
import Concordium.Utils.Encryption

import Control.Monad.Writer
import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text
import Test.Hspec
import Text.Printf

fromBase64 :: BS.ByteString -> Base64ByteString
fromBase64 bs =
  case BS64.decode bs of
    Right x -> Base64ByteString x
    Left e -> error $ "Error decoding base64 string: " ++ e

testPassword :: String
testPassword = "helloworld"

configSpec :: Spec
configSpec = describe "config" $ do
  parseAccountNameMapEntrySpec
  parseAccountNameMapSpec
  resolveAccountAddressSpec
  printSpec

invalidNameMsg :: String -> String
invalidNameMsg n = "invalid name '" ++ n ++ "' (should not be empty or start/end with whitespace and consist of letters, numbers, space, '.', ',', '!', '?', '-', and '_' only)"

parseAccountNameMapEntrySpec :: Spec
parseAccountNameMapEntrySpec = describe "parseAccountNameEntryMap" $ do
  specify "without whitespace" $
    p (printf "name=%s" s) `shouldBe` Right ("name", a)
  specify "with whitespace" $
    p (printf " name = %s " s) `shouldBe` Right ("name", a)
  specify "missing name" $
    p (printf "=%s" s) `shouldBe` Left (invalidNameMsg "")
  specify "missing address" $
    p "name=" `shouldBe` Left "empty address"
  specify "name with whitespace" $
    p (printf "na me = %s " s) `shouldBe` Right ("na me", a)
  specify "invalid name" $
    p (printf "n@me=%s" s)`shouldBe` Left (invalidNameMsg "n@me")
  specify "invalid address" $
    p "name=1234" `shouldBe` Left "invalid address '1234': Base 58 checksum invalid."
  specify "empty" $
    p "" `shouldBe` Left "invalid mapping format '' (should be '<name> = <address>')"
  specify "no separators" $
    p "name" `shouldBe` Left "invalid mapping format 'name' (should be '<name> = <address>')"
  specify "two separators" $
    let input = printf "name1=name2=%s" s
    in p input `shouldBe` (Left $ printf "invalid mapping format '%s' (should be '<name> = <address>')" input)
  where p = parseAccountNameMapEntry
        s = "35FtQ8HgRShXLGUer7k8wtovjKAcSQ2Ys8RQPx27KfRA7zf7i4"
        a = case IDTypes.addressFromText s of
              Right addr -> addr
              -- This does not happen since the format
              -- of the text is that of an account name
              -- map entry.
              Left err -> error err

parseAccountNameMapSpec :: Spec
parseAccountNameMapSpec = describe "parseAccountNameMap" $ do
  specify "empty" $
    parseAccountNameMap [] `shouldBe` Right Map.empty
  specify "two lines separated by blank" $
    let input = [ ""
                , "first= " ++ s1
                , " \t "
                , "second =" ++ s2 ]
        want = Map.fromList [("first", a1), ("second", a2)]
    in parseAccountNameMap input `shouldBe` Right want
  specify "invalid format" $
    parseAccountNameMap ["invalid"] `shouldBe` Left "invalid mapping format 'invalid' (should be '<name> = <address>')"
  specify "invalid name" $
    parseAccountNameMap ["n@me = " ++ s1] `shouldBe` Left (invalidNameMsg "n@me")
  where s1 = "35FtQ8HgRShXLGUer7k8wtovjKAcSQ2Ys8RQPx27KfRA7zf7i4"
        s2 = "4RDhNeQB7DUKcKNStBQfLjU6y32HYDMxsJef2ATVncKRYJWoCV"
        a1 = case IDTypes.addressFromText $ pack s1 of
          Right addr -> addr
          -- This does not happen since the format
          -- of the text is that of a valid address.
          Left err -> error err
        a2 = case IDTypes.addressFromText $ pack s2 of
          Right addr -> addr
          -- This does not happen since the format
          -- of the text is that of a valid address.
          Left err -> error err

resolveAccountAddressSpec :: Spec
resolveAccountAddressSpec = describe "resolveAccountAddress" $ do
  specify "valid account address" $
    r Map.empty s1 `shouldBe` Just NamedAddress { naNames = [], naAddr = a1 }
  specify "valid account address is not looked up in map" $
    r (Map.fromList [(s1, a2)]) s1 `shouldBe` Just NamedAddress { naNames = [], naAddr = a1 }
  specify "existing account name" $
    r (Map.fromList [("name", a1)]) "name" `shouldBe` Just NamedAddress { naNames = ["name"], naAddr = a1 }
  specify "nonexisting account name" $
    r Map.empty "name" `shouldBe` Nothing
  where r = resolveAccountAddress
        s1 = "35FtQ8HgRShXLGUer7k8wtovjKAcSQ2Ys8RQPx27KfRA7zf7i4"
        s2 = "4RDhNeQB7DUKcKNStBQfLjU6y32HYDMxsJef2ATVncKRYJWoCV"
        a1 = case IDTypes.addressFromText s1 of
          Right addr -> addr
          -- This does not happen since the format
          -- of the text is that of a valid address.
          Left err -> error err
        a2 = case IDTypes.addressFromText s2 of
          Right addr -> addr
          -- This does not happen since the format
          -- of the text is that of a valid address.
          Left err -> error err

printSpec :: Spec
printSpec = describe "print" $ do
  printBaseConfigSpec
  printAccountConfigSpec

printBaseConfigSpec :: Spec
printBaseConfigSpec = describe "base config" $ do
  -- TODO Update to new format when the format of this and other commands has been decided.
  specify "with map" $ p exampleBaseConfigWithAccountNameMap `shouldBe`
    [ "Base configuration:"
    , "- Verbose:            yes"
    , "- Account config dir: /some/path"
    , "- Contract config dir: /some/path"
    , "- Account name map:"
    , "    accName1 -> 2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "    accName2 -> 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y"
    , "- Contract name map:"
    , "    contrName1 -> {\"index\":0,\"subindex\":0}"
    , "    contrName2 -> {\"index\":1,\"subindex\":0}"
    , "- Module name map:"
    , "    modName1 -> de0cd794099a5e03c2131d662d423164111d3b78d5122970197cd7e1937ed0e4"
    , "    modName2 -> 3bdc9752a50026c173ce5e1e344b09bc131b04ba15e9f870e23c53490a51b840"]
  specify "without map" $ p exampleBaseConfigWithoutNameMaps `shouldBe`
    [ "Base configuration:"
    , "- Verbose:            no"
    , "- Account config dir: /some/other/path"
    , "- Contract config dir: /some/other/path"
    , "- Account name map:   none"
    , "- Contract name map:   none"
    , "- Module name map:   none" ]
  where p = execWriter . printBaseConfig

printAccountConfigSpec :: Spec
printAccountConfigSpec = describe "account config" $ do
  -- TODO Update to new format when the format of this and other commands has been decided.
  specify "with keys and name" $ p exampleSelectedKeyConfigWithKeysAndName `shouldBe`
    [ "Account configuration:"
    , "- Names:   'name'"
    , "- Address: 2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "- Credentials keys:"
    , "   - Keys for credential with index 0"
    , "      2: {\n    \"encryptedSignKey\": {\n        \"cipherText\": \"hV5NemYi36f3erxCE8sC/uUdHKe1+2OrP3JVYVtBeUqn3QrOm8dlJcAd4mk7ufogJVyv0OR56w/oKqQ7HG8/UycDYtBlubGRHE0Ym4LCoqY=\",\n        \"metadata\": {\n            \"encryptionMethod\": \"AES-256\",\n            \"initializationVector\": \"NXbbI8Cc3AXtaG/go+L+FA==\",\n            \"iterations\": 100000,\n            \"keyDerivationMethod\": \"PBKDF2WithHmacSHA256\",\n            \"salt\": \"slzkcKo8IPymU5t7jamGQQ==\"\n        }\n    },\n    \"schemeId\": \"Ed25519\",\n    \"verifyKey\": \"f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213\"\n}"
    , "      11: {\n    \"encryptedSignKey\": {\n        \"cipherText\": \"9ltKSJtlkiBXY/kU8huA4GoCaGNjy8M2Ym2SOtlg1ay6lfI9o95sXJ1cjcQ2b8gV+WddwS7ile8ZhIr8es58pTaM8PczlLbKBCSJ11R2iqw=\",\n        \"metadata\": {\n            \"encryptionMethod\": \"AES-256\",\n            \"initializationVector\": \"z6tTcT5ko8vS2utlwwNvbw==\",\n            \"iterations\": 100000,\n            \"keyDerivationMethod\": \"PBKDF2WithHmacSHA256\",\n            \"salt\": \"sQ8NG/fBLdLuuLd1ARlAqw==\"\n        }\n    },\n    \"schemeId\": \"Ed25519\",\n    \"verifyKey\": \"c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027\"\n}"]
  specify "without keys and name" $ p exampleAccountConfigWithoutKeysAndName `shouldBe`
    [ "Account configuration:"
    , "- Names:   none"
    , "- Address: 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y"
    , "- Credentials keys:    none" ]
  where p = execWriter . printSelectedKeyConfig

exampleBaseConfigWithAccountNameMap :: BaseConfig
exampleBaseConfigWithAccountNameMap =
  BaseConfig
  { bcVerbose = True
  , bcAccountNameMap = Map.fromList [("accName1", exampleAccountAddress1), ("accName2", exampleAccountAddress2)]
  , bcAccountCfgDir = "/some/path"
  , bcContractNameMap = Map.fromList [("contrName1", exampleContractAddress1), ("contrName2", exampleContractAddress2)]
  , bcModuleNameMap = Map.fromList [("modName1", exampleModuleRef1), ("modName2", exampleModuleRef2)]
  , bcContractCfgDir = "/some/path"}

exampleBaseConfigWithoutNameMaps :: BaseConfig
exampleBaseConfigWithoutNameMaps =
  BaseConfig
  { bcVerbose = False
  , bcAccountNameMap = Map.empty
  , bcAccountCfgDir = "/some/other/path"
  , bcContractNameMap = Map.empty
  , bcModuleNameMap = Map.empty
  , bcContractCfgDir = "/some/other/path"}


dummyEncryptionSecretKey :: ElgamalSecretKey
dummyEncryptionSecretKey = fromJust $ AE.decode "\"a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd521271365fdd517916086b19e5cdc337659274b1149ab59fa780aa76f3bad2f74\""
dummyEncryptionPublicKey :: IDTypes.AccountEncryptionKey
dummyEncryptionPublicKey = fromJust $ AE.decode "\"a820662531d0aac70b3a80dd8a249aa692436097d06da005aec7c56aad17997ec8331d1e4050fd8dced2b92f06277bd5aae71cf315a6d70c849508f6361ac6d51c2168305dd1604c4c6448da4499b2f14afb94fff0f42b79a68ed7ba206301f4\""

exampleSelectedKeyConfigWithKeysAndName :: EncryptedSigningData
exampleSelectedKeyConfigWithKeysAndName =
  EncryptedSigningData
  { esdAddress = NamedAddress { naNames = ["name"] , naAddr = exampleAccountAddress1 }
  -- TODO Generate testdata instead of hard-coding (generate key pairs, encrypt).
  -- Test keypairs can either be generated with
  -- randomEd25519KeyPair from Concordium.Crypto.DummyData if determinism is required, or
  -- with newKeyPair from Concordium.Crypto.SignatureScheme if determinism is not important.
  , esdKeys = Map.singleton 0 $ Map.fromList [ (11,
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
  , esdEncryptionKey = Just EncryptedText {
      etMetadata = EncryptionMetadata {
          emEncryptionMethod = AES256,
          emIterations = 100000,
          emSalt = fromBase64 "w7pmsDi1K4bWf+zkLCuzVw==",
          emInitializationVector = fromBase64 "EXhd7ctFeqKvaA0P/oB8wA==",
          emKeyDerivationMethod = PBKDF2SHA256
          },
      etCipherText = fromBase64 "pYvIywCAMLhvag1EJmGVuVezGsNvYn24zBnB6TCTkwEwOH50AOrx8NAZnVuQteZMQ7k7Kd7a1RorSxIQI1H/WX+Usi8f3VLnzdZFJmbk4Cme+dcgAbI+wWr0hisgrCDl"
      }}
  where
        v1 = "c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027"
        v2 = "f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213"
        (vk1, vk2) = case (BSH.deserializeBase16 v1, BSH.deserializeBase16 v2) of
                       (Just v', Just v'') -> (v',v'')
                       -- This does not happen since the formats of
                       -- v1 and v2 are base 16 strings and hence
                       -- they are always deserialized.
                       _ -> error "unable to deserialize"

exampleAccountConfigWithoutKeysAndName :: EncryptedSigningData
exampleAccountConfigWithoutKeysAndName =
  EncryptedSigningData
  { esdAddress = NamedAddress { naNames = [], naAddr = exampleAccountAddress2}
  , esdKeys = Map.empty
  , esdEncryptionKey = Nothing}

exampleAccountAddress1 :: IDTypes.AccountAddress
exampleAccountAddress1 = case IDTypes.addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6" of
  Right addr -> addr
  Left err -> error err

exampleAccountAddress2 :: IDTypes.AccountAddress
exampleAccountAddress2 = case IDTypes.addressFromText "4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y" of
  Right addr -> addr
  Left err -> error err

exampleContractAddress1 :: Types.ContractAddress
exampleContractAddress1 = Types.ContractAddress (Types.ContractIndex 0) (Types.ContractSubindex 0)

exampleContractAddress2 :: Types.ContractAddress
exampleContractAddress2 = Types.ContractAddress (Types.ContractIndex 1) (Types.ContractSubindex 0)

exampleModuleRef1 :: Types.ModuleRef
exampleModuleRef1 = Types.ModuleRef $ getHash ("ref1" :: BS.ByteString) -- Hash: de0cd794099a5e03c2131d662d423164111d3b78d5122970197cd7e1937ed0e4

exampleModuleRef2 :: Types.ModuleRef
exampleModuleRef2 = Types.ModuleRef $ getHash ("ref2" :: BS.ByteString) -- Hash: 3bdc9752a50026c173ce5e1e344b09bc131b04ba15e9f870e23c53490a51b840
