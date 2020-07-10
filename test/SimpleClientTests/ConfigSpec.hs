module SimpleClientTests.ConfigSpec where

import Concordium.Client.Cli
import Concordium.Client.Config
import Concordium.Client.Output
import Concordium.Client.Encryption
import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.Crypto.SignatureScheme as S
import qualified Concordium.ID.Types as IDTypes
import Concordium.Client.Types.Account

import Control.Monad.Writer
import qualified Data.HashMap.Strict as M
import Data.Text
import Test.Hspec
import Text.Printf
import System.FilePath ((</>))


testPassword :: String
testPassword = "helloworld"

configSpec :: Spec
configSpec = describe "config" $ do
  parseAccountNameMapEntrySpec
  parseAccountNameMapSpec
  resolveAccountAddressSpec
  printSpec

parseAccountNameMapEntrySpec :: Spec
parseAccountNameMapEntrySpec = describe "parseAccountNameEntryMap" $ do
  specify "without whitespace" $
    p (printf "name=%s" s) `shouldBe` Right ("name", a)
  specify "with whitespace" $
    p (printf " name = %s " s) `shouldBe` Right ("name", a)
  specify "missing name" $
    p (printf "=%s" s) `shouldBe` Left "empty name"
  specify "missing address" $
    p "name=" `shouldBe` Left "empty address"
  specify "name with whitespace" $
    p (printf "na me = %s " s) `shouldBe` Right ("na me", a)
  specify "invalid name" $
    p (printf "n@me=%s" s)`shouldBe` Left "invalid name 'n@me' (should consist of letters, numbers, space, '.', ',', '!', '?', '-', and '_' only)"
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
        (Right a) = IDTypes.addressFromText s

parseAccountNameMapSpec :: Spec
parseAccountNameMapSpec = describe "parseAccountNameMap" $ do
  specify "empty" $
    parseAccountNameMap [] `shouldBe` Right M.empty
  specify "two lines separated by blank" $
    let input = [ ""
                , "first= " ++ s1
                , " \t "
                , "second =" ++ s2 ]
        want = M.fromList [("first", a1), ("second", a2)]
    in parseAccountNameMap input `shouldBe` Right want
  specify "invalid format" $
    parseAccountNameMap ["invalid"] `shouldBe` Left "invalid mapping format 'invalid' (should be '<name> = <address>')"
  specify "invalid name" $
    parseAccountNameMap ["n@me = " ++ s1] `shouldBe` Left "invalid name 'n@me' (should consist of letters, numbers, space, '.', ',', '!', '?', '-', and '_' only)"
  where s1 = "35FtQ8HgRShXLGUer7k8wtovjKAcSQ2Ys8RQPx27KfRA7zf7i4"
        s2 = "4RDhNeQB7DUKcKNStBQfLjU6y32HYDMxsJef2ATVncKRYJWoCV"
        (Right a1) = IDTypes.addressFromText $ pack s1
        (Right a2) = IDTypes.addressFromText $ pack s2

resolveAccountAddressSpec :: Spec
resolveAccountAddressSpec = describe "resolveAccountAddress" $ do
  specify "valid account address" $
    r M.empty s1 `shouldBe` Just NamedAddress { naName = Nothing, naAddr = a1 }
  specify "valid account address is not looked up in map" $
    r (M.fromList [(s1, a2)]) s1 `shouldBe` Just NamedAddress { naName = Nothing, naAddr = a1 }
  specify "existing account name" $
    r (M.fromList [("name", a1)]) "name" `shouldBe` Just NamedAddress { naName = Just "name", naAddr = a1 }
  specify "nonexisting account name" $
    r M.empty "name" `shouldBe` Nothing
  where r = resolveAccountAddress
        s1 = "35FtQ8HgRShXLGUer7k8wtovjKAcSQ2Ys8RQPx27KfRA7zf7i4"
        s2 = "4RDhNeQB7DUKcKNStBQfLjU6y32HYDMxsJef2ATVncKRYJWoCV"
        (Right a1) = IDTypes.addressFromText s1
        (Right a2) = IDTypes.addressFromText s2

printSpec :: Spec
printSpec = describe "print" $ do
  printBaseConfigSpec
  printAccountConfigSpec
  printAccountConfigListSpec

printBaseConfigSpec :: Spec
printBaseConfigSpec = describe "base config" $ do
  -- TODO Update to new format when the format of this and other commands has been decided.
  xspecify "with map" $ p exampleBaseConfigWithAccountNameMap `shouldBe`
    [ "Base configuration:"
    , "- Verbose:            yes"
    , "- Account config dir: /some/path"
    , "- Account name map:"
    , "    name1 -> 2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
    , "    name2 -> 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y" ]
  specify "without map" $ p exampleBaseConfigWithoutAccountNameMap `shouldBe`
    [ "Base configuration:"
    , "- Verbose:            no"
    , "- Account config dir: /some/other/path"
    , "- Account name map:   none" ]
  where p = execWriter . printBaseConfig

printAccountConfigSpec :: Spec
printAccountConfigSpec = describe "account config" $ do
  -- TODO Update to new format when the format of this and other commands has been decided.
  xspecify "with keys and name" $ p exampleAccountConfigWithKeysAndName `shouldBe`
    [ "Account configuration:"
    , "- Name:    name"
    , "- Address: 2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"
      , "- Keys:"
    , "    2: sign=9b301aa72d991d720750935de632983f1854d701ada3e5b763215d0802d5541c, verify=f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213"
    , "    11: sign=6d00a10ccac23d2fd0bea163756487288fd19ff3810e1d3f73b686e60d801915, verify=c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027" ]
  specify "without keys and name" $ p exampleAccountConfigWithoutKeysAndName `shouldBe`
    [ "Account configuration:"
    , "- Name:    none"
    , "- Address: 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y"
    , "- Keys:    none" ]
  where p = execWriter . printAccountConfig

printAccountConfigListSpec :: Spec
printAccountConfigListSpec = describe "all account config" $ do
  specify "empty" $ p [] `shouldBe`
    [ "Account keys: none" ]
  -- TODO Update to new format when the format of this and other commands has been decided.
  xspecify "non-empty" $ p [exampleAccountConfigWithKeysAndName, exampleAccountConfigWithoutKeysAndName] `shouldBe`
    [ "Account keys:"
    , "- '2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6' (name):"
    , "    2: sign=9b301aa72d991d720750935de632983f1854d701ada3e5b763215d0802d5541c, verify=f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213"
    , "    11: sign=6d00a10ccac23d2fd0bea163756487288fd19ff3810e1d3f73b686e60d801915, verify=c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027"
    , "- '4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y': none" ]
  where p = execWriter . printAccountConfigList

exampleBaseConfigWithAccountNameMap :: BaseConfig
exampleBaseConfigWithAccountNameMap =
  BaseConfig
  { bcVerbose = True
  , bcAccountNameMap = M.fromList [("name1", exampleAccountAddress1), ("name2", exampleAccountAddress2)]
  , bcAccountCfgDir = "/some/path" }

exampleBaseConfigWithoutAccountNameMap :: BaseConfig
exampleBaseConfigWithoutAccountNameMap =
  BaseConfig
  { bcVerbose = False
  , bcAccountNameMap = M.empty
  , bcAccountCfgDir = "/some/other/path"}

exampleAccountConfigWithKeysAndName :: AccountConfig
exampleAccountConfigWithKeysAndName =
  AccountConfig
  { acAddr = NamedAddress { naName = Just "name" , naAddr = exampleAccountAddress1 }
  -- NOTE The following 'EncryptedAccountKeyPair's are generated with executing genEncryptedAccountKeyPairs.ghci in GHCI.
  , acKeys = M.fromList [ (11, EncryptedAccountKeyPair { verifyKey=vk1
                                                       , encryptedSignKey = EncryptedJSON (EncryptedText {etMetadata = EncryptionMetadata {emEncryptionMethod = AES256, emKeyDerivationMethod = PBKDF2SHA256, emIterations = 100000, emSalt = "sQ8NG/fBLdLuuLd1ARlAqw==", emInitializationVector = "z6tTcT5ko8vS2utlwwNvbw=="}, etCipherText = "9ltKSJtlkiBXY/kU8huA4GoCaGNjy8M2Ym2SOtlg1ay6lfI9o95sXJ1cjcQ2b8gV+WddwS7ile8ZhIr8es58pTaM8PczlLbKBCSJ11R2iqw="}) })
                        , ( 2, EncryptedAccountKeyPair { verifyKey=vk2
                                                       , encryptedSignKey = EncryptedJSON (EncryptedText {etMetadata = EncryptionMetadata {emEncryptionMethod = AES256, emKeyDerivationMethod = PBKDF2SHA256, emIterations = 100000, emSalt = "slzkcKo8IPymU5t7jamGQQ==", emInitializationVector = "NXbbI8Cc3AXtaG/go+L+FA=="}, etCipherText = "hV5NemYi36f3erxCE8sC/uUdHKe1+2OrP3JVYVtBeUqn3QrOm8dlJcAd4mk7ufogJVyv0OR56w/oKqQ7HG8/UycDYtBlubGRHE0Ym4LCoqY="})}) ]
  , acThreshold = 2}
  where s1 = "6d00a10ccac23d2fd0bea163756487288fd19ff3810e1d3f73b686e60d801915"
        v1 = "c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027"
        s2 = "9b301aa72d991d720750935de632983f1854d701ada3e5b763215d0802d5541c"
        v2 = "f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213"
        -- (Just sk1) = BSH.deserializeBase16 s1
        (Just vk1) = BSH.deserializeBase16 v1
        -- (Just sk2) = BSH.deserializeBase16 s2
        (Just vk2) = BSH.deserializeBase16 v2

exampleAccountConfigWithoutKeysAndName :: AccountConfig
exampleAccountConfigWithoutKeysAndName =
  AccountConfig
  { acAddr = NamedAddress { naName = Nothing, naAddr = exampleAccountAddress2}
  , acKeys = M.empty
  , acThreshold = 1}

exampleAccountAddress1 :: IDTypes.AccountAddress
Right exampleAccountAddress1 = IDTypes.addressFromText "2zR4h351M1bqhrL9UywsbHrP3ucA1xY3TBTFRuTsRout8JnLD6"

exampleAccountAddress2 :: IDTypes.AccountAddress
Right exampleAccountAddress2 = IDTypes.addressFromText "4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y"
