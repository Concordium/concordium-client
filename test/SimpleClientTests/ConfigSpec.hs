module SimpleClientTests.ConfigSpec where

import Concordium.Client.Cli
import Concordium.Client.Config
import Concordium.Client.Output
import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.Crypto.SignatureScheme as S
import qualified Concordium.ID.Types as IDTypes

import Control.Monad.Writer
import qualified Data.HashMap.Strict as M
import Data.Text
import Test.Hspec
import Text.Printf
import System.FilePath ((</>))

configSpec :: Spec
configSpec = describe "config" $ do
  parseAccountNameMapEntrySpec
  parseAccountNameMapSpec
  resolveAccountAddressSpec
  loadKeyMapSpec
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
  specify "invalid name" $
    p (printf "na me=%s" s)`shouldBe` Left "invalid name 'na me' (should consist of letters, numbers, '-', and '_' only)"
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
    parseAccountNameMap ["n@me = " ++ s1] `shouldBe` Left "invalid name 'n@me' (should consist of letters, numbers, '-', and '_' only)"
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

loadKeyMapSpec :: Spec
loadKeyMapSpec = describe "loadKeyMap" $ do
  rawKeysFromFilesSpec
  insertRawKeySpec
  keyMapFromRawSpec

rawKeysFromFilesSpec :: Spec
rawKeysFromFilesSpec = describe "rawKeysFromFilesSpec" $ do
  specify "empty" $
    rawKeysFromFiles "dir" [] `shouldBe` []
  specify "sign and verify files are included, others are skipped" $
    rawKeysFromFiles "dir" ["0.sign", "0.verify", "sign", "1.verify"] `shouldBe`
    [("dir" </> "0.sign", ("0", Sign)), ("dir" </> "0.verify", ("0", Verify)), ("dir" </> "1.verify", ("1", Verify))]

insertRawKeySpec :: Spec
insertRawKeySpec = describe "insertRawKey" $ do
  specify "set sign key" $
    insertRawKey ("0", Sign) "key" M.empty `shouldBe` M.fromList [("0", (Just "key", Nothing))]
  specify "set verify key" $
    insertRawKey ("0", Verify) "key" M.empty `shouldBe` M.fromList [("0", (Nothing, Just "key"))]
  specify "add sign key" $
    insertRawKey ("0", Sign) "key" (M.fromList [("0", (Nothing, Just "verify-key"))]) `shouldBe` M.fromList [("0", (Just "key", Just "verify-key"))]
  specify "add verify key" $
    insertRawKey ("0", Verify) "key" (M.fromList [("0", (Just "sign-key", Nothing))]) `shouldBe` M.fromList [("0", (Just "sign-key", Just "key"))]
  specify "overwrite sign key" $
    insertRawKey ("0", Sign) "new-key" (M.fromList [("0", (Just "old-key", Nothing))]) `shouldBe` M.fromList [("0", (Just "new-key", Nothing))]
  specify "overwrite verify key" $
    insertRawKey ("0", Verify) "new-key" (M.fromList [("0", (Nothing, Just "old-key"))]) `shouldBe` M.fromList [("0", (Nothing, Just "new-key"))]

keyMapFromRawSpec :: Spec
keyMapFromRawSpec = describe "keyMapFromRaw" $ do
  specify "empty" $ keyMapFromRaw M.empty `shouldBe` Right M.empty
  specify "key with both sign and verify parts are included" $
    let rawMap = M.fromList [("0", (Just s1, Just v1))]
        want = M.fromList [(0, S.KeyPairEd25519 { S.signKey=sk1, S.verifyKey=vk1 })]
    in keyMapFromRaw rawMap `shouldBe` Right want
  specify "key with only verify part fails" $
    keyMapFromRaw (M.fromList [("0", (Nothing, Just v1))]) `shouldBe` Left "incomplete key pair '0': sign key is missing"
  specify "key with only sign part is skipped" $
    keyMapFromRaw (M.fromList [("0", (Just s1, Nothing))]) `shouldBe` Left "incomplete key pair '0': verify key is missing"
  specify "key with no parts fails" $
    keyMapFromRaw (M.fromList [("0", (Nothing, Nothing))]) `shouldBe` Left "missing key pair '0'"
  specify "multiple keys" $
    let rawMap = M.fromList [("0", (Just s1, Just v1)), ("2", (Just s2, Just v2))]
        want = M.fromList [(0, S.KeyPairEd25519 { S.signKey=sk1, S.verifyKey=vk1 }), (2, S.KeyPairEd25519 { S.signKey=sk2, S.verifyKey=vk2 })]
    in keyMapFromRaw rawMap `shouldBe` Right want
  specify "complete key pair with non-int name fails" $
    keyMapFromRaw (M.fromList [("invalid", (Just s1, Just v1))]) `shouldBe` Left "invalid key index 'invalid'"
  specify "incomplete key pair with invalid name fails (sign key missing)" $
    keyMapFromRaw (M.fromList [("invalid", (Nothing, Just v1))]) `shouldBe` Left "incomplete key pair 'invalid': sign key is missing"
  specify "incomplete key pair with invalid name fails (verify key missing)" $
    keyMapFromRaw (M.fromList [("invalid", (Just s1, Nothing))]) `shouldBe` Left "incomplete key pair 'invalid': verify key is missing"
  specify "key pair with invalid sign key fails" $
    keyMapFromRaw (M.fromList [("0", (Just "invalid", Just v1))]) `shouldBe` Left "invalid sign key 'invalid' (should be base-16 string of length 64)"
  specify "key pair with invalid verify key fails" $
    keyMapFromRaw (M.fromList [("0", (Just s1, Just "invalid"))]) `shouldBe` Left "invalid verify key 'invalid' (should be base-16 string of length 64)"
  where s1 = "6d00a10ccac23d2fd0bea163756487288fd19ff3810e1d3f73b686e60d801915"
        v1 = "c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027"
        s2 = "9b301aa72d991d720750935de632983f1854d701ada3e5b763215d0802d5541c"
        v2 = "f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213"
        (Just sk1) = BSH.deserializeBase16 s1
        (Just vk1) = BSH.deserializeBase16 v1
        (Just sk2) = BSH.deserializeBase16 s2
        (Just vk2) = BSH.deserializeBase16 v2

printSpec :: Spec
printSpec = describe "print" $ do
  printBaseConfigSpec
  printAccountConfigSpec
  printAccountConfigListSpec

printBaseConfigSpec :: Spec
printBaseConfigSpec = describe "base config" $ do
  specify "with map" $ p exampleBaseConfigWithAccountNameMap `shouldBe`
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
  specify "with keys and name" $ p exampleAccountConfigWithKeysAndName `shouldBe`
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
  specify "non-empty" $ p [exampleAccountConfigWithKeysAndName, exampleAccountConfigWithoutKeysAndName] `shouldBe`
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
  , acKeys = M.fromList [ (11, S.KeyPairEd25519 { S.signKey=sk1, S.verifyKey=vk1 })
                        , (2, S.KeyPairEd25519 { S.signKey=sk2, S.verifyKey=vk2 }) ]
  , acThreshold = 2}
  where s1 = "6d00a10ccac23d2fd0bea163756487288fd19ff3810e1d3f73b686e60d801915"
        v1 = "c825d0ada6ebedcdf58b78cf4bc2dccc98c67ea0b0df6757f15c2b639e09f027"
        s2 = "9b301aa72d991d720750935de632983f1854d701ada3e5b763215d0802d5541c"
        v2 = "f489ebb6bec1f44ca1add277482c1a24d42173f2dd2e1ba9e79ed0ec5f76f213"
        (Just sk1) = BSH.deserializeBase16 s1
        (Just vk1) = BSH.deserializeBase16 v1
        (Just sk2) = BSH.deserializeBase16 s2
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
