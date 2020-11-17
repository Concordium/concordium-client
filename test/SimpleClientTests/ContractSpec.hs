{-# LANGUAGE NumericUnderscores #-}
module SimpleClientTests.ContractSpec where

import Concordium.Client.Types.Contract

import Data.Aeson (ToJSON, object, (.=))
import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Data.Word (Word8, Word64)
import Test.Hspec
import Test.QuickCheck

contractSpec :: Spec
contractSpec = describe "contract" $ do
  printSchemaSpec
  printInfoSpec
  printParamsSpec

printSchemaSpec :: Spec
printSchemaSpec = describe "serialize Schema" $ do
  it "decodeSchema is inverse of encodeSchema" $ withMaxSuccess 50 $
    forAll (sized genSchema) $ \c -> (decodeSchema . encodeSchema) c === Right c

printInfoSpec :: Spec
printInfoSpec = describe "serialize Info" $ do
  it "fromJSON is inverse of toJSON for Info" $ do
    pending

  it "fromJSON is inverse of toJSON for Model" $ withMaxSuccess 100 $
    forAll genModel $ \m -> (AE.fromJSON . AE.toJSON) m === AE.Success m

printParamsSpec :: Spec
printParamsSpec = describe "serialize JSON params to bytes and deserialize to JSON works for:" $ do
  it "Unit" $ do
    fromToJSONSucceed (RsUnit) $ AE.Null

  it "Bool" $ do
    fromToJSONSucceed (RsBool) $ AE.Bool False
    fromToJSONSucceed (RsBool) $ AE.Bool True

  it "U8" $ do
    fromToJSONSucceed RsU8 $ AE.Number 42
    fromToJSONSucceed RsU8 $ AE.Number 255  -- Max
    fromToJSONFail    RsU8 $ AE.Number (-1) -- Min - 1
    fromToJSONFail    RsU8 $ AE.Number 256  -- Max + 1

  it "U16" $ do
    fromToJSONSucceed RsU16 $ AE.Number 42
    fromToJSONSucceed RsU16 $ AE.Number 65_535 -- Max
    fromToJSONFail    RsU16 $ AE.Number (-1)   -- Min - 1
    fromToJSONFail    RsU16 $ AE.Number 65_536 -- Max + 1

  it "U32" $ do
    fromToJSONSucceed RsU32 $ AE.Number 42
    fromToJSONSucceed RsU32 $ AE.Number 4_294_967_295 -- Max
    fromToJSONFail    RsU32 $ AE.Number (-1)          -- Min - 1
    fromToJSONFail    RsU32 $ AE.Number 4_294_967_296 -- Max + 1

  it "U64" $ do
    fromToJSONSucceed RsU64 $ AE.Number 42
    fromToJSONSucceed RsU64 $ AE.Number 18_446_744_073_709_551_615 -- Max
    fromToJSONFail    RsU64 $ AE.Number (-1)                       -- Min - 1
    fromToJSONFail    RsU64 $ AE.Number 18_446_744_073_709_551_616 -- Max + 1

  it "I8" $ do
    fromToJSONSucceed RsI8 $ AE.Number (-42)
    fromToJSONSucceed RsI8 $ AE.Number 42
    fromToJSONSucceed RsI8 $ AE.Number (-128) -- Min
    fromToJSONSucceed RsI8 $ AE.Number 127    -- Max
    fromToJSONFail    RsI8 $ AE.Number (-129) -- Min - 1
    fromToJSONFail    RsI8 $ AE.Number 128    -- Max + 1

  it "I16" $ do
    fromToJSONSucceed RsI16 $ AE.Number (-42)
    fromToJSONSucceed RsI16 $ AE.Number 42
    fromToJSONSucceed RsI16 $ AE.Number (-32_768) -- Min
    fromToJSONSucceed RsI16 $ AE.Number 32_767    -- Max
    fromToJSONFail    RsI16 $ AE.Number (-32_769) -- Min - 1
    fromToJSONFail    RsI16 $ AE.Number 32_768    -- Max + 1

  it "I32" $ do
    fromToJSONSucceed RsI32 $ AE.Number (-42)
    fromToJSONSucceed RsI32 $ AE.Number 42
    fromToJSONSucceed RsI32 $ AE.Number (-2_147_483_648) -- Min
    fromToJSONSucceed RsI32 $ AE.Number 2_147_483_647    -- Max
    fromToJSONFail    RsI32 $ AE.Number (-2_147_483_649) -- Min - 1
    fromToJSONFail    RsI32 $ AE.Number 2_147_483_648    -- Max + 1

  it "I64" $ do
    fromToJSONSucceed RsI64 $ AE.Number (-42)
    fromToJSONSucceed RsI64 $ AE.Number 42
    fromToJSONSucceed RsI64 $ AE.Number (-9_223_372_036_854_775_808) -- Min
    fromToJSONSucceed RsI64 $ AE.Number 9_223_372_036_854_775_807    -- Max
    fromToJSONFail    RsI64 $ AE.Number (-9_223_372_036_854_775_809) -- Min - 1
    fromToJSONFail    RsI64 $ AE.Number 9_223_372_036_854_775_808    -- Max + 1

  it "Amount" $ do
    fromToJSONSucceed RsAmount $ AE.String "42"
    fromToJSONFail RsAmount $ AE.String "-42" -- Negatives not allowed
    fromToJSONFail RsAmount $ AE.String "42.5" -- Floats not allowed
    fromToJSONFail RsAmount $ AE.String "-42.0" -- Negative floats are definitely not allowed

  it "AccountAddress" $ do
    fromToJSONSucceed RsAccountAddress $ AE.String . Text.pack $ accAddr
    fromToJSONFail    RsAccountAddress $ AE.String . Text.pack . drop 10 $ accAddr
    fromToJSONFail    RsAccountAddress $ AE.String . Text.pack . (++ "---") . drop 3 $ accAddr

  it "ContractAddress" $ do
    fromToJSON RsContractAddress (object ["index" .= idx]) `shouldBe` Right (object ["index" .= idx, "subindex" .= subidx0])
    fromToJSONSucceed    RsContractAddress $ object ["index" .= idx, "subindex" .= subidx]
    fromToJSONFail       RsContractAddress $ object ["subindex" .= subidx]
    fromToJSONFail       RsContractAddress $ object ["wrongname" .= idx]
    fromToJSONFail       RsContractAddress $ object ["index" .= idx, "subindex" .= subidx, "tooManyFields" .= idx]
    fromToJSONFail       RsContractAddress $ object []

  it "Pair" $ do
    fromToJSONSucceed (RsPair RsU8 RsU8) $ AE.toJSON ([99, 255] :: [Word8])
    fromToJSONSucceed (RsPair RsU64 RsI8) $ toArray $ [AE.Number 99, AE.Number (-54)]

  it "String" $ do
    fromToJSONSucceed (RsString LenU8) $ AE.String "something"
    fromToJSONSucceed (RsString LenU8) $ AE.String . Text.pack . replicate 255 $ 'a'
    fromToJSONFail (RsString LenU8) $ AE.String . Text.pack . replicate 256 $ 'a'

  it "List" $ do
    fromToJSONSucceed (RsList LenU16 (RsString LenU8)) $ AE.Array . V.fromList $ ["abc", "def"]
    fromToJSONSucceed (RsList LenU8 RsU8) $ toNumArray [0,1,2]
    fromToJSONFail (RsList LenU8 RsU8) $ toNumArray $ replicate 256 2

  it "Set" $ do
    fromToJSONSucceed (RsSet LenU16 RsU8) $ toNumArray [1,2,3,4,5]
    fromToJSONSucceed (RsSet LenU8 RsU8) $ toNumArray [1..255]
    fromToJSONFail (RsSet LenU8 RsU8) $ toNumArray [0..256] -- Too long
    fromToJSONFail (RsSet LenU8 RsU8) $ toNumArray [1, 2, 2, 3] -- Contains duplicates

  it "Map" $ do
    fromToJSONSucceed (RsMap LenU16 RsU8 (RsString LenU64)) $ toArray . map toPair $ ([(0, "a"), (1, "b"), (2, "c")] :: [(Word8, Text)])
    fromToJSONSucceed (RsMap LenU8 RsU8 RsU16) $ toArray . map toPair $ (zip [0..10] [20..30] :: [(Int, Int)])
    fromToJSONSucceed (RsMap LenU8 RsU8 RsU8) $ toArray . map toPair $ (zip [1..255] [1..255] :: [(Int, Int)])
    fromToJSONFail (RsMap LenU8 RsU8 RsU8) $ toArray . map toPair $ (zip [1..256] [1..256] :: [(Int, Int)]) -- Too long

  it "Array" $ do
    fromToJSONSucceed (RsArray 10 RsU8) $ toNumArray [1..10]
    fromToJSONFail (RsArray 1000 RsU8) $ toNumArray [1..1001] -- Too long
    fromToJSONFail (RsArray 1000 RsU8) $ toNumArray [1..999] -- Too short

  it "Struct" $ do
    -- Named
    let namedStructType = RsStruct (Named [("num", RsU8), ("str", RsString LenU8)])
    fromToJSONSucceed namedStructType $ toArray [object ["num" .= AE.Number 42], object ["str" .= AE.String "abc"]]
    fromToJSON namedStructType (toArray [object ["str" .= AE.String "abc"], object ["num" .= AE.Number 42]])
      `shouldBe` Right (toArray [object ["num" .= AE.Number 42], object ["str" .= AE.String "abc"]]) -- Fields in different ordering works
    fromToJSONFail namedStructType $ toArray [object ["str" .= AE.String "abc"]] -- missing fields
    fromToJSONFail namedStructType $ toArray [object ["str" .= AE.String "abc"], object ["wrong" .= AE.String "field"]] -- missing and incorrect field
    fromToJSONFail namedStructType $ toArray [object ["num" .= AE.Number 42]
                                         , object ["str" .= AE.String "abc"]
                                         , object ["str" .= AE.String "def"]] -- duplicate fields

    -- Unnamed
    let unnamedStructType = RsStruct (Unnamed [RsU8, RsString LenU8, RsPair RsU8 RsAccountAddress])
    fromToJSONSucceed unnamedStructType $
      toArray [AE.Number 10, AE.String "abc", toArray [AE.Number 8, AE.String (Text.pack accAddr)]]
    fromToJSONFail unnamedStructType $
      toArray [AE.Number 10, toArray [AE.Number 8, AE.String (Text.pack accAddr)]] -- Too few fields
    fromToJSONFail unnamedStructType $
      toArray [AE.Number 10, AE.Number 11, AE.Number 12, toArray [AE.Number 8, AE.String (Text.pack accAddr)]] -- Too many fields

    -- Unit
    fromToJSONSucceed (RsStruct Unit) $ toArray []
    fromToJSONFail (RsStruct Unit) $ toArray [AE.Null] -- Non-empty list for unit

  it "Enum" $ do
    let enumType = RsEnum [ ("a", Named [("a.1", RsBool)])
                          , ("b", Unnamed [RsBool])
                          , ("c", Unit)]

    fromToJSONSucceed enumType $ object ["a" .= toArray [object ["a.1" .= AE.Bool True]]]
    fromToJSONSucceed enumType $ object ["b" .= toArray [AE.Bool True]]
    fromToJSONSucceed enumType $ object ["c" .= toArray []]

  where idx :: Word64
        idx = 42

        subidx :: Word64
        subidx = 255

        subidx0 :: Word64
        subidx0 = 0

        toArray = AE.Array . V.fromList

        toNumArray :: [Int] -> AE.Value
        toNumArray = toArray . map (AE.Number . fromIntegral)

        toPair :: (ToJSON a, ToJSON b) => (a, b) -> AE.Value
        toPair (a,b) = toArray [AE.toJSON a, AE.toJSON b]

        accAddr :: String
        accAddr = "47JNHkJZo9ShomDypbiSJzdGN7FNxo8MwtUFsPa49KGvejf7Wh"

        fromToJSON :: RsType -> AE.Value -> Either String AE.Value
        fromToJSON rsType originalJSON = serializeParams rsType originalJSON >>= runGetRsValueAsJSON rsType

        fromToJSONSucceed :: RsType -> AE.Value -> Expectation
        fromToJSONSucceed rsType originalJSON = fromToJSON rsType originalJSON `shouldBe` Right originalJSON

        fromToJSONFail :: RsType -> AE.Value -> Expectation
        fromToJSONFail rsType originalJSON = fromToJSON rsType originalJSON `shouldSatisfy` isLeft


-- ** Arbitrary Generators **

genModel :: Gen Model
genModel = JustBytes . BS.pack <$> listOf (arbitrary :: Gen Word8)

genSchema :: Int -> Gen Schema
genSchema n = Schema <$> genState <*> (HM.fromList <$> listOf (genTwoOf genText (genRsType n')))
  where genState = case n of
          0 -> pure Nothing
          _ -> frequency [ (9, Just <$> genRsType n')
                         , (1, pure Nothing) ]
        n' = nextNSize n

genFields :: Int -> Gen Fields
genFields n
  | n == 0 = pure Unit
  | otherwise = frequency [ (8, Named <$> listOf (genTwoOf genText (genRsType n')))
                  , (4, Unnamed <$> listOf (genRsType n'))
                  , (1, pure Unit) ]
  where n' = nextNSize n

genRsType :: Int -> Gen RsType
genRsType n
  | n == 0 = pure RsUnit
  | otherwise = oneof
      [ pure RsUnit
      , pure RsBool
      , pure RsU8
      , pure RsU16
      , pure RsU32
      , pure RsU64
      , pure RsI8
      , pure RsI16
      , pure RsI32
      , pure RsI64
      , pure RsAccountAddress
      , pure RsContractAddress
      , RsOption <$> genRsType n'
      , RsPair <$> genRsType n' <*> genRsType n'
      , RsString <$> genSizeLen
      , RsList <$> genSizeLen <*> genRsType n'
      , RsSet <$> genSizeLen <*> genRsType n'
      , RsMap <$> genSizeLen <*> genRsType n' <*> genRsType n'
      , RsArray <$> arbitrary <*> genRsType n'
      , RsStruct <$> genFields n'
      , RsEnum <$> listOf (genTwoOf genText (genFields n')) ]
  where n' = nextNSize n

genSizeLen :: Gen SizeLength
genSizeLen = oneof $ pure <$> [LenU8, LenU16, LenU32, LenU64]

genTwoOf :: Gen a -> Gen b -> Gen (a, b)
genTwoOf ga gb = (,) <$> ga <*> gb

genText :: Gen Text
genText = Text.pack <$> arbitrary

-- The factor here determines how large generated test data will become (large factor => small test data).
nextNSize :: Int -> Int
nextNSize = (`div` 3)
