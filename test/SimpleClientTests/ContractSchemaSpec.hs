{-# LANGUAGE NumericUnderscores #-}
module SimpleClientTests.ContractSchemaSpec where

import Concordium.Client.Types.ContractSchema

import Data.Aeson (ToJSON, object, (.=))
import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Data.Word (Word8, Word64)
import Test.Hspec
import Test.QuickCheck

contractSchemaSpec :: Spec
contractSchemaSpec = describe "contractSchema" $ do
  printSchemaSpec
  printInfoSpec
  printParamsSpec

printSchemaSpec :: Spec
printSchemaSpec = describe "serialize Module" $ do
  it "decode is inverse of encode" $ withMaxSuccess 30 $
    forAll (sized genModule) $ \c -> (S.decode . S.encode) c === Right c

printInfoSpec :: Spec
printInfoSpec = describe "serialize Info" $ do
  it "fromJSON is inverse of toJSON for Info" $ do
    pendingWith "TODO"

  it "fromJSON is inverse of toJSON for Model" $ withMaxSuccess 100 $
    forAll genModel $ \m -> (AE.fromJSON . AE.toJSON) m === AE.Success m


printParamsSpec :: Spec
printParamsSpec = describe "serialize JSON params to bytes and deserialize to JSON works for:" $ do
  it "Unit" $ do
    fromToJSONSucceed (Unit) $ AE.Null

  it "Bool" $ do
    fromToJSONSucceed Bool $ AE.Bool False
    fromToJSONSucceed Bool $ AE.Bool True

  it "U8" $ do
    fromToJSONSucceed U8 $ AE.Number 42
    fromToJSONSucceed U8 $ AE.Number 255  -- Max
    fromToJSONFail    U8 $ AE.Number (-1) -- Min - 1
    fromToJSONFail    U8 $ AE.Number 256  -- Max + 1

  it "U16" $ do
    fromToJSONSucceed U16 $ AE.Number 42
    fromToJSONSucceed U16 $ AE.Number 65_535 -- Max
    fromToJSONFail    U16 $ AE.Number (-1)   -- Min - 1
    fromToJSONFail    U16 $ AE.Number 65_536 -- Max + 1

  it "U32" $ do
    fromToJSONSucceed U32 $ AE.Number 42
    fromToJSONSucceed U32 $ AE.Number 4_294_967_295 -- Max
    fromToJSONFail    U32 $ AE.Number (-1)          -- Min - 1
    fromToJSONFail    U32 $ AE.Number 4_294_967_296 -- Max + 1

  it "U64" $ do
    fromToJSONSucceed U64 $ AE.Number 42
    fromToJSONSucceed U64 $ AE.Number 18_446_744_073_709_551_615 -- Max
    fromToJSONFail    U64 $ AE.Number (-1)                       -- Min - 1
    fromToJSONFail    U64 $ AE.Number 18_446_744_073_709_551_616 -- Max + 1

  it "I8" $ do
    fromToJSONSucceed I8 $ AE.Number (-42)
    fromToJSONSucceed I8 $ AE.Number 42
    fromToJSONSucceed I8 $ AE.Number (-128) -- Min
    fromToJSONSucceed I8 $ AE.Number 127    -- Max
    fromToJSONFail    I8 $ AE.Number (-129) -- Min - 1
    fromToJSONFail    I8 $ AE.Number 128    -- Max + 1

  it "I16" $ do
    fromToJSONSucceed I16 $ AE.Number (-42)
    fromToJSONSucceed I16 $ AE.Number 42
    fromToJSONSucceed I16 $ AE.Number (-32_768) -- Min
    fromToJSONSucceed I16 $ AE.Number 32_767    -- Max
    fromToJSONFail    I16 $ AE.Number (-32_769) -- Min - 1
    fromToJSONFail    I16 $ AE.Number 32_768    -- Max + 1

  it "I32" $ do
    fromToJSONSucceed I32 $ AE.Number (-42)
    fromToJSONSucceed I32 $ AE.Number 42
    fromToJSONSucceed I32 $ AE.Number (-2_147_483_648) -- Min
    fromToJSONSucceed I32 $ AE.Number 2_147_483_647    -- Max
    fromToJSONFail    I32 $ AE.Number (-2_147_483_649) -- Min - 1
    fromToJSONFail    I32 $ AE.Number 2_147_483_648    -- Max + 1

  it "I64" $ do
    fromToJSONSucceed I64 $ AE.Number (-42)
    fromToJSONSucceed I64 $ AE.Number 42
    fromToJSONSucceed I64 $ AE.Number (-9_223_372_036_854_775_808) -- Min
    fromToJSONSucceed I64 $ AE.Number 9_223_372_036_854_775_807    -- Max
    fromToJSONFail    I64 $ AE.Number (-9_223_372_036_854_775_809) -- Min - 1
    fromToJSONFail    I64 $ AE.Number 9_223_372_036_854_775_808    -- Max + 1

  it "Amount" $ do
    fromToJSONSucceed Amount $ AE.String "42"
    fromToJSONFail    Amount $ AE.String "-42" -- Negatives not allowed
    fromToJSONFail    Amount $ AE.String "42.5" -- Floats not allowed
    fromToJSONFail    Amount $ AE.String "-42.0" -- Negative floats are definitely not allowed

  it "AccountAddress" $ do
    fromToJSONSucceed AccountAddress $ AE.String . Text.pack $ accAddr
    fromToJSONFail    AccountAddress $ AE.String . Text.pack . drop 10 $ accAddr
    fromToJSONFail    AccountAddress $ AE.String . Text.pack . (++ "---") . drop 3 $ accAddr

  it "ContractAddress" $ do
    fromToJSON ContractAddress (object ["index" .= idx]) `shouldBe` Right (object ["index" .= idx, "subindex" .= subidx0])
    fromToJSONSucceed    ContractAddress $ object ["index" .= idx, "subindex" .= subidx]
    fromToJSONFail       ContractAddress $ object ["subindex" .= subidx]
    fromToJSONFail       ContractAddress $ object ["wrongname" .= idx]
    fromToJSONFail       ContractAddress $ object ["index" .= idx, "subindex" .= subidx, "tooManyFields" .= idx]
    fromToJSONFail       ContractAddress $ object []

  it "Pair" $ do
    fromToJSONSucceed (Pair U8 U8)  $ AE.toJSON ([99, 255] :: [Word8])
    fromToJSONSucceed (Pair U64 I8) $ toArray $ [AE.Number 99, AE.Number (-54)]

  it "List" $ do
    fromToJSONSucceed (List LenU16 U16) $ toNumArray [0,1,2]
    fromToJSONFail (List LenU8 U8) $ toNumArray $ replicate 256 2

  it "Set" $ do
    fromToJSONSucceed (Set LenU16 U8) $ toNumArray [1,2,3,4,5]
    fromToJSONSucceed (Set LenU8 U8)  $ toNumArray [1..255]
    fromToJSONFail (Set LenU8 U8)     $ toNumArray [0..256] -- Too long
    fromToJSONFail (Set LenU8 U8)     $ toNumArray [1, 2, 2, 3] -- Contains duplicates

  it "Map" $ do
    fromToJSONSucceed (Map LenU8 U8 U16) $ toArray . map toPair $ (zip [0..10] [20..30] :: [(Int, Int)])
    fromToJSONSucceed (Map LenU8 U8 U8) $ toArray . map toPair $ (zip [1..255] [1..255] :: [(Int, Int)])
    fromToJSONFail (Map LenU8 U8 U8) $ toArray . map toPair $ (zip [1..256] [1..256] :: [(Int, Int)]) -- Too long

  it "Array" $ do
    fromToJSONSucceed (Array 10 U8) $ toNumArray [1..10]
    fromToJSONFail (Array 1000 U8)  $ toNumArray [1..1001] -- Too long
    fromToJSONFail (Array 1000 U8)  $ toNumArray [1..999] -- Too short

  it "Struct" $ do
    -- Named
    let namedStructType = Struct (Named [("num", U8), ("bool", Bool)])
    fromToJSONSucceed namedStructType $ object ["num" .= AE.Number 42, "bool" .= AE.Bool True]
    fromToJSON namedStructType (object ["bool" .= AE.Bool True, "num" .= AE.Number 42])
      `shouldBe` Right (object ["num" .= AE.Number 42, "bool" .= AE.Bool True]) -- Fields in different ordering works
    fromToJSONFail namedStructType $ object ["bool" .= AE.Bool True] -- missing fields
    fromToJSONFail namedStructType $ object ["bool" .= AE.Bool True, "wrong" .= AE.String "field"] -- missing and incorrect field

    -- Unnamed
    let unnamedStructType = Struct (Unnamed [U8, Bool, Pair U8 AccountAddress])
    fromToJSONSucceed unnamedStructType $
      toArray [AE.Number 10, AE.Bool True, toArray [AE.Number 8, AE.String (Text.pack accAddr)]]
    fromToJSONFail unnamedStructType $
      toArray [AE.Number 10, toArray [AE.Number 8, AE.String (Text.pack accAddr)]] -- Too few fields
    fromToJSONFail unnamedStructType $
      toArray [AE.Number 10, AE.Number 11, AE.Number 12, toArray [AE.Number 8, AE.String (Text.pack accAddr)]] -- Too many fields

    -- Empty
    fromToJSONSucceed (Struct Empty) $ toArray []
    fromToJSONFail (Struct Empty) $ toArray [AE.Null] -- Non-empty list for Empty

  it "Enum" $ do
    let enumType = Enum [ ("a", Named [("a.1", Bool)])
                        , ("b", Unnamed [Bool])
                        , ("c", Empty)]

    fromToJSONSucceed enumType $ object ["a" .= object ["a.1" .= AE.Bool True]]
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

        fromToJSON :: SchemaType -> AE.Value -> Either String AE.Value
        fromToJSON typ originalJSON = serializeParams typ originalJSON >>= S.runGet (getValueAsJSON typ)

        fromToJSONSucceed :: SchemaType -> AE.Value -> Expectation
        fromToJSONSucceed typ originalJSON = fromToJSON typ originalJSON `shouldBe` Right originalJSON

        fromToJSONFail :: SchemaType -> AE.Value -> Expectation
        fromToJSONFail typ originalJSON = fromToJSON typ originalJSON `shouldSatisfy` isLeft


-- ** Arbitrary Generators **

genModel :: Gen Model
genModel = JustBytes . BS.pack <$> listOf (arbitrary :: Gen Word8)

genModule :: Int -> Gen Module
genModule n = Module . Map.fromList <$> listOf (genTwoOf genText $ genContract n)

genContract :: Int -> Gen Contract
genContract n = Contract <$> genMaybeSchemaType <*> genMaybeSchemaType <*> genReceiveSigs
  where genMaybeSchemaType :: Gen (Maybe SchemaType)
        genMaybeSchemaType = frequency [ (7, Just <$> genSchemaType (n'))
                                       , (1, pure Nothing) ]

        genReceiveSigs :: Gen (Map Text SchemaType)
        genReceiveSigs = Map.fromList <$> listOf (genTwoOf genText (genSchemaType n'))

        n' = nextNSize n

genFields :: Int -> Gen Fields
genFields n
  | n == 0 = pure Empty
  | otherwise = frequency [ (8, Named <$> listOf (genTwoOf genText (genSchemaType n')))
                  , (4, Unnamed <$> listOf (genSchemaType n'))
                  , (1, pure Empty) ]
  where n' = nextNSize n

genSchemaType :: Int -> Gen SchemaType
genSchemaType n
  | n == 0 = pure Unit
  | otherwise = oneof
      [ pure Unit
      , pure Bool
      , pure U8
      , pure U16
      , pure U32
      , pure U64
      , pure I8
      , pure I16
      , pure I32
      , pure I64
      , pure AccountAddress
      , pure ContractAddress
      , Pair <$> genSchemaType n' <*> genSchemaType n'
      , List <$> genSizeLen <*> genSchemaType n'
      , Set <$> genSizeLen <*> genSchemaType n'
      , Map <$> genSizeLen <*> genSchemaType n' <*> genSchemaType n'
      , Array <$> arbitrary <*> genSchemaType n'
      , Struct <$> genFields n'
      , Enum <$> listOf (genTwoOf genText (genFields n')) ]
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
