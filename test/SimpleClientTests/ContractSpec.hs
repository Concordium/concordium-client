{-# LANGUAGE NumericUnderscores #-}

module SimpleClientTests.ContractSpec where

import Concordium.Client.Types.Contract.Parameter
import Concordium.Client.Types.Contract.Schema

import Data.Aeson (ToJSON, object, (.=))
import qualified Data.Aeson as AE
import Data.Either (isLeft)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Data.Word (Word64, Word8)
import Test.Hspec
import Test.QuickCheck

contractSpec :: Spec
contractSpec = describe "contract" $ do
    printParameterSpec

printParameterSpec :: Spec
printParameterSpec = describe "serialize JSON params to bytes and deserialize to JSON works for:" $ do
    it "Unit" $ do
        fromToJSONSucceed Unit AE.Null

    it "Bool" $ do
        fromToJSONSucceed Bool $ AE.Bool False
        fromToJSONSucceed Bool $ AE.Bool True

    it "UInt8" $ do
        fromToJSONSucceed UInt8 $ AE.Number 42
        fromToJSONSucceed UInt8 $ AE.Number 255 -- Max
        fromToJSONFail UInt8 $ AE.Number (-1) -- Min - 1
        fromToJSONFail UInt8 $ AE.Number 256 -- Max + 1
    it "UInt16" $ do
        fromToJSONSucceed UInt16 $ AE.Number 42
        fromToJSONSucceed UInt16 $ AE.Number 65_535 -- Max
        fromToJSONFail UInt16 $ AE.Number (-1) -- Min - 1
        fromToJSONFail UInt16 $ AE.Number 65_536 -- Max + 1
    it "UInt32" $ do
        fromToJSONSucceed UInt32 $ AE.Number 42
        fromToJSONSucceed UInt32 $ AE.Number 4_294_967_295 -- Max
        fromToJSONFail UInt32 $ AE.Number (-1) -- Min - 1
        fromToJSONFail UInt32 $ AE.Number 4_294_967_296 -- Max + 1
    it "UInt64" $ do
        fromToJSONSucceed UInt64 $ AE.Number 42
        fromToJSONSucceed UInt64 $ AE.Number 18_446_744_073_709_551_615 -- Max
        fromToJSONFail UInt64 $ AE.Number (-1) -- Min - 1
        fromToJSONFail UInt64 $ AE.Number 18_446_744_073_709_551_616 -- Max + 1
    it "Int8" $ do
        fromToJSONSucceed Int8 $ AE.Number (-42)
        fromToJSONSucceed Int8 $ AE.Number 42
        fromToJSONSucceed Int8 $ AE.Number (-128) -- Min
        fromToJSONSucceed Int8 $ AE.Number 127 -- Max
        fromToJSONFail Int8 $ AE.Number (-129) -- Min - 1
        fromToJSONFail Int8 $ AE.Number 128 -- Max + 1
    it "Int16" $ do
        fromToJSONSucceed Int16 $ AE.Number (-42)
        fromToJSONSucceed Int16 $ AE.Number 42
        fromToJSONSucceed Int16 $ AE.Number (-32_768) -- Min
        fromToJSONSucceed Int16 $ AE.Number 32_767 -- Max
        fromToJSONFail Int16 $ AE.Number (-32_769) -- Min - 1
        fromToJSONFail Int16 $ AE.Number 32_768 -- Max + 1
    it "Int32" $ do
        fromToJSONSucceed Int32 $ AE.Number (-42)
        fromToJSONSucceed Int32 $ AE.Number 42
        fromToJSONSucceed Int32 $ AE.Number (-2_147_483_648) -- Min
        fromToJSONSucceed Int32 $ AE.Number 2_147_483_647 -- Max
        fromToJSONFail Int32 $ AE.Number (-2_147_483_649) -- Min - 1
        fromToJSONFail Int32 $ AE.Number 2_147_483_648 -- Max + 1
    it "Int64" $ do
        fromToJSONSucceed Int64 $ AE.Number (-42)
        fromToJSONSucceed Int64 $ AE.Number 42
        fromToJSONSucceed Int64 $ AE.Number (-9_223_372_036_854_775_808) -- Min
        fromToJSONSucceed Int64 $ AE.Number 9_223_372_036_854_775_807 -- Max
        fromToJSONFail Int64 $ AE.Number (-9_223_372_036_854_775_809) -- Min - 1
        fromToJSONFail Int64 $ AE.Number 9_223_372_036_854_775_808 -- Max + 1
    it "Amount" $ do
        fromToJSONSucceed Amount $ AE.String "42"
        fromToJSONFail Amount $ AE.String "-42" -- Negatives not allowed
        fromToJSONFail Amount $ AE.String "42.5" -- Floats not allowed
        fromToJSONFail Amount $ AE.String "-42.0" -- Negative floats are definitely not allowed
    it "AccountAddress" $ do
        fromToJSONSucceed AccountAddress $ AE.String . Text.pack $ accAddr
        fromToJSONFail AccountAddress $ AE.String . Text.pack . drop 10 $ accAddr
        fromToJSONFail AccountAddress $ AE.String . Text.pack . (++ "---") . drop 3 $ accAddr

    it "ContractAddress" $ do
        fromToJSON ContractAddress (object ["index" .= idx]) `shouldBe` Right (object ["index" .= idx, "subindex" .= subidx0])
        fromToJSONSucceed ContractAddress $ object ["index" .= idx, "subindex" .= subidx]
        fromToJSONFail ContractAddress $ object ["subindex" .= subidx]
        fromToJSONFail ContractAddress $ object ["wrongname" .= idx]
        fromToJSONFail ContractAddress $ object ["index" .= idx, "subindex" .= subidx, "tooManyFields" .= idx]
        fromToJSONFail ContractAddress $ object []

    it "Timestamp" $ do
        fromToJSON Timestamp (AE.String "1996-12-19T16:39:57Z")
            `shouldBe` Right (AE.String "1996-12-19T16:39:57Z") -- UTC
        fromToJSON Timestamp (AE.String "1996-12-19T08:39:57-08:20")
            `shouldBe` Right (AE.String "1996-12-19T16:59:57Z") -- Different timezone
        fromToJSON Timestamp (AE.String "1996-12-19T16:39:57.87+00:00") `shouldBe` Right (AE.String "1996-12-19T16:39:57.87Z")

        fromToJSONFail Timestamp (AE.String "1969-12-31T23:59:59Z") -- Before unix time stamps (1970-01-01)
        fromToJSONFail Timestamp (AE.String "1996-12-19T16:39:57") -- No timezone
    it "Duration" $ do
        fromToJSONSucceed Duration (AE.String "1d 20h 30m 40s 50ms")
        fromToJSON Duration (AE.String "40s 50ms 1d 30m 20h")
            `shouldBe` Right (AE.String "1d 20h 30m 40s 50ms") -- different ordering
        fromToJSON Duration (AE.String "1d 2d 10h 10h 20m 20m 25s 25s 400ms 400ms")
            `shouldBe` Right (AE.String "3d 20h 40m 50s 800ms") -- combine
        fromToJSON Duration (AE.String "12h 12h 30m 30m 30s 30s 500ms 500ms 1ms")
            `shouldBe` Right (AE.String "1d 1h 1m 1s 1ms") -- combine and collect
    it "Pair" $ do
        fromToJSONSucceed (Pair UInt8 UInt8) $ AE.toJSON ([99, 255] :: [Word8])
        fromToJSONSucceed (Pair UInt64 Int8) $ toArray $ [AE.Number 99, AE.Number (-54)]

    it "List" $ do
        fromToJSONSucceed (List Two UInt16) $ toNumArray [0, 1, 2]
        fromToJSONFail (List One UInt8) $ toNumArray $ replicate 256 2

    it "Set" $ do
        fromToJSONSucceed (Set Two UInt8) $ toNumArray [1, 2, 3, 4, 5]
        fromToJSONSucceed (Set One UInt8) $ toNumArray [1 .. 255]
        fromToJSONFail (Set One UInt8) $ toNumArray [0 .. 256] -- Too long
    it "Map" $ do
        fromToJSONSucceed (Map One UInt8 UInt16) $ toArray . map toPair $ (zip [0 .. 10] [20 .. 30] :: [(Int, Int)])
        fromToJSONSucceed (Map One UInt8 UInt8) $ toArray . map toPair $ (zip [1 .. 255] [1 .. 255] :: [(Int, Int)])
        fromToJSONFail (Map One UInt8 UInt8) $ toArray . map toPair $ (zip [1 .. 256] [1 .. 256] :: [(Int, Int)]) -- Too long
    it "Array" $ do
        fromToJSONSucceed (Array 10 UInt8) $ toNumArray [1 .. 10]
        fromToJSONFail (Array 1000 UInt8) $ toNumArray [1 .. 1001] -- Too long
        fromToJSONFail (Array 1000 UInt8) $ toNumArray [1 .. 999] -- Too short
    it "Struct" $ do
        -- Named
        let namedStructType = Struct (Named [("num", UInt8), ("bool", Bool)])
        fromToJSONSucceed namedStructType $ object ["num" .= AE.Number 42, "bool" .= AE.Bool True]
        fromToJSON namedStructType (object ["bool" .= AE.Bool True, "num" .= AE.Number 42])
            `shouldBe` Right (object ["num" .= AE.Number 42, "bool" .= AE.Bool True]) -- Fields in different ordering works
        fromToJSONFail namedStructType $ object ["bool" .= AE.Bool True] -- missing fields
        fromToJSONFail namedStructType $ object ["bool" .= AE.Bool True, "wrong" .= AE.String "field"] -- missing and incorrect field

        -- Unnamed
        let unnamedStructType = Struct (Unnamed [UInt8, Bool, Pair UInt8 AccountAddress])
        fromToJSONSucceed unnamedStructType $
            toArray [AE.Number 10, AE.Bool True, toArray [AE.Number 8, AE.String (Text.pack accAddr)]]
        fromToJSONFail unnamedStructType $
            toArray [AE.Number 10, toArray [AE.Number 8, AE.String (Text.pack accAddr)]] -- Too few fields
        fromToJSONFail unnamedStructType $
            toArray [AE.Number 10, AE.Number 11, AE.Number 12, toArray [AE.Number 8, AE.String (Text.pack accAddr)]] -- Too many fields

        -- None
        fromToJSONSucceed (Struct None) $ toArray []
        fromToJSONFail (Struct None) $ toArray [AE.Null] -- Non-empty list for None
    it "Enum" $ do
        let enumType =
                Enum
                    [ ("a", Named [("a.1", Bool)]),
                      ("b", Unnamed [Bool]),
                      ("c", None)
                    ]

        fromToJSONSucceed enumType $ object ["a" .= object ["a.1" .= AE.Bool True]]
        fromToJSONSucceed enumType $ object ["b" .= toArray [AE.Bool True]]
        fromToJSONSucceed enumType $ object ["c" .= toArray []]

    it "TaggedEnum" $ do
        let taggedEnumType =
                TaggedEnum $
                    Map.fromList
                        [ (251, ("a" :: Text, Named [("a.1", Bool)])),
                          (255, ("b" :: Text, Unnamed [Bool])),
                          (1, ("c" :: Text, None))
                        ]

        fromToJSONSucceed taggedEnumType $ object ["a" .= object ["a.1" .= AE.Bool True]]
        fromToJSONSucceed taggedEnumType $ object ["b" .= toArray [AE.Bool True]]
        fromToJSONSucceed taggedEnumType $ object ["c" .= toArray []]

    it "String" $ do
        fromToJSONSucceed (String One) $ AE.String "something"
        fromToJSONSucceed (String One) $ AE.String "UTF-8: 😀🦄✅"
        fromToJSONSucceed (String One) $ AE.String . Text.pack . replicate 255 $ 'a'
        fromToJSONFail (String One) $ AE.String . Text.pack . replicate 256 $ 'a' -- Too long
    it "UInt128" $ do
        fromToJSONFail UInt128 $ AE.String "-1" -- Below min bound
        fromToJSONSucceed UInt128 $ AE.String "0" -- Min bound
        fromToJSONSucceed UInt128 $ AE.String "340282366920938463463374607431768211455" -- Max bound
        fromToJSONFail UInt128 $ AE.String "340282366920938463463374607431768211456" -- Greater than max bound
        fromToJSONFail UInt128 $ AE.String "3.3" -- With decimal point
        fromToJSONFail UInt128 $ AE.String "NotANumber" -- Not a number
    it "Int128" $ do
        fromToJSONFail Int128 $ AE.String "-170141183460469231731687303715884105729" -- Below min bound
        fromToJSONSucceed Int128 $ AE.String "-170141183460469231731687303715884105728" -- Min bound
        fromToJSONSucceed Int128 $ AE.String "170141183460469231731687303715884105727" -- Max bound
        fromToJSONFail Int128 $ AE.String "170141183460469231731687303715884105728" -- Greater than max bound
        fromToJSONFail Int128 $ AE.String "3.3" -- With decimal point
        fromToJSONFail Int128 $ AE.String "NotANumber" -- Not a number
    it "ContractName" $ do
        fromToJSONSucceed (ContractName One) $ object ["contract" .= AE.String "contrName"]
        fromToJSONFail (ContractName One) $ object [] -- Missing field
        fromToJSONFail (ContractName One) $ object ["contract" .= AE.Number 3] -- Incorrect type
        fromToJSONFail (ContractName One) $ object ["wrongKey" .= AE.String "contrName"] -- Incorrect key
        -- Additional fields not allowed
        fromToJSONFail (ContractName One) $ object ["contract" .= AE.String "contrName", "func" .= AE.String "funcName"]

    it "ReceiveName" $ do
        fromToJSONSucceed (ReceiveName One) $ object ["contract" .= AE.String "contrName", "func" .= AE.String "funcName"]
        fromToJSONFail (ReceiveName One) $ object ["contract" .= AE.String "contrName"] -- Missing field
        fromToJSONFail (ReceiveName One) $ object ["contract" .= AE.String "contrName", "func" .= AE.Number 3] -- Incorrect type
        fromToJSONFail (ReceiveName One) $ object ["contract" .= AE.Number 3, "func" .= AE.String "funcName"] -- Incorrect type
        -- Additional fields not allowed
        fromToJSONFail (ReceiveName One) $
            object
                [ "contract" .= AE.String "contrName",
                  "func" .= AE.String "funcName",
                  "extra" .= AE.String "extra"
                ]
        fromToJSONFail (ReceiveName One) $ object ["contract" .= AE.String "contrName.withDot", "func" .= AE.String "funcName"]

    it "ULeb128" $ do
        fromToJSONSucceed (ULeb128 1) $ AE.String "0"
        fromToJSONSucceed (ULeb128 1) $ AE.String "10"
        fromToJSONSucceed (ULeb128 2) $ AE.String "129"
        fromToJSONSucceed (ULeb128 10) $ AE.String "18446744073709551615"
        fromToJSONSucceed (ULeb128 37) $ AE.String "115792089237316195423570985008687907853269984665640564039457584007913129639935"

        fromToJSONFail (ULeb128 9) $ AE.String "18446744073709551615" -- Byte constraint violation
        fromToJSONFail (ULeb128 2) $ AE.String "-123" -- Negative number
        fromToJSONFail (ULeb128 2) $ AE.String "ab" -- Not a number
    it "ILeb128" $ do
        fromToJSONSucceed (ILeb128 1) $ AE.String "0"
        fromToJSONSucceed (ILeb128 1) $ AE.String "10"
        fromToJSONSucceed (ILeb128 2) $ AE.String "129"
        fromToJSONSucceed (ILeb128 10) $ AE.String "18446744073709551615"
        fromToJSONSucceed (ILeb128 37) $ AE.String "115792089237316195423570985008687907853269984665640564039457584007913129639935"

        fromToJSONFail (ILeb128 9) $ AE.String "18446744073709551615" -- Byte constraint violation
        fromToJSONSucceed (ILeb128 1) $ AE.String "-1"
        fromToJSONSucceed (ILeb128 1) $ AE.String "-10"
        fromToJSONSucceed (ILeb128 2) $ AE.String "-129"
        fromToJSONSucceed (ILeb128 10) $ AE.String "-18446744073709551615"
        fromToJSONSucceed (ILeb128 37) $ AE.String "-115792089237316195423570985008687907853269984665640564039457584007913129639935"

        fromToJSONFail (ILeb128 9) $ AE.String "-18446744073709551615" -- Byte constraint violation
        fromToJSONFail (ILeb128 2) $ AE.String "[]" -- Not a number
    it "ByteList" $ do
        fromToJSONSucceed (ByteList One) $ AE.String "00000000"
        fromToJSONSucceed (ByteList One) $ AE.String "ffffffff"
        fromToJSONSucceed (ByteList One) $ AE.String "123456789abdef"
        fromToJSONSucceed (ByteList One) $ AE.String ""
        fromToJSONSucceed (ByteList One) $ AE.String $ Text.replicate 255 "10"

        fromToJSONFail (ByteList One) $ AE.String $ Text.replicate 256 "10" -- Too long
        fromToJSONFail (ByteList One) $ AE.String "0123456789abdef" -- Invalid base16 Uneven number of characters
        fromToJSONFail (ByteList One) $ AE.String "abcdefghjk" -- Not hex
    it "ByteArray" $ do
        fromToJSONSucceed (ByteArray 4) $ AE.String "00000000"
        fromToJSONSucceed (ByteArray 4) $ AE.String "ffffffff"
        fromToJSONSucceed (ByteArray 7) $ AE.String "123456789abdef"
        fromToJSONSucceed (ByteArray 0) $ AE.String ""
        fromToJSONSucceed (ByteArray 256) $ AE.String $ Text.replicate 256 "10"

        fromToJSONFail (ByteArray 2) $ AE.String "abcdef" -- Too long
        fromToJSONFail (ByteArray 2) $ AE.String "ab" -- Too few
        fromToJSONFail (ByteArray 8) $ AE.String "0123456789abdef" -- Invalid base16 Uneven number of characters
        fromToJSONFail (ByteArray 2) $ AE.String "efgh" -- Not hex
  where
    idx :: Word64
    idx = 42

    subidx :: Word64
    subidx = 255

    subidx0 :: Word64
    subidx0 = 0

    toArray = AE.Array . V.fromList

    toNumArray :: [Int] -> AE.Value
    toNumArray = toArray . map (AE.Number . fromIntegral)

    toPair :: (ToJSON a, ToJSON b) => (a, b) -> AE.Value
    toPair (a, b) = toArray [AE.toJSON a, AE.toJSON b]

    accAddr :: String
    accAddr = "47JNHkJZo9ShomDypbiSJzdGN7FNxo8MwtUFsPa49KGvejf7Wh"

    fromToJSON :: SchemaType -> AE.Value -> Either String AE.Value
    fromToJSON typ originalJSON = serializeWithSchema typ originalJSON >>= S.runGet (getJSONUsingSchema typ)

    fromToJSONSucceed :: SchemaType -> AE.Value -> Expectation
    fromToJSONSucceed typ originalJSON = fromToJSON typ originalJSON `shouldBe` Right originalJSON

    fromToJSONFail :: SchemaType -> AE.Value -> Expectation
    fromToJSONFail typ originalJSON = fromToJSON typ originalJSON `shouldSatisfy` isLeft

-- ** Arbitrary Generators **

genModuleSchemaV0 :: Int -> Gen ModuleSchema
genModuleSchemaV0 n = ModuleSchemaV0 . Map.fromList <$> listOf (genTwoOf genText $ genContractSchema n)

genContractSchema :: Int -> Gen ContractSchemaV0
genContractSchema n = ContractSchemaV0 <$> genMaybeSchemaType <*> genMaybeSchemaType <*> genReceiveSigs
  where
    genMaybeSchemaType :: Gen (Maybe SchemaType)
    genMaybeSchemaType =
        frequency
            [ (7, Just <$> genSchemaType (n')),
              (1, pure Nothing)
            ]

    genReceiveSigs :: Gen (Map Text SchemaType)
    genReceiveSigs = Map.fromList <$> listOf (genTwoOf genText (genSchemaType n'))

    n' = nextNSize n

genFields :: Int -> Gen Fields
genFields n
    | n == 0 = pure None
    | otherwise =
        frequency
            [ (8, Named <$> listOf (genTwoOf genText (genSchemaType n'))),
              (4, Unnamed <$> listOf (genSchemaType n')),
              (1, pure None)
            ]
  where
    n' = nextNSize n

genSchemaType :: Int -> Gen SchemaType
genSchemaType n
    | n == 0 = pure Unit
    | otherwise =
        oneof
            [ pure Unit,
              pure Bool,
              pure UInt8,
              pure UInt16,
              pure UInt32,
              pure UInt64,
              pure Int8,
              pure Int16,
              pure Int32,
              pure Int64,
              pure AccountAddress,
              pure ContractAddress,
              Pair <$> genSchemaType n' <*> genSchemaType n',
              List <$> genSizeLen <*> genSchemaType n',
              Set <$> genSizeLen <*> genSchemaType n',
              Map <$> genSizeLen <*> genSchemaType n' <*> genSchemaType n',
              Array <$> arbitrary <*> genSchemaType n',
              Struct <$> genFields n',
              Enum <$> listOf (genTwoOf genText (genFields n'))
            ]
  where
    n' = nextNSize n

genSizeLen :: Gen SizeLength
genSizeLen = oneof $ pure <$> [One, Two, Four, Eight]

genTwoOf :: Gen a -> Gen b -> Gen (a, b)
genTwoOf ga gb = (,) <$> ga <*> gb

genText :: Gen Text
genText = Text.pack <$> arbitrary

-- The factor here determines how large generated test data will become (large factor => small test data).
nextNSize :: Int -> Int
nextNSize = (`div` 3)
