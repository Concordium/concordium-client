module SimpleClientTests.ContractSpec where

import Concordium.Client.Types.Contract

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck

contractSpec :: Spec
contractSpec = describe "contract" $ do
  printSchemaSpec
  printInfoSpec

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
