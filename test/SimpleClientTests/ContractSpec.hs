module SimpleClientTests.ContractSpec where

import Concordium.Client.Types.Contract

import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec
import Test.QuickCheck

contractSpec :: Spec
contractSpec = describe "contract" $ do
  printDecodeSpec

printDecodeSpec :: Spec
printDecodeSpec = describe "decodeSchema" $ do
  it "is inverse to encodeSchema" $ withMaxSuccess 50 $
    forAll (sized genSchema) $ \c -> (decodeSchema . encodeSchema) c === Right c

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
      [ pure RsU8
      , pure RsU16
      , pure RsU32
      , pure RsU64
      , pure RsString
      , pure RsUnit
      , pure RsBool
      , pure RsBytes
      , RsPair <$> genRsType n' <*> genRsType n'
      , RsStruct <$> genFields n'
      , RsEnum <$> listOf (genTwoOf genText (genFields n'))
      , RsList <$> genRsType n'
      , RsMap <$> genRsType n' <*> genRsType n'
      , RsSet <$> genRsType n'
      , RsOption <$> genRsType n'
      , pure RsAccountAddress
      , pure RsContractAddress
      , RsArray <$> arbitrary <*> genRsType n'
      , pure RsI8
      , pure RsI16
      , pure RsI32
      , pure RsI64 ]
  where n' = nextNSize n

genTwoOf :: Gen a -> Gen b -> Gen (a, b)
genTwoOf ga gb = (,) <$> ga <*> gb

genText :: Gen Text
genText = Text.pack <$> arbitrary

-- The factor here determines how large generated test data will become (large factor => small test data).
nextNSize :: Int -> Int
nextNSize = (`div` 3)
