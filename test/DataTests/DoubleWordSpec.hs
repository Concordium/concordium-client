module DataTests.DoubleWordSpec where

import Data.DoubleWord
import Data.Either (isLeft)
import qualified Data.Serialize as S
import Test.Hspec
import Test.QuickCheck

tests :: Spec
tests = describe "doubleword" $ do
  word128Spec
  int128Spec

word128Spec :: Spec
word128Spec = describe "Word128" $ do
  it "decode is inverse of encode" $
    forAll genWord128 $ \w -> (S.decode . S.encode) w === Right w
  it "convert to integer is inverse of from integer" $
    forAll genWord128 $ \w -> (integerToWord128 . word128ToInteger) w === Right w
  it "parse from string: is inverse of show" $
    forAll genWord128 $ \w -> (word128FromString . show) w === Right w
  it "parse from string: fails when below min bound" $
    word128FromString (show . subtract 1 $ minBnd) `shouldSatisfy` isLeft
  it "parse from string: succeeds for min bound" $
    (word128ToInteger <$> word128FromString (show minBnd)) === Right minBnd
  it "parse from string: succeeds for max bound" $
    (word128ToInteger <$> word128FromString (show maxBnd)) === Right maxBnd
  it "parse from string: fails when above max bound" $
    word128FromString (show . (+1) $ maxBnd) `shouldSatisfy` isLeft
  it "parse from string: fails for number with decimal point" $
    word128FromString "3.3" `shouldSatisfy` isLeft
  it "parse from string: fails for string with words" $
    word128FromString "NotANumber" `shouldSatisfy` isLeft
  where minBnd = 0
        maxBnd = 340282366920938463463374607431768211455 -- 2 ^ 128

int128Spec :: Spec
int128Spec = describe "Int128" $ do
  it "decode is inverse of encode" $
    forAll genInt128 $ \w -> (S.decode . S.encode) w === Right w
  it "convert to integer is inverse of from integer" $
    forAll genInt128 $ \w -> (integerToInt128 . int128ToInteger) w === Right w
  it "parse from string: is inverse of show" $
    forAll genInt128 $ \w -> (int128FromString . show) w === Right w
  it "parse from string: fails when below min bound" $
    int128FromString (show . subtract 1 $ minBnd) `shouldSatisfy` isLeft
  it "parse from string: succeeds for min bound" $
    (int128ToInteger <$> int128FromString (show minBnd)) === Right minBnd
  it "parse from string: succeeds for max bound" $
    (int128ToInteger <$> int128FromString (show maxBnd)) === Right maxBnd
  it "parse from string: fails for " $
    (int128ToInteger <$> int128FromString (show minBnd)) === Right minBnd
  it "parse from string: succeeds for max bound" $
    (int128ToInteger <$> int128FromString (show maxBnd)) === Right maxBnd
  it "parse from string: fails when above max bound" $
    int128FromString (show . (+1) $ maxBnd) `shouldSatisfy` isLeft
  it "parse from string: fails for negative number with decimal point" $
    int128FromString "-42.2" `shouldSatisfy` isLeft
  it "parse from string: fails for positive number with decimal point" $
    int128FromString "3.3" `shouldSatisfy` isLeft
  it "parse from string: fails for string with Ints" $
    int128FromString "NotANumber" `shouldSatisfy` isLeft
  where minBnd = -170141183460469231731687303715884105728 -- -2 ^ 127
        maxBnd = 170141183460469231731687303715884105727 -- 2 ^ 127 - 1

genWord128 :: Gen Word128
genWord128 = do
  word128Lo64 <- arbitrary
  word128Hi64 <- arbitrary
  return Word128 {..}

genInt128 :: Gen Int128
genInt128 = do
  int128Lo64 <- arbitrary
  int128Hi64 <- arbitrary
  return Int128 {..}
