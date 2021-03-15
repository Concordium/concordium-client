-- | Signed and unsigned 128-bit integer types.
--   Used as values for contract schema types UInt128 and Int128.
module Data.DoubleWord where

import qualified Data.Bits as Bits
import Data.Int (Int64)
import qualified Data.Serialize as S
import Data.Word (Word64)
import Text.Read (readMaybe)

-- | 128-bit unsigned integer type.
--   Serialization instance uses big-endian encoding as is normal for integral type Serialize instances.
data Word128 = Word128 {
  -- | Least significant 64 bits.
  word128Lo64 :: !Word64,
  -- | Most significant 64 bits.
  word128Hi64 :: !Word64
  } deriving Eq

instance Show Word128 where
  show = show . word128ToInteger

instance S.Serialize Word128 where
  get = S.label "Word128" $ do
    word128Hi64 <- S.get
    word128Lo64 <- S.get
    return Word128 {..}
  put Word128 {..} = S.put word128Hi64 <> S.put word128Lo64

-- | Get 'Word128' using little-endian encoding.
getWord128le :: S.Get Word128
getWord128le = S.label "Word128" $ do
  word128Lo64 <- S.getWord64le
  word128Hi64 <- S.getWord64le
  return Word128 {..}

-- | Put 'Word128' using little-endian encoding.
putWord128le :: S.Putter Word128
putWord128le Word128 {..} = S.putWord64le word128Lo64 <> S.putWord64le word128Hi64

-- | Parse a 'Word128' from a 'String'. Bounds are checked.
word128FromString :: String -> Either String Word128
word128FromString s = case readMaybe s of
  Nothing -> Left "Invalid Word128: Not an integer."
  Just n -> integerToWord128 n

-- | Convert a 'Word128' to an 'Integer'.
word128ToInteger :: Word128 -> Integer
word128ToInteger Word128 {..} = toInteger word128Hi64 `Bits.shiftL` 64 + toInteger word128Lo64

-- | Try to convert an 'Integer' to a 'Word128'. Bounds are checked.
integerToWord128 :: Integer -> Either String Word128
integerToWord128 x
  | x < minBnd = Left $ "Invalid Word128: Less than min bound: " ++ show minBnd
  | x > maxBnd = Left $ "Invalid Word128: Greater than max bound: " ++ show maxBnd
  | otherwise = Right $ Word128 { word128Lo64 = fromIntegral x
                                , word128Hi64 = fromIntegral $ x `Bits.shiftR` 64
                                }
  where
    minBnd = 0
    maxBnd = 2 ^ (128 :: Int) - 1

-- | 128-bit signed integer type.
--   Serialization instance uses big-endian encoding as is normal for integral type Serialize instances.
-- This uses two's complement representation to represent positive and negative numbers.
data Int128 = Int128
  {
  -- | Least significant 64 bits.
  int128Lo64 :: !Word64,
  -- | Most significant 64 bits.
  int128Hi64 :: !Word64
  } deriving Eq

instance Show Int128 where
  show = show . int128ToInteger

instance S.Serialize Int128 where
  get = S.label "Int128" $ do
    int128Hi64 <- S.get
    int128Lo64 <- S.get
    return Int128 {..}
  put Int128 {..} = S.put int128Hi64 <> S.put int128Lo64

-- | Get 'Int128' using little-endian encoding.
getInt128le :: S.Get Int128
getInt128le = S.label "Int128" $ do
  int128Lo64 <- S.getWord64le
  int128Hi64 <- S.getWord64le
  return Int128 {..}

-- | Put 'Int128' using little-endian encoding.
putInt128le :: S.Putter Int128
putInt128le Int128 {..} = S.putWord64le int128Lo64 <> S.putWord64le int128Hi64

-- | Convert an 'Int128' to an 'Integer'.
int128ToInteger :: Int128 -> Integer
int128ToInteger Int128 {..} = toInteger (fromIntegral int128Hi64 :: Int64) `Bits.shiftL` 64 + toInteger int128Lo64

-- | Try to convert an 'Integer' to an 'Int128'. Bounds are checked.
integerToInt128 :: Integer -> Either String Int128
integerToInt128 x
  | x < minBnd = Left $ "Invalid Int128: Less than min bound: " ++ show minBnd
  | x > maxBnd = Left $ "Invalid Int128: Greater than max bound: " ++ show maxBnd
  | otherwise = Right $ Int128 { int128Lo64 = fromIntegral x
                               , int128Hi64 = fromIntegral $ x `Bits.shiftR` 64
                               }
  where
    minBnd = -2 ^ (127 :: Int)
    maxBnd = 2 ^ (127 :: Int) - 1

-- | Parse an 'Int128' from a 'String'. Bounds are checked.
int128FromString :: String -> Either String Int128
int128FromString s = case readMaybe s of
  Nothing -> Left "Invalid Int128: Not an integer."
  Just n -> integerToInt128 n
