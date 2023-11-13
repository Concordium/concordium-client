{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A number of utility functions for parsing Wasm modules in the context of
--  Concordium smart contract tooling.
module Concordium.Client.Types.Contract.WasmParseHelpers (
    getTextLE,
    getTextWithLEB128Len,
    getListOfWithLEB128Len,
    getLEB128Word32le,
    wasmMagicHash,
    wasmSpecVersion,
    ensureWasmModule,
) where

import Control.Monad (replicateM, unless)
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word32, Word64, Word8)

-- | 4 bytes that start every valid Wasm module in binary.
wasmMagicHash :: BS.ByteString
wasmMagicHash = BS.pack [0x00, 0x61, 0x73, 0x6D]

-- | The currently supported version of the Wasm specification.
wasmSpecVersion :: BS.ByteString
wasmSpecVersion = BS.pack [0x01, 0x00, 0x00, 0x00]

-- | Parses 8 bytes and checks that the Wasm magic hash and versions are as
--  expected.
ensureWasmModule :: S.Get ()
ensureWasmModule = do
    mhBs <- S.getByteString 4
    unless (mhBs == wasmMagicHash) $ fail "Unknown magic value. This is likely not a Wasm module."
    vBs <- S.getByteString 4
    unless (vBs == wasmSpecVersion) $ fail "Unsupported Wasm standard version."

-- | Parse a UTF8 encoded string where the length prefix is encoded as 32-bit
--  little endian value. This is the default serialization of Strings produced by
--  Concordium smart contract libraries.
getTextLE :: S.Get Text
getTextLE = do
    len <- S.getWord32le
    txt <- Text.decodeUtf8' . BS.pack <$> replicateM (fromIntegral len) S.get
    case txt of
        Left err -> fail [i|Could not decode Text with LEB128 len: #{err}|]
        Right txt' -> pure txt'

-- | Get Text where the length is encoded as LEB128-Word32.
getTextWithLEB128Len :: S.Get Text
getTextWithLEB128Len = S.label "Text with LEB128 Length" $ do
    txt <- Text.decodeUtf8' . BS.pack <$> getListOfWithLEB128Len S.get
    case txt of
        Left err -> fail [i|Could not decode Text with LEB128 len: #{err}|]
        Right txt' -> pure txt'

-- \|Get a list of items where the length of the list is encoded as LEB128-Word32.
getListOfWithLEB128Len :: S.Get a -> S.Get [a]
getListOfWithLEB128Len getElem = S.label "List with LEB128 length" $ do
    len <- getLEB128Word32le
    getListOfWithKnownLen len getElem

-- \|Get a LEB128-encoded Word32. This uses an encoding compatible with the Wasm standard,
-- which means that the encoding will use at most 5 bytes.
getLEB128Word32le :: S.Get Word32
getLEB128Word32le = S.label "Word32LEB128" $ decode7 0 5 1
  where
    decode7 :: Word64 -> Word8 -> Word64 -> S.Get Word32
    decode7 acc left multiplier = do
        unless (left > 0) $ fail "Section size byte overflow"
        byte <- S.getWord8
        if Bits.testBit byte 7
            then decode7 (acc + multiplier * fromIntegral (Bits.clearBit byte 7)) (left - 1) (multiplier * 128)
            else do
                let value = acc + multiplier * fromIntegral byte
                unless (value <= fromIntegral (maxBound :: Word32)) $ fail "Section size value overflow"
                return . fromIntegral $ value

-- | Nearly identical to Data.Serialize.getListOf implementation (except for length).
getListOfWithKnownLen :: (Integral len, Show len) => len -> S.Get a -> S.Get [a]
getListOfWithKnownLen len ga = S.label ("List of known length " ++ show len) $ go [] len
  where
    go as 0 = return $! reverse as
    go as l = do
        x <- ga
        x `seq` go (x : as) (l - 1)
