{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.Contract.Parameter where

import Concordium.Client.Config (showCompactPrettyJSON, showPrettyJSON)
import qualified Concordium.Types as T
import Concordium.Client.Types.Contract.Schema
import Concordium.Client.Utils
import qualified Concordium.Wasm as Wasm
import qualified Data.DoubleWord as DW

import Control.Monad (unless, when, zipWithM)
import Data.Aeson (FromJSON, Result, ToJSON, (.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Key as AE
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AE
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, isFloating, toBoundedInteger)
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Lens.Micro.Platform ((^?), ix)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Time (addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Ratio
import qualified Data.Bits as Bits
import Text.Read (readMaybe)
import GHC.Integer (remInteger, modInteger)

-- |Serialize JSON parameter to binary using `SchemaType` or fail with an error message.
encodeParameter :: SchemaType -> AE.Value -> Either String ByteString
encodeParameter typ params = S.runPut <$> putJSONUsingSchema typ params

-- |Create a `Serialize.Get` for decoding binary as specified by a `SchemaType` into JSON.
-- The `SchemaType` is pattern matched and for each variant, the corresponding binary
-- deserialization is used followed by the corresponding JSON serialization.
-- The Value that is returned should match what is expected from `putJSONUsingSchema` when using the same schema.
getJSONUsingSchema :: SchemaType -> S.Get AE.Value
getJSONUsingSchema typ = case typ of
  Unit -> return AE.Null
  Bool -> AE.Bool <$> S.get
  UInt8   -> AE.toJSON <$> S.getWord8
  UInt16  -> AE.toJSON <$> S.getWord16le
  UInt32  -> AE.toJSON <$> S.getWord32le
  UInt64  -> AE.toJSON <$> S.getWord64le
  Int8   -> AE.toJSON <$> S.getInt8
  Int16  -> AE.toJSON <$> S.getInt16le
  Int32  -> AE.toJSON <$> S.getInt32le
  Int64  -> AE.toJSON <$> S.getInt64le
  Amount -> AE.toJSON <$> (S.get :: S.Get AmountLE)
  AccountAddress  -> AE.toJSON <$> (S.get :: S.Get T.AccountAddress)
  ContractAddress -> AE.toJSON <$>
    (T.ContractAddress <$> (T.ContractIndex <$> S.getWord64le) <*> (T.ContractSubindex <$> S.getWord64le))
  Timestamp -> AE.toJSON . timestampToRFC3339 <$> S.getWord64le
  Duration -> AE.toJSON . durationToText <$> S.getWord64le
  Pair a b -> do
    l <- getJSONUsingSchema a
    r <- getJSONUsingSchema b
    return $ AE.toJSON [l, r]
  List sl elemType -> AE.toJSON <$> getListOfWithSizeLen sl (getJSONUsingSchema elemType)
  Set sl elemType  -> AE.toJSON <$> getListOfWithSizeLen sl (getJSONUsingSchema elemType)
  Map sl keyType valType -> AE.toJSON <$> getListOfWithSizeLen sl (S.getTwoOf (getJSONUsingSchema keyType) (getJSONUsingSchema valType))
  Array len elemType     -> AE.toJSON <$> getListOfWithKnownLen len (getJSONUsingSchema elemType)
  Struct fields -> AE.toJSON <$> getFieldsAsJSON fields
  Enum variants -> do
    idx <- if length variants <= 255
           then fromIntegral <$> S.getWord8
           else fromIntegral <$> S.getWord32le
    (name, fields) <- case variants ^? ix idx of
                      Just v -> return v
                      Nothing -> fail [i|Variant with index #{idx} does not exist for Enum.|]
    fields' <- getFieldsAsJSON fields
    return $ AE.object [AE.fromText name .= fields']
  String sl -> AE.toJSON <$> getUtf8String sl
  UInt128 -> AE.toJSON . show <$> DW.getWord128le
  Int128 -> AE.toJSON . show <$> DW.getInt128le
  ContractName sl -> do
    rawName <- getUtf8String sl
    case Text.stripPrefix "init_" rawName of
      Just contractName -> return $ AE.object ["contract" .= contractName]
      Nothing -> fail [i|Expect contract name with format: 'init_<contract_name>', but got: #{rawName}.|]
  ReceiveName sl -> do
    rawName <- getUtf8String sl
    let separatorIndex = Text.findIndex (== '.') rawName
    case separatorIndex of
      Nothing -> fail [i|Expected format '<contract_name>.<func_name>' but got: #{rawName}.|]
      Just idx -> let (contractName, funcNameWithDot) = Text.splitAt idx rawName
                      funcName = Text.tail funcNameWithDot -- Safe, since we know it contains a dot.
                  in return $ AE.object ["contract" .= contractName, "func" .= funcName]
  ULeb128 constraint -> AE.toJSON . show <$> leb128 0
    where
      leb128 :: Int -> S.Get Integer
      leb128 byteCount = do
        unless (byteCount < fromIntegral constraint) $ fail "Leb128 encoding violated the constraint on the number of bytes"
        byte <- S.getWord8
        let value = fromIntegral (Bits.clearBit byte 7)
        let moreBytes = Bits.testBit byte 7
        if moreBytes
          then do
            next <- leb128 (byteCount + 1)
            return $! value + 128 * next
          else return value
  ILeb128 constraint -> do
    AE.toJSON . show <$> ileb128 0 0
    where
      ileb128 :: Int -> Integer -> S.Get Integer
      ileb128 byteCount acc = do
        unless (byteCount < fromIntegral constraint) $ fail "Leb128 encoding violated the constraint on the number of bytes"
        byte <- S.getWord8
        let byteValue = fromIntegral (Bits.clearBit byte 7)
        let value = acc + (2 ^ (7 * byteCount)) * byteValue
        let moreBytes = Bits.testBit byte 7
        if moreBytes
          then do
            ileb128 (byteCount + 1) value
          else let isNegative = Bits.testBit byte 6
               in return $! if isNegative
                              then value - 2 ^ ((byteCount + 1) * 7)
                              else value
  ByteList sl -> do
    byteList <- getListOfWithSizeLen sl S.getWord8
    return $ AE.toJSON $ toHex $ BS.pack byteList
  ByteArray len -> do
    bytes <- S.getByteString (fromIntegral len)
    return $ AE.toJSON $ toHex bytes

  where
    -- Notice we can safely use the partial function decodeUft8, since the output of BS16.encode is always a valid UFT8 string
    toHex = Text.decodeUtf8 . BS16.encode
    getFieldsAsJSON :: Fields -> S.Get AE.Value
    getFieldsAsJSON fields = case fields of
      Named pairs -> AE.toJSON . Map.fromList <$> mapM getPair pairs
      Unnamed xs  -> AE.toJSON <$> mapM getJSONUsingSchema xs
      None        -> return $ AE.Array mempty
      where getPair (k, v) = (k,) <$> getJSONUsingSchema v

    -- |Converts a unix timestamp in milliseconds to UTC in ISO8601/RFC3339-format.
    -- Example: 220966827870 -> "1977-01-01T11:40:27.87Z"
    timestampToRFC3339 :: Word64 -> Text
    timestampToRFC3339 millis = Text.pack . iso8601Show $ utcTime
      where utcTime = addUTCTime diffTime (posixSecondsToUTCTime 0)
            diffTime = fromRational (toInteger millis % 1000)


    -- |Gets a string of the specified SizeLength. Fails if the string is not valid UTF8.
    getUtf8String :: SizeLength -> S.Get Text
    getUtf8String sl = do
      bStr <- BS.pack <$> getListOfWithSizeLen sl S.get
      case Text.decodeUtf8' bStr of
        Left _ -> fail "String is not valid UTF-8."
        Right str -> return str

-- |Create a `Serialize.Put` for JSON using a `SchemaType`.
-- It goes through the JSON and SchemaType recursively, and
-- deserializes the JSON before serializing the values to binary.
-- A descriptive error message is shown if the JSON does not match
-- the expected format as specified by the `SchemaType`.
putJSONUsingSchema :: SchemaType -> AE.Value -> Either String S.Put
putJSONUsingSchema typ json = case (typ, json) of
  (Unit, AE.Null)     -> pure mempty
  (Bool, AE.Bool b)   -> pure $ S.put b
  (UInt8,   AE.Number x) -> S.putWord8    <$> fromScientific x UInt8
  (UInt16,  AE.Number x) -> S.putWord16le <$> fromScientific x UInt16
  (UInt32,  AE.Number x) -> S.putWord32le <$> fromScientific x UInt32
  (UInt64,  AE.Number x) -> S.putWord64le <$> fromScientific x UInt64
  (Int8,   AE.Number x) -> S.putInt8     <$> fromScientific x Int8
  (Int16,  AE.Number x) -> S.putInt16le  <$> fromScientific x Int16
  (Int32,  AE.Number x) -> S.putInt32le  <$> fromScientific x Int32
  (Int64,  AE.Number x) -> S.putInt64le  <$> fromScientific x Int64

  (Amount, amt@(AE.String _)) -> addTraceInfo $ (S.put :: S.Putter AmountLE) <$> (resToEither . AE.fromJSON $ amt)

  (AccountAddress, v@(AE.String _)) -> addTraceInfo $ (S.put :: S.Putter T.AccountAddress) <$> AE.parseEither AE.parseJSON v

  (ContractAddress, AE.Object obj) -> addTraceInfo $ case KM.toList obj of
    [("index", AE.Number idx)] -> putContrAddr idx 0
    [("index", AE.Number idx), ("subindex", AE.Number subidx)] -> putContrAddr idx subidx
    [("subindex", AE.Number subidx), ("index", AE.Number idx)] -> putContrAddr idx subidx
    _ -> Left [i|Invalid contract address. It should be an object with an 'index' and an optional 'subindex' field.|]

  (Timestamp, AE.String s) -> S.putWord64le <$> rfc3339ToTimestamp s

  (Duration, AE.String s) -> S.putWord64le <$> textToDuration s

  (Pair ta tb, AE.Array vec) -> addTraceInfo $ case V.toList vec of
    [a, b] -> do
      putA <- putJSONUsingSchema ta a
      putB <- putJSONUsingSchema tb b
      pure $ putA <> putB
    _ -> Left [i|Invalid pair. It should have the form: [#{showCompactPrettyJSON ta}, #{showCompactPrettyJSON tb}].|]

  (List sl elemType, AE.Array vec) -> do
    let len = fromIntegral . V.length $ vec
        maxLen = maxSizeLen sl
    when (len > maxLen) $ Left $ tooLongError "List" maxLen len
    addTraceInfo $ putListLike sl elemType (V.toList vec)

  (Set sl elemType, AE.Array vec) -> do
    let len = fromIntegral . V.length $ vec
        maxLen = maxSizeLen sl
        ls = V.toList vec
    when (len > maxLen) $ Left $ tooLongError "Set" maxLen len
    addTraceInfo $ putListLike sl elemType ls

  (Map sl keyType valType, AE.Array vec) -> do
    let len = fromIntegral . V.length $ vec
        maxLen = maxSizeLen sl
        putLen = putLenWithSizeLen sl $ V.length vec
    when (len > maxLen) $ Left $ tooLongError "Map" maxLen len
    putElems <- mapM (putJSONUsingSchema (Pair keyType valType)) vec
    addTraceInfo $ pure . sequence_ $ V.cons putLen putElems

  (Array expectedLen elemType, AE.Array vec) -> do
    let ls = V.toList vec
        actualLen = length ls
    unless (actualLen == fromIntegral expectedLen) $ addTraceInfo
      $ Left [i|Expected length is #{expectedLen}, but actual length is #{actualLen}.|]
    addTraceInfo $ sequence_ <$> mapM (putJSONUsingSchema elemType) ls

  (Struct fields, val) -> addTraceInfo $ putJSONFields fields val

  (enum@(Enum variants), AE.Object obj) -> case KM.toList obj of
    [] -> Left [i|The object provided was empty, but it should have contained a variant of the following enum:\n#{showPrettyJSON enum}.|]
    [(name, fields)] -> case lookupItemAndIndex name $ map (first AE.fromText) variants of
      Nothing -> Left [i|Enum variant '#{name}' does not exist in:\n#{showPrettyJSON enum}|]
      Just (fieldTypes, idx) -> do
        let putLen = if length variants <= 255
                       then S.putWord8 $ fromIntegral idx
                       else S.putWord32le $ fromIntegral idx
        putJSONFields' <- putJSONFields fieldTypes fields `addTraceInfoOf` [i|In enum variant '#{name}'.|]
        pure $ putLen <> putJSONFields'
    _ -> Left [i|#{obj} had too many fields. It should contain a single variant of the following enum:\n#{showPrettyJSON enum}.|]

  (String sl, AE.String str) -> do
    let bytes = BS.unpack . Text.encodeUtf8 $ str
        len = fromIntegral $ length bytes
        maxLen = maxSizeLen sl
    when (len > maxLen) $ Left $ tooLongError "String" maxLen len
    let putLen = putLenWithSizeLen sl $ fromIntegral len
    let putBytes = mapM_ S.put bytes
    pure $ putLen <> putBytes

  (UInt128, AE.String str) -> do
    case DW.word128FromString $ Text.unpack str of
      Left err -> Left [i|Invalid UInt128 '#{str}': #{err}|]
      Right n -> pure $ DW.putWord128le n

  (Int128, AE.String str) -> do
    case DW.int128FromString $ Text.unpack str of
      Left err -> Left [i|Invalid Int128 '#{str}': #{err}|]
      Right n -> pure $ DW.putInt128le n

  (ContractName sl, AE.Object obj) -> do
    let fieldCount = length . KM.toList $ obj
    when (fieldCount /= 1) $ Left [i|Expected object with a single field 'contract', but got: #{showPrettyJSON obj}.|]
    case KM.lookup "contract" obj of
      Just (AE.String contractName) -> do
        let nameWithInit = "init_" <> contractName
            bytes = BS.unpack . Text.encodeUtf8 $ nameWithInit
            len = fromIntegral $ length bytes
            maxLen = maxSizeLen sl
        unless (Wasm.isValidInitName nameWithInit) $
          -- Include 'obj' in the error to be explicit about where the error occured.
          Left [i|'#{contractName}' is not a valid contract name.\nIn #{showPrettyJSON obj}.|]
        when (len > maxLen) $ Left $ tooLongError "ContractName" maxLen len
        let putLen = putLenWithSizeLen sl $ fromIntegral len
        let putBytes = mapM_ S.put bytes
        pure $ putLen <> putBytes
      _ -> Left [i|Expected object with a field 'contract' of type String, but got: #{showPrettyJSON obj}.|]

  (ReceiveName sl, AE.Object obj) -> do
    let fieldCount = length . KM.toList $ obj
    when (fieldCount /= 2) $ Left [i|Expected object with two fields 'contract' and 'func', but got: #{showPrettyJSON obj}.|]
    case (KM.lookup "contract" obj, KM.lookup "func" obj) of
      (Just (AE.String contractName), Just (AE.String funcName)) -> do
        let nameWithInit = "init_" <> contractName
            nameWithDot = contractName <> "." <> funcName
            bytes = BS.unpack . Text.encodeUtf8 $ nameWithDot
            len = fromIntegral $ length bytes
            maxLen = maxSizeLen sl
        -- Include 'obj' in the following errors to be explicit about where the errors occured.
        unless (Wasm.isValidInitName nameWithInit) $ -- This check ensures that the contract name is valid on its own.
          Left [i|'#{contractName}' is not a valid contract name.\nIn #{showPrettyJSON obj}.|]
        unless (Wasm.isValidReceiveName nameWithDot) $
          Left [i|'#{contractName}' combined with '#{funcName}' is not a valid receive name.\nIn #{showPrettyJSON obj}.|]
        when (len > maxLen) $ Left $ tooLongError "ReceiveName" maxLen len
        let putLen = putLenWithSizeLen sl $ fromIntegral len
        let putBytes = mapM_ S.put bytes
        pure $ putLen <> putBytes
      _ -> Left [i|Expected object with fields 'contract' and 'func' of type String, but got: #{showPrettyJSON obj}.|]
  (ULeb128 constraint, AE.String str) -> do
    value :: Integer <- maybe (Left [i|Expected string containing an unsigned integer, but got: #{showPrettyJSON str}.|]) Right $ readMaybe $ Text.unpack str
    unless (value >= 0) (Left [i|Expected string containing a positive integer, but got: #{showPrettyJSON str}.|])
    uleb128 0 value
    where
      uleb128 :: Int -> Integer -> Either String S.Put
      uleb128 byteCount value = do
        unless (byteCount < fromIntegral constraint) (Left [i|The encoding of the unsigned integer uses more than #{show constraint} bytes and violates the constraint.|])
        let byte :: Word8 = fromIntegral $ value `remInteger` 128
        let remainingValue = value `Bits.shiftR` 7
        let noMore = remainingValue == 0
        if noMore then
          return $ S.putWord8 byte
        else do
          let putByte = S.putWord8 $ byte `Bits.setBit` 7
          remainingBytes <- uleb128 (byteCount + 1) remainingValue
          return $ putByte <> remainingBytes

  (ILeb128 constraint, AE.String str) -> do
    value :: Integer <- maybe (Left [i|Expected string containing a signed integer, but got: #{showPrettyJSON str}.|]) Right $ readMaybe $ Text.unpack str
    ileb128 0 value
    where
      ileb128 :: Int -> Integer -> Either String S.Put
      ileb128 byteCount value = do
        unless (byteCount < fromIntegral constraint) (Left [i|The encoding of the signed integer uses more than #{show constraint} bytes and violates the constraint.|])
        let byte :: Word8 = fromIntegral $ value `modInteger` 128
        let remainingValue = value `Bits.shiftR` 7
        let noMore = (remainingValue == 0 && not (byte `Bits.testBit` 6)) || (remainingValue == -1 && byte `Bits.testBit` 6)
        if noMore then
          return $ S.putWord8 byte
        else do
          let putByte = S.putWord8 $ byte `Bits.setBit` 7
          remainingBytes <- ileb128 (byteCount + 1) remainingValue
          return $ putByte <> remainingBytes

  (ByteList sl, AE.String str) -> do
    bytes <- BS16.decode $ Text.encodeUtf8 str
    let len = fromIntegral $ BS.length bytes
    let maxLen = maxSizeLen sl
    when (len > maxLen) $ Left $ tooLongError "ByteList" (fromIntegral maxLen) (fromIntegral len)
    let putLen = putLenWithSizeLen sl $ fromIntegral len
    let putBytes = mapM_ S.put $ BS.unpack bytes
    return $ putLen <> putBytes

  (ByteArray expectedLen, AE.String str) -> do
    bytes <- BS16.decode (Text.encodeUtf8 str)
    let actualLen = fromIntegral $ BS.length bytes
    unless (actualLen == expectedLen) $ addTraceInfo
      $ Left [i|Expected length is #{expectedLen}, but actual length is #{actualLen}.|]
    return $ mapM_ S.put $ BS.unpack bytes

  (type_, value) -> Left [i|Expected value of type #{showCompactPrettyJSON type_}, but got: #{showCompactPrettyJSON value}.|]

  where
    putJSONFields :: Fields -> AE.Value -> Either String S.Put
    putJSONFields fields val = case (fields, val) of
      (Named pairs, AE.Object obj) -> do
        let ls = KM.toList obj
        let actualLen = length ls
        let expectedLen = length pairs
        when (actualLen /= expectedLen)
          $ Left [i|#{actualLen} fields were provided, but expected #{expectedLen} fields for type:\n#{showPrettyJSON fields}.|]
        putNamedUnordered <- mapM (lookupAndPut pairs) ls
        -- The fields entered might be in a different order, so we need to order them correctly.
        pure . mapM_ snd . List.sortOn fst $ putNamedUnordered

      (Unnamed types, AE.Array vec) -> do
        let ls = V.toList vec
        let expectedLen = length types
        let actualLen = length ls
        when (actualLen /= expectedLen)
          $ Left [i|#{actualLen} fields were provided, but it should have had #{expectedLen} according to its type:\n#{showPrettyJSON fields}.|]
        putUnnamed <- zipWithM putJSONUsingSchema types ls `addTraceInfoOf` [i|In #{showPrettyJSON vec}.|]
        pure . sequence_ $ putUnnamed

      (None, AE.Array vec) -> if V.null vec
                                then pure mempty
                                else Left [i|Expected an empty array to represent None, but got: #{showCompactPrettyJSON val}.|]

      (type_, value) -> Left [i|Expected value of type #{showCompactPrettyJSON type_}, but got: #{showCompactPrettyJSON value}.|]

    putListLike :: SizeLength -> SchemaType -> [AE.Value] -> Either String S.Put
    putListLike sl elemType xs = do
      let putLen = putLenWithSizeLen sl $ length xs
      putElems <- mapM (putJSONUsingSchema elemType) xs
      pure . sequence_ $ putLen : putElems

    putContrAddr :: Scientific -> Scientific -> Either String S.Put
    putContrAddr idx subidx = do
      idx' <- fromScientific idx UInt64
      subidx' <- fromScientific subidx UInt64
      pure $ S.putWord64le idx' <> S.putWord64le subidx'

    -- |The `SchemaType` should be a type of number.
    fromScientific :: (Integral i, Bounded i) => Scientific -> SchemaType -> Either String i
    fromScientific x numType = if isFloating x then Left [i|#{x} is a float, but it should have been of type #{numType}.|]
      else case toBoundedInteger x of
        Nothing -> Left [i|#{x} is out of bounds for type #{numType}.|]
        Just x' -> Right x'

    tooLongError :: String -> Integer -> Integer -> String
    tooLongError typeName maxLen actualLen =
      [i|The provided #{typeName} is too long. It has length #{actualLen} and the maximum is #{maxLen}.|]

    maxSizeLen :: SizeLength -> Integer
    maxSizeLen = \case
      One   -> toInteger (maxBound :: Word8)
      Two   -> toInteger (maxBound :: Word16)
      Four  -> toInteger (maxBound :: Word32)
      Eight -> toInteger (maxBound :: Word64)

    lookupAndPut :: [(Text, SchemaType)]       -- ^ The names and types for Named Fields.
                 -> (AE.Key, AE.Value)         -- ^ A field name and a value.
                 -> Either String (Int, S.Put) -- ^ The index of the field in the particular Named Fields,
                                               --   used for subsequent ordering,
                                               --   and a putter for the value (or an error message).
    lookupAndPut types (key, value) =
      case lookupItemAndIndex name types of
        Nothing -> Left [i|'#{name}' is not a valid field in the type:\n#{showPrettyJSON (Named types)}.|]
        Just (typ', idx) -> ((idx, ) <$> putJSONUsingSchema typ' value) `addTraceInfoOf` [i|In field '#{name}'.|]
      where
        name = AE.toText key

    lookupItemAndIndex :: Eq a => a -> [(a, b)] -> Maybe (b, Int)
    lookupItemAndIndex item thePairs = go item thePairs 0
      where go _ [] _ = Nothing
            go x ((a,b):pairs) idx = if x == a
                                    then Just (b, idx)
                                    else go x pairs (idx + 1)

    resToEither :: Result a -> Either String a
    resToEither (AE.Error str) = Left str
    resToEither (AE.Success a) = Right a

    addTraceInfo :: Either String a -> Either String a
    addTraceInfo = flip addTraceInfoOf [i|In #{showPrettyJSON json}.|]

    addTraceInfoOf :: Either String a -> String -> Either String a
    addTraceInfoOf (Left err) a = Left [i|#{err}\n#{a}|]
    addTraceInfoOf right _ = right

    -- |Converts a date in RFC3339-format to a unix timestamp in milliseconds.
    -- Returns an error message if input is invalid RFC3339 or the date is prior to '1970-01-01T00:00:00Z'.
    -- Example: "1977-01-01T12:00:27.87+00:20" -> Right 220966827870
    rfc3339ToTimestamp :: Text -> Either String Word64
    rfc3339ToTimestamp s = case iso8601ParseM timeString of
      Nothing ->
        case iso8601ParseM timeString of
          Nothing -> Left [i|Invalid timestamp '#{s}'. Should be in a RFC3339 format.|]
          Just utcTime -> utcTimeToTimestamp utcTime
      Just zonedTime -> utcTimeToTimestamp . Time.zonedTimeToUTC $ zonedTime
      where utcTimeToTimestamp :: Time.UTCTime -> Either String Word64
            utcTimeToTimestamp t
              | millis < 0 = Left [i|Invalid timestamp '#{s}'. Dates before '1970-01-01T00:00:00Z' are not supported|]
              | millis > toInteger (maxBound :: Word64) = Left "Timestamp too far in the future."
              | otherwise = Right (fromIntegral millis)
              where frac = 1000 * toRational (diffUTCTime t (posixSecondsToUTCTime 0)) -- conversion functions give seconds
                    millis = numerator frac `div` denominator frac
            timeString = Text.unpack s

-- |Wrapper for Concordium.Types.Amount that uses a little-endian encoding
-- for binary serialization. Show and JSON instances are inherited from
-- the Amount type.
newtype AmountLE = AmountLE T.Amount
  deriving Eq
  deriving newtype (FromJSON, Show, ToJSON)

instance S.Serialize AmountLE where
  get = S.label "AmountLE" $ AmountLE . T.Amount <$> S.getWord64le
  put (AmountLE T.Amount{..}) = S.putWord64le _amount
