{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.Contract
  ( addSchemaToInfo
  , decodeSchema
  , encodeSchema
  , runGetValueAsJSON
  , serializeParams
  , Fields(..)
  , Info(..)
  , Model(..)
  , Schema(..)
  , SchemaType(..)
  , SizeLength(..)
  ) where

import Concordium.Client.Config (showCompactPrettyJSON, showPrettyJSON)
import Concordium.ID.Types (addressFromText)
import qualified Concordium.Types as T

import Control.Monad (unless, when)
import Data.Aeson (FromJSON, Result, ToJSON, (.=), (.:))
import qualified Data.Aeson as AE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Scientific (Scientific, isFloating, toBoundedInteger)
import Data.Serialize (Get, Put, Putter, Serialize, get, getInt8, getInt16le, getInt32le, getInt64le,
                       getTwoOf, getWord8, getWord16le, getWord32le, getWord64le, put, putInt8, putInt16le,
                       putInt32le, putInt64le, putTwoOf, putWord8, putWord16le, putWord32le, putWord64le)
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Word (Word32)
import GHC.Generics

-- ** Data Types and Instances **

-- |Parallel to Contract defined in contracts-common (Rust).
-- Must stay in sync.
data Schema
  =  Schema -- ^ Describes all the schemas of a smart contract.
  {  state :: Maybe SchemaType -- ^ The optional contract state
  ,  methodParameter :: HashMap Text SchemaType -- ^ Named method parameters
  }
  deriving (Eq, Show)

instance Serialize Schema where
  get = Schema <$> get <*> getMapOfWith32leLen getText get
  put Schema {..} = put state *> putMapOfWith32leLen putText put methodParameter

-- |Parallel to Fields defined in contracts-common (Rust).
-- Must stay in sync.
data Fields
  = Named [(Text, SchemaType)] -- ^ Represents an unnamed enum or struct.
  | Unnamed [SchemaType] -- ^ Represents an unnamed struct or struct.
  | Empty -- ^ The leaf node.
  deriving (Eq, Generic, Show)

instance Hashable Fields

instance ToJSON Fields

instance Serialize Fields where
  get = do
    tag <- getWord8
    case tag of
      0 -> Named <$> getListOfWith32leLen (getTwoOf getText get)
      1 -> Unnamed <$> getListOfWith32leLen get
      2 -> pure Empty
      x -> fail [i|Invalid Fields tag: #{x}|]

  put fields = case fields of
    Named pairs -> putWord8 0 *> putListOfWith32leLen (putTwoOf putText put) pairs
    Unnamed types -> putWord8 1 *> putListOfWith32leLen put types
    Empty -> putWord8 2

-- |Parallel to Type defined in contracts-common (Rust).
-- Must stay in sync.
data SchemaType =
    Unit
  | Bool
  | U8
  | U16
  | U32
  | U64
  | I8
  | I16
  | I32
  | I64
  | Amount
  | AccountAddress
  | ContractAddress
  | Pair SchemaType SchemaType
  | List SizeLength SchemaType
  | Set SizeLength SchemaType
  | Map SizeLength SchemaType SchemaType
  | Array Word32 SchemaType
  | Struct Fields
  | Enum [(Text, Fields)]
  deriving (Eq, Generic, Show)

instance Hashable SchemaType

instance ToJSON SchemaType

instance Serialize SchemaType where
  get = do
    tag <- getWord8
    case tag of
      0  -> pure Unit
      1  -> pure Bool
      2  -> pure U8
      3  -> pure U16
      4  -> pure U32
      5  -> pure U64
      6  -> pure I8
      7  -> pure I16
      8  -> pure I32
      9  -> pure I64
      10 -> pure Amount
      11 -> pure AccountAddress
      12 -> pure ContractAddress
      14 -> Pair <$> get <*> get
      16 -> List <$> get <*> get
      17 -> Set <$> get <*> get
      18 -> Map <$> get <*> get <*> get
      19 -> Array <$> getWord32le <*> get
      20 -> Struct <$> get
      21 -> Enum <$> getListOfWith32leLen (getTwoOf getText get)
      x  -> fail [i|Invalid SchemaType tag: #{x}|]

  put typ = case typ of
    Unit -> putWord8 0
    Bool -> putWord8 1
    U8   -> putWord8 2
    U16  -> putWord8 3
    U32  -> putWord8 4
    U64  -> putWord8 5
    I8   -> putWord8 6
    I16  -> putWord8 7
    I32  -> putWord8 8
    I64  -> putWord8 9
    Amount -> putWord8 10
    AccountAddress  -> putWord8 11
    ContractAddress -> putWord8 12
    Pair a b  -> putWord8 14 *> put a *> put b
    List sl a -> putWord8 16 *> put sl *> put a
    Set sl a  -> putWord8 17 *> put sl *> put a
    Map sl k v    -> putWord8 18 *> put sl *> put k *> put v
    Array len a   -> putWord8 19 *> putWord32le len *> put a
    Struct fields -> putWord8 20 *> put fields
    Enum enum     -> putWord8 21 *> putListOfWith32leLen (putTwoOf putText put) enum

-- |Parallel to SizeLength defined in contracts-common (Rust).
-- Must stay in sync.
data SizeLength
  = LenU8
  | LenU16
  | LenU32
  | LenU64
  deriving (Eq, Generic, Show)

instance ToJSON SizeLength

instance Hashable SizeLength

instance Serialize SizeLength where
  get = do
    tag <- getWord8
    case tag of
      0 -> pure LenU8
      1 -> pure LenU16
      2 -> pure LenU32
      3 -> pure LenU64
      x  -> fail [i|Invalid SizeLength tag: #{x}|]

  put sizeLen = case sizeLen of
    LenU8 -> putWord8 0
    LenU16 -> putWord8 1
    LenU32 -> putWord8 2
    LenU64 -> putWord8 3

data Info
  -- |Info about a contract.
  = Info
    { -- |The contract balance.
      ciAmount :: !T.Amount
      -- |The owner of the contract.
    , ciOwner  :: !T.AccountAddress
      -- |The contract state.
    , ciModel  :: !Model }
  deriving (Eq, Show)

instance FromJSON Info where
  parseJSON = AE.withObject "Info" $ \v -> Info
      <$> v .: "amount"
      <*> v .: "owner"
      <*> v .: "model"

instance ToJSON Info where
  toJSON Info {..} = AE.object
    [ "amount" .= ciAmount
    , "owner"  .= ciOwner
    , "model"  .= ciModel ]

data Model =
    WithSchema AE.Value  -- ^ The decoded contract state.
  | JustBytes ByteString -- ^ The binary-encoded contract state.
  deriving (Eq, Show)

instance ToJSON Model where
  toJSON (WithSchema val) = val
  toJSON (JustBytes bs) = AE.String . Text.decodeUtf8 . BS16.encode $ bs

instance FromJSON Model where
  parseJSON (AE.String s) = JustBytes <$> decodeBase16 s
    where decodeBase16 xs =
            let (parsed, remaining) = BS16.decode . Text.encodeUtf8 $ xs in
              if BS.null remaining
              then pure parsed
              else fail [i|Invalid model. Parsed: '#{parsed}', but failed on the remaining: '#{remaining}'|]
  parseJSON obj@(AE.Object _) = pure $ WithSchema obj
  parseJSON invalid = fail [i|Invalid model. Should be either a ByteString or a JSON obj: #{invalid}|]


-- ** Get and Put JSON **

getModelAsJSON :: Schema -> Get AE.Value
getModelAsJSON Schema{..} = do
  state' <- case state of
      Nothing    -> return AE.Null
      Just typ -> getValueAsJSON typ
  return $ AE.object ["state" .= state', "method_parameters" .= methodParameter]

getValueAsJSON :: SchemaType -> Get AE.Value
getValueAsJSON typ = case typ of
  Unit -> return AE.Null
  Bool -> AE.Bool <$> get
  U8   -> AE.toJSON <$> getWord8
  U16  -> AE.toJSON <$> getWord16le
  U32  -> AE.toJSON <$> getWord32le
  U64  -> AE.toJSON <$> getWord64le
  I8   -> AE.toJSON <$> getInt8
  I16  -> AE.toJSON <$> getInt16le
  I32  -> AE.toJSON <$> getInt32le
  I64  -> AE.toJSON <$> getInt64le
  Amount -> AE.toJSON . T.Amount <$> getWord64le
  AccountAddress  -> AE.toJSON <$> (get :: Get T.AccountAddress)
  ContractAddress -> AE.toJSON <$>
    (T.ContractAddress <$> (T.ContractIndex <$> getWord64le) <*> (T.ContractSubindex <$> getWord64le))
  Pair a b -> do
    l <- getValueAsJSON a
    r <- getValueAsJSON b
    return $ AE.toJSON [l, r]
  List sl elemType -> AE.toJSON <$> getListOfWithSizeLen sl (getValueAsJSON elemType)
  Set sl elemType  -> AE.toJSON <$> getListOfWithSizeLen sl (getValueAsJSON elemType)
  Map sl keyType valType -> AE.toJSON <$> getListOfWithSizeLen sl (getTwoOf (getValueAsJSON keyType) (getValueAsJSON valType))
  Array len elemType     -> AE.toJSON <$> getListOfWithKnownLen len (getValueAsJSON elemType)
  Struct fields -> AE.toJSON <$> getFieldsAsJSON fields
  Enum variants -> do
    idx <- if length variants <= 256
           then fromIntegral <$> getWord8
           else fromIntegral <$> getWord32le
    (name, fields) <- case variants !? idx of
                      Just v -> return v
                      Nothing -> fail [i|Variant with index #{idx} does not exist for Enum.|]
    fields' <- getFieldsAsJSON fields
    return $ AE.object [name .= fields']
  where
    getFieldsAsJSON :: Fields -> Get AE.Value
    getFieldsAsJSON fields = case fields of
      Named pairs -> AE.toJSON <$> traverse getPair pairs
      Unnamed xs  -> AE.toJSON <$> traverse getValueAsJSON xs
      Empty       -> return $ AE.Array mempty
      where getPair (k, v) = (\val -> AE.object [k .= val]) <$> getValueAsJSON v

    -- Slightly simplified version of Data.List.Safe.(!!)
    (!?) :: [a] -> Integer -> Maybe a
    (!?) [] _ = Nothing
    (!?) (x:xs) n | n == 0 = Just x
                  | n < 0 = Nothing
                  | otherwise = (!?) xs (n-1)

putJSONParams :: SchemaType -> AE.Value -> Either String Put
putJSONParams typ json = case (typ, json) of
  (Unit, AE.Null)     -> pure mempty
  (Bool, AE.Bool b)   -> pure $ put b
  (U8,   AE.Number x) -> putWord8    <$> fromScientific x U8
  (U16,  AE.Number x) -> putWord16le <$> fromScientific x U16
  (U32,  AE.Number x) -> putWord32le <$> fromScientific x U32
  (U64,  AE.Number x) -> putWord64le <$> fromScientific x U64
  (I8,   AE.Number x) -> putInt8     <$> fromScientific x I8
  (I16,  AE.Number x) -> putInt16le  <$> fromScientific x I16
  (I32,  AE.Number x) -> putInt32le  <$> fromScientific x I32
  (I64,  AE.Number x) -> putInt64le  <$> fromScientific x I64

  (Amount, amt@(AE.String _)) -> addTraceInfo $ putWord64le . T._amount <$> (resToEither . AE.fromJSON $ amt)

  (AccountAddress, AE.String s) -> addTraceInfo $ put <$> addressFromText s

  (ContractAddress, AE.Object obj) -> addTraceInfo $ case HM.toList obj of
    [("index", AE.Number idx)] -> putContrAddr idx 0
    [("index", AE.Number idx), ("subindex", AE.Number subidx)] -> putContrAddr idx subidx
    [("subindex", AE.Number subidx), ("index", AE.Number idx)] -> putContrAddr idx subidx
    _ -> Left [i|Invalid contract address. It should be an object with an 'index' and an optional 'subindex' field.|]

  (Pair ta tb, AE.Array vec) -> addTraceInfo $ case V.toList vec of
    [a, b] -> do
      putA <- putJSONParams ta a
      putB <- putJSONParams tb b
      pure $ putA <> putB
    _ -> Left [i|Invalid pair. It should have the form '[#{ta}, #{tb}]'.|]

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
    unless (allUnique ls) $ Left [i|All elements must be unique in a set, but got:\n#{showPrettyJSON vec}.|]
    addTraceInfo $ putListLike sl elemType ls

  (Map sl keyType valType, AE.Array vec) -> do
    let len = fromIntegral . V.length $ vec
        maxLen = maxSizeLen sl
        putLen = putLenWithSizeLen sl $ V.length vec
    when (len > maxLen) $ Left $ tooLongError "Map" maxLen len
    putElems <- traverse (putJSONParams (Pair keyType valType)) vec
    addTraceInfo $ pure . sequence_ $ V.cons putLen putElems

  (Array expectedLen elemType, AE.Array vec) -> do
    let ls = V.toList vec
        actualLen = length ls
    unless (actualLen == fromIntegral expectedLen) $ addTraceInfo
      $ Left [i|Expected length is #{expectedLen}, but actual length is #{actualLen}.|]
    addTraceInfo $ sequence_ <$> traverse (putJSONParams elemType) ls

  (Struct fields, val) -> addTraceInfo $ putJSONFields fields val

  (Enum variants, AE.Object obj) -> case HM.toList obj of
    [] -> Left [i|The object provided was empty, but it should have contained a variant of the following enum: #{variants}.|]
    [(name, fields)] -> case lookupItemAndIndex name variants of
      Nothing -> Left [i|Enum variant '#{name}' does not exist in enum #{variants}|]
      Just (fieldTypes, idx) -> do
        let putLen = if length variants <= 256
                       then putWord8 $ fromIntegral idx
                       else putWord32le $ fromIntegral idx
        putJSONFields' <- putJSONFields fieldTypes fields `addTraceInfoOf` [i|In enum variant '#{name}'.|]
        pure $ putLen <> putJSONFields'
    _ -> Left [i|#{obj} had too many fields. It should contain a single variant of the following enum: #{variants}.|]

  (type_, value) -> Left [i|Expected value of type #{type_}, but got: #{showCompactPrettyJSON value}.|]

  where
    putJSONFields :: Fields -> AE.Value -> Either String Put
    putJSONFields fields val = case (fields, val) of
      (Named pairs, AE.Array vec) -> do
        let ls = V.toList vec
        let actualLen = length ls
        let expectedLen = length pairs
        when (actualLen /= expectedLen)
          $ Left [i|#{actualLen} fields were provided, but expected #{expectedLen} fields for type: #{fields}.|]
        putNamedUnordered <- traverse (lookupAndPut pairs) ls
        unless (allUnique . map fst $ putNamedUnordered) $ Left [i|Multiple fields with the same name exist.|]
        -- The fields entered might be in a different order, so we need to order them correctly.
        pure . traverse_ snd . List.sortOn fst $ putNamedUnordered

      (Unnamed types, AE.Array vec) -> do
        let ls = V.toList vec
        let expectedLen = length types
        let actualLen = length ls
        when (actualLen /= expectedLen)
          $ Left [i|#{actualLen} fields were provided, but it should have had #{expectedLen} according to its type: #{fields}.|]
        putUnnamed <- traverse (uncurry putJSONParams) (zip types ls) `addTraceInfoOf` [i|In #{showPrettyJSON vec}.|]
        pure . sequence_ $ putUnnamed

      (Empty, AE.Array vec) -> if V.null vec
                                then pure mempty
                                else Left [i|Expected an empty array to represent Empty, but got: #{showCompactPrettyJSON val}.|]

      (type_, value) -> Left [i|Expected value of type #{type_}, but got: #{showCompactPrettyJSON value}.|]

    putListLike :: SizeLength -> SchemaType -> [AE.Value] -> Either String Put
    putListLike sl elemType xs = do
      let putLen = putLenWithSizeLen sl $ length xs
      putElems <- traverse (putJSONParams elemType) xs
      pure . sequence_ $ putLen : putElems

    putContrAddr :: Scientific -> Scientific -> Either String Put
    putContrAddr idx subidx = do
      idx' <- fromScientific idx U64
      subidx' <- fromScientific subidx U64
      pure $ putWord64le idx' <> putWord64le subidx'

    -- |The `SchemaType` should be a type of number.
    fromScientific :: (Integral i, Bounded i) => Scientific -> SchemaType -> Either String i
    fromScientific x numType = if isFloating x then Left [i|#{x} is a float, but it should have been of type #{numType}.|]
      else case toBoundedInteger x of
        Nothing -> Left [i|#{x} is out of bounds for for type #{numType}.|]
        Just x' -> Right x'

    tooLongError :: String -> Integer -> Integer -> String
    tooLongError typeName maxLen actualLen =
      [i|The provided #{typeName} is too long. It has length #{actualLen} and the maximum is #{maxLen}.|]

    maxSizeLen :: SizeLength -> Integer
    maxSizeLen = \case
      LenU8 -> 255
      LenU16 -> 65_535
      LenU32 -> 4_294_967_295
      LenU64 -> 18_446_744_073_709_551_615

    lookupAndPut :: [(Text, SchemaType)] -> AE.Value -> Either String (Int, Put)
    lookupAndPut pairs v = case v of
      AE.Object obj -> case HM.toList obj of
        [] -> Left [i|The object provided was empty, but it should have contained a named field from: #{namedFields}.|]
        [(name, value)] -> case lookupItemAndIndex name pairs of
          Nothing -> Left [i|'#{name}' is not a valid field in the type: #{Named pairs}.|]
          Just (typ', idx) -> ((idx, ) <$> putJSONParams typ' value) `addTraceInfoOf` [i|In field '#{name}'.|]
        _ -> Left [i|Invalid named field. Expected an object with a single field, but got: #{showCompactPrettyJSON v}.|]
      _ -> Left [i|#{showCompactPrettyJSON v} should have been a named field from: #{namedFields}.|]
      where namedFields = Named pairs

    lookupItemAndIndex :: Eq a => a -> [(a, b)] -> Maybe (b, Int)
    lookupItemAndIndex item thePairs = go item thePairs 0
      where go _ [] _ = Nothing
            go x ((a,b):pairs) idx = if x == a
                                    then Just (b, idx)
                                    else go x pairs (idx + 1)

    allUnique :: Eq a => [a] -> Bool
    allUnique xs = length xs == length (List.nub xs)

    resToEither :: Result a -> Either String a
    resToEither (AE.Error str) = Left str
    resToEither (AE.Success a) = Right a

    addTraceInfo :: Either String a -> Either String a
    addTraceInfo = flip addTraceInfoOf [i|In #{showPrettyJSON json}.|]

    addTraceInfoOf :: Either String a -> String -> Either String a
    addTraceInfoOf (Left err) a = Left [i|#{err}\n#{a}|]
    addTraceInfoOf right _ = right


-- ** Helpers **

-- * List *

-- Nearly identical to Data.Serialize.getListOf implementation (except for length).
getListOfWithKnownLen :: Integral len => len -> Get a -> Get [a]
getListOfWithKnownLen len ga = go [] len
  where
    go as 0 = return $! reverse as
    go as l = do x <- ga
                 x `seq` go (x:as) (l - 1)

getListOfWithSizeLen :: SizeLength -> Get a -> Get [a]
getListOfWithSizeLen sl ga = do
  len :: Integer <- case sl of
    LenU8  -> fromIntegral <$> getWord8
    LenU16 -> fromIntegral <$> getWord16le
    LenU32 -> fromIntegral <$> getWord32le
    LenU64 -> fromIntegral <$> getWord64le
  getListOfWithKnownLen len ga

getListOfWith32leLen :: Get a -> Get [a]
getListOfWith32leLen = getListOfWithSizeLen LenU32

putListOfWithSizeLen :: SizeLength -> Putter a -> Putter [a]
putListOfWithSizeLen sl pa ls = do
  putLenWithSizeLen sl $ length ls
  mapM_ pa ls

putLenWithSizeLen :: SizeLength -> Putter Int
putLenWithSizeLen sl len = case sl of
  LenU8  -> putWord8    $ fromIntegral len
  LenU16 -> putWord16le $ fromIntegral len
  LenU32 -> putWord32le $ fromIntegral len
  LenU64 -> putWord64le $ fromIntegral len

putListOfWith32leLen :: Putter a -> Putter [a]
putListOfWith32leLen = putListOfWithSizeLen LenU32


-- * Map *

getMapOfWith32leLen :: (Eq k, Hashable k) => Get k -> Get v -> Get (HashMap k v)
getMapOfWith32leLen gt gv = HM.fromList <$> getListOfWith32leLen (getTwoOf gt gv)

putMapOfWith32leLen :: Putter k -> Putter v -> Putter (HashMap k v)
putMapOfWith32leLen pv pk = putListOfWith32leLen (putTwoOf pv pk) . HM.toList


-- * Text *

getText :: Get Text
getText = Text.decodeUtf8 . BS.pack <$> getListOfWithSizeLen LenU32 get

putText :: Putter Text
putText = putListOfWithSizeLen LenU32 put . BS.unpack . Text.encodeUtf8


-- ** External API **

-- |Uses a `Schema` to parse the `JustBytes` of `Info`.
-- Returns `Left` if a schema has already been included or the parsing fails.
addSchemaToInfo :: Info -> Schema -> Either String Info
addSchemaToInfo info@Info{..} schema = case ciModel of
  WithSchema _ -> Left "Already contains a schema."
  JustBytes bytes -> (\m -> info {ciModel = WithSchema m}) <$> S.runGet (getModelAsJSON schema) bytes

-- |Serialize JSON parameters to binary using `SchemaType` or fail with an error message.
serializeParams :: SchemaType -> AE.Value -> Either String ByteString
serializeParams typ params = S.runPut <$> putJSONParams typ params

-- * For Testing *

decodeSchema :: ByteString -> Either String Schema
decodeSchema = S.decode

encodeSchema :: Schema -> ByteString
encodeSchema = S.encode

runGetValueAsJSON :: SchemaType -> ByteString -> Either String AE.Value
runGetValueAsJSON typ = S.runGet (getValueAsJSON typ)
