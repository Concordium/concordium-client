{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.ContractSchema
  ( addSchemaToInfo
  , contractNameFromInitName
  , decodeEmbeddedSchema
  , decodeSchema
  , getValueAsJSON
  , lookupSignatureForFunc
  , methodNameFromReceiveName
  , putJSONParams
  , serializeParams
  , Contract(..)
  , Fields(..)
  , FuncName(..)
  , Info(..)
  , Model(..)
  , Module(..)
  , SchemaType(..)
  , SizeLength(..)
  ) where

import Concordium.Client.Config (showCompactPrettyJSON, showPrettyJSON)
import qualified Concordium.Types as T
import qualified Concordium.Wasm as Wasm

import Control.Monad (unless, when, zipWithM)
import Data.Aeson (FromJSON, Result, ToJSON, (.=), (.:))
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, isFloating, toBoundedInteger)
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Word (Word16, Word8, Word32, Word64)
import GHC.Generics
import Lens.Micro.Platform ((^?), ix)

-- ** Data Types and Instances **

-- |Parallel to Module defined in contracts-common (Rust).
-- Must stay in sync.
newtype Module
  = Module { contracts :: Map Text Contract }
  deriving (Eq, Show, Generic)

instance ToJSON Module

instance S.Serialize Module where
  get = S.label "Module" $ Module <$> getMapOfWith32leLen getText S.get
  put Module {..} = putMapOfWith32leLen putText S.put contracts

-- |Parallel to Contract defined in contracts-common (Rust).
-- Must stay in sync.
data Contract
  =  Contract -- ^ Describes the schemas of a smart contract.
  {  state :: Maybe SchemaType -- ^ The optional contract state
  ,  initSig :: Maybe SchemaType -- ^ Type signature for the init function
  ,  receiveSigs :: Map Text SchemaType -- ^ Type signatures for the receive functions
  }
  deriving (Eq, Show, Generic)

instance ToJSON Contract

instance S.Serialize Contract where
  get = S.label "Contract" $ do
    state <- S.label "state" S.get
    initSig <- S.label "initSig" S.get
    receiveSigs <- S.label "receiveSigs" $ getMapOfWith32leLen getText S.get
    pure Contract{..}
  put Contract {..} = S.put state <> S.put initSig <> putMapOfWith32leLen putText S.put receiveSigs

-- |Parallel to Fields defined in contracts-common (Rust).
-- Must stay in sync.
data Fields
  = Named [(Text, SchemaType)] -- ^ Represents a named enum or struct.
  | Unnamed [SchemaType] -- ^ Represents an unnamed enum or struct.
  | None -- ^ Represents an empty enum or struct.
  deriving (Eq, Generic, Show)

instance Hashable Fields

instance ToJSON Fields where
  toJSON (Named fields) = AE.object . map (\(name, value) -> name .= value) $ fields
  toJSON (Unnamed fields) = AE.toJSON fields
  toJSON None = AE.Array V.empty

instance S.Serialize Fields where
  get = S.label "Fields" $ do
    tag <- S.getWord8
    case tag of
      0 -> S.label "Named" $ Named <$> getListOfWith32leLen (S.getTwoOf getText S.get)
      1 -> S.label "Unnamed" $ Unnamed <$> getListOfWith32leLen S.get
      2 -> S.label "None" $ pure None
      x -> fail [i|Invalid Fields tag: #{x}|]

  put fields = case fields of
    Named pairs -> S.putWord8 0 <> putListOfWith32leLen (S.putTwoOf putText S.put) pairs
    Unnamed types -> S.putWord8 1 <> putListOfWith32leLen S.put types
    None -> S.putWord8 2

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

-- |This should match the format used in `getValueAsJSON` so the user can copy this and use it for
-- creating a parameter file in json format. The only difference is for enums, in which all of its
-- variants are shown here, but only one should be used in the parameter file.
instance ToJSON SchemaType where
  toJSON = \case
    Struct fields -> AE.toJSON fields
    Enum variants -> AE.object ["Enum" .= (AE.Array . V.fromList . map (\(k, v) -> AE.object [k .= v]) $ variants)]
    Pair typA typB -> AE.Array . V.fromList $ [AE.toJSON typA, AE.toJSON typB]
    x -> AE.String . Text.pack . show $ x

instance S.Serialize SchemaType where
  get = S.label "SchemaType" $ do
    tag <- S.label "tag" S.getWord8
    case tag of
      0  -> S.label "Unit" $ pure Unit
      1  -> S.label "Bool" $ pure Bool
      2  -> S.label "U8"   $ pure U8
      3  -> S.label "U16"  $ pure U16
      4  -> S.label "U32"  $ pure U32
      5  -> S.label "U64"  $ pure U64
      6  -> S.label "I8"   $ pure I8
      7  -> S.label "I16"  $ pure I16
      8  -> S.label "I32"  $ pure I32
      9  -> S.label "I64"  $ pure I64
      10 -> S.label "Amount" $ pure Amount
      11 -> S.label "AccountAddress"  $ pure AccountAddress
      12 -> S.label "ContractAddress" $ pure ContractAddress
      13 -> S.label "Pair"   $ Pair <$> S.get <*> S.get
      14 -> S.label "List"   $ List <$> S.get <*> S.get
      15 -> S.label "Set"    $ Set <$> S.get <*> S.get
      16 -> S.label "Map"    $ Map <$> S.get <*> S.get <*> S.get
      17 -> S.label "Array"  $ Array <$> S.getWord32le <*> S.get
      18 -> S.label "Struct" $ Struct <$> S.get
      19 -> S.label "Enum"   $ Enum <$> getListOfWith32leLen (S.getTwoOf getText S.get)
      x  -> fail [i|Invalid SchemaType tag: #{x}|]

  put typ = case typ of
    Unit -> S.putWord8 0
    Bool -> S.putWord8 1
    U8   -> S.putWord8 2
    U16  -> S.putWord8 3
    U32  -> S.putWord8 4
    U64  -> S.putWord8 5
    I8   -> S.putWord8 6
    I16  -> S.putWord8 7
    I32  -> S.putWord8 8
    I64  -> S.putWord8 9
    Amount -> S.putWord8 10
    AccountAddress  -> S.putWord8 11
    ContractAddress -> S.putWord8 12
    Pair a b  -> S.putWord8 13 <> S.put a <> S.put b
    List sl a -> S.putWord8 14 <> S.put sl <> S.put a
    Set sl a  -> S.putWord8 15 <> S.put sl <> S.put a
    Map sl k v    -> S.putWord8 16 <> S.put sl <> S.put k <> S.put v
    Array len a   -> S.putWord8 17 <> S.putWord32le len <> S.put a
    Struct fields -> S.putWord8 18 <> S.put fields
    Enum enum     -> S.putWord8 19 <> putListOfWith32leLen (S.putTwoOf putText S.put) enum

-- |Parallel to SizeLength defined in contracts-common (Rust).
-- Must stay in sync.
data SizeLength
  = LenU8
  | LenU16
  | LenU32
  | LenU64
  deriving (Eq, Generic, Show)

instance ToJSON SizeLength where
  toJSON LenU8  = AE.String "LenU8"
  toJSON LenU16 = AE.String "LenU16"
  toJSON LenU32 = AE.String "LenU32"
  toJSON LenU64 = AE.String "LenU64"

instance Hashable SizeLength

instance S.Serialize SizeLength where
  get = S.label "SizeLength" $ do
    tag <- S.label "tag" S.getWord8
    case tag of
      0 -> S.label "LenU8"  $ pure LenU8
      1 -> S.label "LenU16" $ pure LenU16
      2 -> S.label "LenU32" $ pure LenU32
      3 -> S.label "LenU64" $ pure LenU64
      x  -> fail [i|Invalid SizeLength tag: #{x}|]

  put sizeLen = case sizeLen of
    LenU8 -> S.putWord8 0
    LenU16 -> S.putWord8 1
    LenU32 -> S.putWord8 2
    LenU64 -> S.putWord8 3

-- |This is returned by the node and specified in Concordium.Getters (from prototype repo).
-- Must stay in sync.
data Info
  -- |Info about a contract.
  = Info
    { -- |The contract balance.
      iAmount :: !T.Amount
      -- |The owner of the contract.
    , iOwner  :: !T.AccountAddress
      -- |The contract state.
    , iModel  :: !Model
      -- |The receive functions/methods.
    , iMethods :: ![Wasm.ReceiveName]
      -- |The contract name.
    , iName :: !Wasm.InitName
      -- |The corresponding source module.
    , iSourceModule :: !T.ModuleRef }
  deriving (Eq, Show)

instance FromJSON Info where
  parseJSON = AE.withObject "Info" $ \v -> do
    iAmount       <- v .: "amount"
    iOwner        <- v .: "owner"
    iModel        <- case HM.lookup "model" v of
      Just (AE.String s) -> JustBytes <$> decodeBase16 s
      Just x -> fail [i|Invalid Info, expected "model" field to be a String of base16 bytes, but got: #{x}|]
      Nothing -> fail [i|Invalid Info, missing "model" field.|]
    iMethods      <- v .: "methods"
    iName         <- v .: "name"
    iSourceModule <- v .: "sourceModule"
    return Info{..}
    where decodeBase16 xs =
            let (parsed, remaining) = BS16.decode . Text.encodeUtf8 $ xs in
              if BS.null remaining
              then return parsed
              else fail [i|Invalid model. Parsed: '#{parsed}', but failed on the remaining: '#{remaining}'|]

-- |State of the smart contract.
data Model =
    WithSchema !Module !AE.Value  -- ^ The schema and the decoded contract state.
  | JustBytes !ByteString        -- ^ The binary-encoded contract state.
  deriving (Eq, Show)

-- |Wrapper for Concordium.Types.Amount that uses a little-endian encoding
-- for binary serialization. Show and JSON instances are inherited from
-- the Amount type.
newtype AmountLE = AmountLE T.Amount
  deriving Eq
  deriving newtype (FromJSON, Show, ToJSON)

instance S.Serialize AmountLE where
  get = S.label "AmountLE" $ AmountLE . T.Amount <$> S.getWord64le
  put (AmountLE T.Amount{..}) = S.putWord64le _amount

-- ** Get and Put JSON **

getModelAsJSON :: Contract -> S.Get AE.Value
getModelAsJSON Contract{..} = do
  state' <- case state of
      Nothing    -> return AE.Null
      Just typ -> getValueAsJSON typ
  return $ AE.object ["state" .= state', "init" .= initSig, "receive" .= receiveSigs]

getValueAsJSON :: SchemaType -> S.Get AE.Value
getValueAsJSON typ = case typ of
  Unit -> return AE.Null
  Bool -> AE.Bool <$> S.get
  U8   -> AE.toJSON <$> S.getWord8
  U16  -> AE.toJSON <$> S.getWord16le
  U32  -> AE.toJSON <$> S.getWord32le
  U64  -> AE.toJSON <$> S.getWord64le
  I8   -> AE.toJSON <$> S.getInt8
  I16  -> AE.toJSON <$> S.getInt16le
  I32  -> AE.toJSON <$> S.getInt32le
  I64  -> AE.toJSON <$> S.getInt64le
  Amount -> AE.toJSON <$> (S.get :: S.Get AmountLE)
  AccountAddress  -> AE.toJSON <$> (S.get :: S.Get T.AccountAddress)
  ContractAddress -> AE.toJSON <$>
    (T.ContractAddress <$> (T.ContractIndex <$> S.getWord64le) <*> (T.ContractSubindex <$> S.getWord64le))
  Pair a b -> do
    l <- getValueAsJSON a
    r <- getValueAsJSON b
    return $ AE.toJSON [l, r]
  List sl elemType -> AE.toJSON <$> getListOfWithSizeLen sl (getValueAsJSON elemType)
  Set sl elemType  -> AE.toJSON <$> getListOfWithSizeLen sl (getValueAsJSON elemType)
  Map sl keyType valType -> AE.toJSON <$> getListOfWithSizeLen sl (S.getTwoOf (getValueAsJSON keyType) (getValueAsJSON valType))
  Array len elemType     -> AE.toJSON <$> getListOfWithKnownLen len (getValueAsJSON elemType)
  Struct fields -> AE.toJSON <$> getFieldsAsJSON fields
  Enum variants -> do
    idx <- if length variants <= 255
           then fromIntegral <$> S.getWord8
           else fromIntegral <$> S.getWord32le
    (name, fields) <- case variants ^? ix idx of
                      Just v -> return v
                      Nothing -> fail [i|Variant with index #{idx} does not exist for Enum.|]
    fields' <- getFieldsAsJSON fields
    return $ AE.object [name .= fields']
  where
    getFieldsAsJSON :: Fields -> S.Get AE.Value
    getFieldsAsJSON fields = case fields of
      Named pairs -> AE.toJSON . Map.fromList <$> mapM getPair pairs
      Unnamed xs  -> AE.toJSON <$> mapM getValueAsJSON xs
      None       -> return $ AE.Array mempty
      where getPair (k, v) = (k,) <$> getValueAsJSON v

putJSONParams :: SchemaType -> AE.Value -> Either String S.Put
putJSONParams typ json = case (typ, json) of
  (Unit, AE.Null)     -> pure mempty
  (Bool, AE.Bool b)   -> pure $ S.put b
  (U8,   AE.Number x) -> S.putWord8    <$> fromScientific x U8
  (U16,  AE.Number x) -> S.putWord16le <$> fromScientific x U16
  (U32,  AE.Number x) -> S.putWord32le <$> fromScientific x U32
  (U64,  AE.Number x) -> S.putWord64le <$> fromScientific x U64
  (I8,   AE.Number x) -> S.putInt8     <$> fromScientific x I8
  (I16,  AE.Number x) -> S.putInt16le  <$> fromScientific x I16
  (I32,  AE.Number x) -> S.putInt32le  <$> fromScientific x I32
  (I64,  AE.Number x) -> S.putInt64le  <$> fromScientific x I64

  (Amount, amt@(AE.String _)) -> addTraceInfo $ (S.put :: S.Putter AmountLE) <$> (resToEither . AE.fromJSON $ amt)

  (AccountAddress, v@(AE.String _)) -> addTraceInfo $ (S.put :: S.Putter T.AccountAddress) <$> AE.parseEither AE.parseJSON v

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
    unless (allUnique ls) $ Left [i|All elements must be unique in a set, but got:\n#{showPrettyJSON vec}.|]
    addTraceInfo $ putListLike sl elemType ls

  (Map sl keyType valType, AE.Array vec) -> do
    let len = fromIntegral . V.length $ vec
        maxLen = maxSizeLen sl
        putLen = putLenWithSizeLen sl $ V.length vec
    when (len > maxLen) $ Left $ tooLongError "Map" maxLen len
    putElems <- mapM (putJSONParams (Pair keyType valType)) vec
    addTraceInfo $ pure . sequence_ $ V.cons putLen putElems

  (Array expectedLen elemType, AE.Array vec) -> do
    let ls = V.toList vec
        actualLen = length ls
    unless (actualLen == fromIntegral expectedLen) $ addTraceInfo
      $ Left [i|Expected length is #{expectedLen}, but actual length is #{actualLen}.|]
    addTraceInfo $ sequence_ <$> mapM (putJSONParams elemType) ls

  (Struct fields, val) -> addTraceInfo $ putJSONFields fields val

  (enum@(Enum variants), AE.Object obj) -> case HM.toList obj of
    [] -> Left [i|The object provided was empty, but it should have contained a variant of the following enum:\n#{showPrettyJSON enum}.|]
    [(name, fields)] -> case lookupItemAndIndex name variants of
      Nothing -> Left [i|Enum variant '#{name}' does not exist in:\n#{showPrettyJSON enum}|]
      Just (fieldTypes, idx) -> do
        let putLen = if length variants <= 255
                       then S.putWord8 $ fromIntegral idx
                       else S.putWord32le $ fromIntegral idx
        putJSONFields' <- putJSONFields fieldTypes fields `addTraceInfoOf` [i|In enum variant '#{name}'.|]
        pure $ putLen <> putJSONFields'
    _ -> Left [i|#{obj} had too many fields. It should contain a single variant of the following enum:\n#{showPrettyJSON enum}.|]

  (type_, value) -> Left [i|Expected value of type #{showCompactPrettyJSON type_}, but got: #{showCompactPrettyJSON value}.|]

  where
    putJSONFields :: Fields -> AE.Value -> Either String S.Put
    putJSONFields fields val = case (fields, val) of
      (Named pairs, AE.Object obj) -> do
        let ls = HM.toList obj
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
        putUnnamed <- zipWithM putJSONParams types ls `addTraceInfoOf` [i|In #{showPrettyJSON vec}.|]
        pure . sequence_ $ putUnnamed

      (None, AE.Array vec) -> if V.null vec
                                then pure mempty
                                else Left [i|Expected an empty array to represent None, but got: #{showCompactPrettyJSON val}.|]

      (type_, value) -> Left [i|Expected value of type #{showCompactPrettyJSON type_}, but got: #{showCompactPrettyJSON value}.|]

    putListLike :: SizeLength -> SchemaType -> [AE.Value] -> Either String S.Put
    putListLike sl elemType xs = do
      let putLen = putLenWithSizeLen sl $ length xs
      putElems <- mapM (putJSONParams elemType) xs
      pure . sequence_ $ putLen : putElems

    putContrAddr :: Scientific -> Scientific -> Either String S.Put
    putContrAddr idx subidx = do
      idx' <- fromScientific idx U64
      subidx' <- fromScientific subidx U64
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
      LenU8 -> toInteger (maxBound :: Word8)
      LenU16 -> toInteger (maxBound :: Word16)
      LenU32 -> toInteger (maxBound :: Word32)
      LenU64 -> toInteger (maxBound :: Word64)

    lookupAndPut :: [(Text, SchemaType)]     -- ^ The names and types for Named Fields.
                 -> (Text, AE.Value)         -- ^ A field name and a value.
                 -> Either String (Int, S.Put) -- ^ The index of the field in the particular Named Fields,
                                             --   used for subsequent ordering,
                                             --   and a putter for the value (or an error message).
    lookupAndPut types (name, value) = case lookupItemAndIndex name types of
          Nothing -> Left [i|'#{name}' is not a valid field in the type:\n#{showPrettyJSON (Named types)}.|]
          Just (typ', idx) -> ((idx, ) <$> putJSONParams typ' value) `addTraceInfoOf` [i|In field '#{name}'.|]

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


-- ** Schema from Module **

-- |4 bytes that start every valid Wasm module in binary.
wasmMagicHash :: ByteString
wasmMagicHash = BS.pack [0x00, 0x61, 0x73, 0x6D]

-- |The currently supported version of the Wasm specification.
wasmVersion :: ByteString
wasmVersion = BS.pack [0x01, 0x00, 0x00, 0x00]

getEmbeddedSchemaFromModule :: S.Get (Maybe Module)
getEmbeddedSchemaFromModule = do
  mhBs <- S.getByteString 4
  unless (mhBs == wasmMagicHash) $ fail "Unknown magic value. This is likely not a Wasm module."
  vBs <- S.getByteString 4
  unless (vBs == wasmVersion) $ fail "Unsupported Wasm version."
  go

  where
    go :: S.Get (Maybe Module)
    go = do
      isEmpty <- S.isEmpty
      -- Not all modules contain a schema.
      -- We reached the end of input without finding the schema.
      if isEmpty then
        return Nothing
      else do
        sectionId <- S.label "sectionId" S.getWord8
        sectionSize <- S.label "sectionSize" $ fromIntegral <$> getLEB128Word32le
        if sectionId == 0
        then do
          name <- S.label "Custom Section Name" getTextWithLEB128Len
          if name == "concordium-schema-v1"
          then Just <$> S.get
          else S.skip sectionSize *> go
        else S.skip sectionSize *> go

-- ** Helpers **

-- * List *

-- |Nearly identical to Data.Serialize.getListOf implementation (except for length).
getListOfWithKnownLen :: Integral len => len -> S.Get a -> S.Get [a]
getListOfWithKnownLen len ga = S.label "List of known length" $ go [] len
  where
    go as 0 = return $! reverse as
    go as l = do x <- ga
                 x `seq` go (x:as) (l - 1)

getListOfWithSizeLen :: SizeLength -> S.Get a -> S.Get [a]
getListOfWithSizeLen sl ga = S.label "List" $ do
  len <- S.label "Length" $ case sl of
    LenU8  -> toInteger <$> S.getWord8
    LenU16 -> toInteger <$> S.getWord16le
    LenU32 -> toInteger <$> S.getWord32le
    LenU64 -> toInteger <$> S.getWord64le
  S.label [i|#{len} elements|] $ getListOfWithKnownLen len ga

getListOfWith32leLen :: S.Get a -> S.Get [a]
getListOfWith32leLen = getListOfWithSizeLen LenU32

putListOfWithSizeLen :: SizeLength -> S.Putter a -> S.Putter [a]
putListOfWithSizeLen sl pa ls = do
  putLenWithSizeLen sl $ length ls
  mapM_ pa ls

putLenWithSizeLen :: SizeLength -> S.Putter Int
putLenWithSizeLen sl len = case sl of
  LenU8  -> S.putWord8    $ fromIntegral len
  LenU16 -> S.putWord16le $ fromIntegral len
  LenU32 -> S.putWord32le $ fromIntegral len
  LenU64 -> S.putWord64le $ fromIntegral len

putListOfWith32leLen :: S.Putter a -> S.Putter [a]
putListOfWith32leLen = putListOfWithSizeLen LenU32


-- * Map *

getMapOfWith32leLen :: Ord k => S.Get k -> S.Get v -> S.Get (Map k v)
getMapOfWith32leLen gt gv = S.label "Map" $ Map.fromList <$> getListOfWith32leLen (S.getTwoOf gt gv)

putMapOfWith32leLen :: S.Putter k -> S.Putter v -> S.Putter (Map k v)
putMapOfWith32leLen pv pk = putListOfWith32leLen (S.putTwoOf pv pk) . Map.toList


-- * LEB128 *

-- |Get a LEB128-encoded Word32. This uses an encoding compatible with the Wasm standard,
-- which means that the encoding will use at most 5 bytes.
getLEB128Word32le :: S.Get Word32
getLEB128Word32le = S.label "Word32LEB128" $ decode7 0 5 1
  where
    decode7 :: Word64 -> Word8 -> Word64 -> S.Get Word32
    decode7 acc left multiplier = do
      unless (left > 0) $ fail "Section size byte overflow"
      byte <- S.getWord8
      if Bits.testBit byte 7
        then decode7 (acc + multiplier * fromIntegral (Bits.clearBit byte 7)) (left-1) (multiplier * 128)
      else do
        let value = acc + multiplier * fromIntegral byte
        unless (value <= fromIntegral (maxBound :: Word32)) $ fail "Section size value overflow"
        return . fromIntegral $ value


-- * Text *

-- |Read text where length is serialized as little-endian 4-byte value.
-- This tries to decode the bytes in utf8 encoding, signalling failure
-- if the byte sequence is not valid utf8.
getText :: S.Get Text
getText = do
  txt <- S.label "Text" $ Text.decodeUtf8' . BS.pack <$> getListOfWith32leLen S.get
  case txt of
    Left err -> fail [i|Could not decode Text: #{err}|]
    Right txt' -> pure txt'

-- |Serialize text in utf8 encoding. The length it output as
-- 4 bytes, little endian.
putText :: S.Putter Text
putText = putListOfWithSizeLen LenU32 S.put . BS.unpack . Text.encodeUtf8

getTextWithLEB128Len :: S.Get Text
getTextWithLEB128Len = S.label "Text with LEB128 Length" $ do
  len <- getLEB128Word32le
  txt <- Text.decodeUtf8' . BS.pack <$> getListOfWithKnownLen len S.get
  case txt of
    Left err -> fail [i|Could not decode Text with LEB128 len: #{err}|]
    Right txt' -> pure txt'

-- |Get a contract name from an InitName, i.e. extracting the text and removing the "init_" prefix.
-- If stripping the prefix fails, it simply returns the extracted text
-- (this should never happen, unless the InitName was incorrectly constructed).
contractNameFromInitName :: Wasm.InitName -> Text
contractNameFromInitName initName = fromMaybe initNameText (Text.stripPrefix "init_" initNameText)
  where initNameText = Wasm.initName initName

-- |Get a method name from a Receive name, i.e. extracting the text and removing the "<contractName>." prefix.
-- If the receiveName does not have the prefix, it simply returns the extracted text.
methodNameFromReceiveName :: Wasm.ReceiveName -> Text
methodNameFromReceiveName rcvName = case Text.split (=='.') receiveNameText of
  [_contrName, methodName] -> methodName
  _ -> receiveNameText
  where receiveNameText = Wasm.receiveName rcvName

-- ** External API **

-- |Uses a `Schema` to parse the `JustBytes` of `Info`.
-- Returns `Left` if a schema has already been included or the parsing fails.
addSchemaToInfo :: Info -> Module -> Either String Info
addSchemaToInfo info@Info{..} schema@Module{..} = case iModel of
  WithSchema _ _ -> Left "Already contains a schema."
  JustBytes bytes -> case Map.lookup contractName contracts of
    Nothing -> Left "The provided schema does not match the provided contract info."
    Just contract -> (\decodedState -> info {iModel = WithSchema schema decodedState}) <$> S.runGet (getModelAsJSON contract) bytes
  where contractName = contractNameFromInitName iName

-- |Serialize JSON parameters to binary using `SchemaType` or fail with an error message.
serializeParams :: SchemaType -> AE.Value -> Either String ByteString
serializeParams typ params = S.runPut <$> putJSONParams typ params

-- |Try to find an embedded schema in a module and decode it. Alias to avoid having to import Data.Serialize.
decodeEmbeddedSchema :: ByteString -> Either String (Maybe Module)
decodeEmbeddedSchema = S.runGet getEmbeddedSchemaFromModule

-- |Decode a schema. Alias to avoid having to import Data.Serialize.
decodeSchema :: ByteString -> Either String Module
decodeSchema = S.decode

-- |A function name for a function inside a smart contract.
data FuncName
  = InitName !Text -- ^ Name of an init function.
  | ReceiveName !Text !Text -- ^ Name of a receive function.
  deriving Eq

-- |Tries to find the signature, i.e. `SchemaType`, for a function by its name.
lookupSignatureForFunc :: Module -> FuncName -> Maybe SchemaType
lookupSignatureForFunc Module{..} funcName = case funcName of
  InitName contrName -> Map.lookup contrName contracts >>= initSig
  ReceiveName contrName receiveName -> do
    contract <- Map.lookup contrName contracts
    Map.lookup receiveName (receiveSigs contract)
