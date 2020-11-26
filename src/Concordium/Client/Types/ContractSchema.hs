{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.ContractSchema
  ( addSchemaToInfo
  , contractNameFromInitName
  , decodeEmbeddedSchema
  , decodeSchema
  , getValueAsJSON
  , lookupSchemaForParams
  , methodNameFromReceiveName
  , putJSONParams
  , runGetVersionedModuleSource
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
import Data.Serialize (Get, Put, Putter, Serialize, get, getInt8, getInt16le, getInt32le, getInt64le,
                       getTwoOf, getWord8, getWord16le, getWord32le, getWord64le, label, put, putInt8, putInt16le,
                       putInt32le, putInt64le, putTwoOf, putWord8, putWord16le, putWord32le, putWord64le)
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Word (Word8, Word32, Word64)
import GHC.Generics
import Lens.Micro.Platform ((^?), ix)


import Concordium.ID.Types (addressFromText)
import Concordium.Crypto.SHA256 as SHA256

-- ** Data Types and Instances **

-- |Parallel to Module defined in contracts-common (Rust).
-- Must stay in sync.
newtype Module
  = Module { contracts :: Map Text Contract }
  deriving (Eq, Show, Generic)

instance ToJSON Module

instance Serialize Module where
  get = label "Module" $ Module <$> getMapOfWith32leLen getText get
  put Module {..} = putMapOfWith32leLen putText put contracts

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

instance Serialize Contract where
  get = label "Contract" $ do
    state <- label "state" get
    initSig <- label "initSig" get
    receiveSigs <- label "receiveSigs" $ getMapOfWith32leLen getText get
    pure Contract{..}
  put Contract {..} = put state <> put initSig <> putMapOfWith32leLen putText put receiveSigs

-- |Parallel to Fields defined in contracts-common (Rust).
-- Must stay in sync.
data Fields
  = Named [(Text, SchemaType)] -- ^ Represents a named enum or struct.
  | Unnamed [SchemaType] -- ^ Represents an unnamed enum or struct.
  | Empty -- ^ Represents an empty enum or struct.
  deriving (Eq, Generic, Show)

instance Hashable Fields

instance ToJSON Fields where
  toJSON (Named fields) = AE.object . map (\(name, value) -> name .= value) $ fields
  toJSON (Unnamed fields) = AE.toJSON fields
  toJSON Empty = AE.Array . V.fromList $ []

instance Serialize Fields where
  get = label "Fields" $ do
    tag <- getWord8
    case tag of
      0 -> label "Named" $ Named <$> getListOfWith32leLen (getTwoOf getText get)
      1 -> label "Unnamed" $ Unnamed <$> getListOfWith32leLen get
      2 -> label "Empty" $ pure Empty
      x -> fail [i|Invalid Fields tag: #{x}|]

  put fields = case fields of
    Named pairs -> putWord8 0 <> putListOfWith32leLen (putTwoOf putText put) pairs
    Unnamed types -> putWord8 1 <> putListOfWith32leLen put types
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

-- |This should match the format used in `getValueAsJSON` so the user can copy this and use it for
-- creating a parameter file in json format. The only difference is for enums, in which all of its
-- variants are shown here, but only one should be used in the parameter file.
instance ToJSON SchemaType where
  toJSON = \case
    Struct fields -> AE.toJSON fields
    Enum variants -> AE.object ["Enum" .= (AE.Array . V.fromList . map (\(k, v) -> AE.object [k .= v]) $ variants)]
    Pair typA typB -> AE.Array . V.fromList $ [AE.toJSON typA, AE.toJSON typB]
    x -> AE.String . Text.pack . show $ x

instance Serialize SchemaType where
  get = label "SchemaType" $ do
    tag <- label "tag" getWord8
    case tag of
      0  -> label "Unit" $ pure Unit
      1  -> label "Bool" $ pure Bool
      2  -> label "U8"   $ pure U8
      3  -> label "U16"  $ pure U16
      4  -> label "U32"  $ pure U32
      5  -> label "U64"  $ pure U64
      6  -> label "I8"   $ pure I8
      7  -> label "I16"  $ pure I16
      8  -> label "I32"  $ pure I32
      9  -> label "I64"  $ pure I64
      10 -> label "Amount" $ pure Amount
      11 -> label "AccountAddress"  $ pure AccountAddress
      12 -> label "ContractAddress" $ pure ContractAddress
      13 -> label "Pair"   $ Pair <$> get <*> get
      14 -> label "List"   $ List <$> get <*> get
      15 -> label "Set"    $ Set <$> get <*> get
      16 -> label "Map"    $ Map <$> get <*> get <*> get
      17 -> label "Array"  $ Array <$> getWord32le <*> get
      18 -> label "Struct" $ Struct <$> get
      19 -> label "Enum"   $ Enum <$> getListOfWith32leLen (getTwoOf getText get)
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
    Pair a b  -> putWord8 13 <> put a <> put b
    List sl a -> putWord8 14 <> put sl <> put a
    Set sl a  -> putWord8 15 <> put sl <> put a
    Map sl k v    -> putWord8 16 <> put sl <> put k <> put v
    Array len a   -> putWord8 17 <> putWord32le len <> put a
    Struct fields -> putWord8 18 <> put fields
    Enum enum     -> putWord8 19 <> putListOfWith32leLen (putTwoOf putText put) enum

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

instance Serialize SizeLength where
  get = label "SizeLength" $ do
    tag <- label "tag" getWord8
    case tag of
      0 -> label "LenU8"  $ pure LenU8
      1 -> label "LenU16" $ pure LenU16
      2 -> label "LenU32" $ pure LenU32
      3 -> label "LenU64" $ pure LenU64
      x  -> fail [i|Invalid SizeLength tag: #{x}|]

  put sizeLen = case sizeLen of
    LenU8 -> putWord8 0
    LenU16 -> putWord8 1
    LenU32 -> putWord8 2
    LenU64 -> putWord8 3

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
    pure Info{..}
    where decodeBase16 xs =
            let (parsed, remaining) = BS16.decode . Text.encodeUtf8 $ xs in
              if BS.null remaining
              then pure parsed
              else fail [i|Invalid model. Parsed: '#{parsed}', but failed on the remaining: '#{remaining}'|]

data Model =
    WithSchema Module AE.Value  -- ^ The schema and the decoded contract state.
  | JustBytes ByteString        -- ^ The binary-encoded contract state.
  deriving (Eq, Show)

-- |Wrapper for Concordium.Types.Amount that uses a little-endian encoding.
newtype AmountLE = AmountLE T.Amount
  deriving Eq
  deriving newtype (FromJSON, Show, ToJSON)

instance Serialize AmountLE where
  get = label "AmountLE" $ AmountLE . T.Amount <$> getWord64le
  put (AmountLE T.Amount{..}) = putWord64le _amount

-- ** Get and Put JSON **

getModelAsJSON :: Contract -> Get AE.Value
getModelAsJSON Contract{..} = do
  state' <- case state of
      Nothing    -> return AE.Null
      Just typ -> getValueAsJSON typ
  return $ AE.object ["state" .= state', "init" .= initSig, "receive" .= receiveSigs]

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
  Amount -> AE.toJSON <$> (get :: Get AmountLE)
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
    idx <- if length variants <= 255
           then fromIntegral <$> getWord8
           else fromIntegral <$> getWord32le
    (name, fields) <- case variants ^? ix idx of
                      Just v -> return v
                      Nothing -> fail [i|Variant with index #{idx} does not exist for Enum.|]
    fields' <- getFieldsAsJSON fields
    return $ AE.object [name .= fields']
  where
    getFieldsAsJSON :: Fields -> Get AE.Value
    getFieldsAsJSON fields = case fields of
      Named pairs -> AE.toJSON . Map.fromList <$> mapM getPair pairs
      Unnamed xs  -> AE.toJSON <$> mapM getValueAsJSON xs
      Empty       -> return $ AE.Array mempty
      where getPair (k, v) = (k,) <$> getValueAsJSON v

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

  (Amount, amt@(AE.String _)) -> addTraceInfo $ (put :: Putter AmountLE) <$> (resToEither . AE.fromJSON $ amt)

  (AccountAddress, v@(AE.String _)) -> addTraceInfo $ (put :: Putter T.AccountAddress) <$> AE.parseEither AE.parseJSON v

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
                       then putWord8 $ fromIntegral idx
                       else putWord32le $ fromIntegral idx
        putJSONFields' <- putJSONFields fieldTypes fields `addTraceInfoOf` [i|In enum variant '#{name}'.|]
        pure $ putLen <> putJSONFields'
    _ -> Left [i|#{obj} had too many fields. It should contain a single variant of the following enum:\n#{showPrettyJSON enum}.|]

  (type_, value) -> Left [i|Expected value of type #{showCompactPrettyJSON type_}, but got: #{showCompactPrettyJSON value}.|]

  where
    putJSONFields :: Fields -> AE.Value -> Either String Put
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

      (Empty, AE.Array vec) -> if V.null vec
                                then pure mempty
                                else Left [i|Expected an empty array to represent Empty, but got: #{showCompactPrettyJSON val}.|]

      (type_, value) -> Left [i|Expected value of type #{showCompactPrettyJSON type_}, but got: #{showCompactPrettyJSON value}.|]

    putListLike :: SizeLength -> SchemaType -> [AE.Value] -> Either String Put
    putListLike sl elemType xs = do
      let putLen = putLenWithSizeLen sl $ length xs
      putElems <- mapM (putJSONParams elemType) xs
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
        Nothing -> Left [i|#{x} is out of bounds for type #{numType}.|]
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

    lookupAndPut :: [(Text, SchemaType)]     -- ^ The names and types for Named Fields.
                 -> (Text, AE.Value)         -- ^ A field name and a value.
                 -> Either String (Int, Put) -- ^ The index of the field in the particular Named Fields,
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

getEmbeddedSchemaFromModule :: Get Module
getEmbeddedSchemaFromModule = do
  label "Magic"   $ S.skip 4
  label "Version" $ S.skip 4
  go

  where
    go :: Get Module
    go = do
      isEmpty <- S.isEmpty
      -- Not all modules contain a schema.
      when isEmpty $ fail "Module does not contain a schema."
      sectionId <- label "sectionId" getWord8
      sectionSize <- label "sectionSize" $ fromIntegral <$> getLEB128Word32le
      if sectionId == 0
      then do
        name <- label "Custom Section Name" getTextWithLEB128Len
        if name == "concordium-schema-v1"
        then do
          get
        else S.skip sectionSize *> go
      else S.skip sectionSize *> go

-- |Extract the WASM version, our API version, and the module source.
getVersionedModuleSource :: Get (Word32, ByteString)
getVersionedModuleSource = do
  version <- label "WASM Version" getWord32le
  sourceLen <- label "Length of Module Source" $ fromIntegral <$> S.getWord32be
  modSource <- label "Module Source" $ S.getBytes sourceLen
  pure (version, modSource)

-- ** Helpers **

-- * List *

-- Nearly identical to Data.Serialize.getListOf implementation (except for length).
getListOfWithKnownLen :: Integral len => len -> Get a -> Get [a]
getListOfWithKnownLen len ga = label "List of known length" $ go [] len
  where
    go as 0 = return $! reverse as
    go as l = do x <- ga
                 x `seq` go (x:as) (l - 1)

getListOfWithSizeLen :: SizeLength -> Get a -> Get [a]
getListOfWithSizeLen sl ga = label "List" $ do
  len :: Integer <- label "Length" $ case sl of
    LenU8  -> fromIntegral <$> getWord8
    LenU16 -> fromIntegral <$> getWord16le
    LenU32 -> fromIntegral <$> getWord32le
    LenU64 -> fromIntegral <$> getWord64le
  label [i|#{len} elements|] $ getListOfWithKnownLen len ga

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

getMapOfWith32leLen :: Ord k => Get k -> Get v -> Get (Map k v)
getMapOfWith32leLen gt gv = label "Map" $ Map.fromList <$> getListOfWith32leLen (getTwoOf gt gv)

putMapOfWith32leLen :: Putter k -> Putter v -> Putter (Map k v)
putMapOfWith32leLen pv pk = putListOfWith32leLen (putTwoOf pv pk) . Map.toList


-- * LEB128 *

getLEB128Word32le :: Get Word32
getLEB128Word32le = label "Word32LEB128" $ decode7 0 5 1
  where
    decode7 :: Word64 -> Word8 -> Word64 -> Get Word32
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

getText :: Get Text
getText = do
  txt <- label "Text" $ Text.decodeUtf8' . BS.pack <$> getListOfWith32leLen get
  case txt of
    Left err -> fail [i|Could not decode Text: #{err}|]
    Right txt' -> pure txt'

putText :: Putter Text
putText = putListOfWithSizeLen LenU32 put . BS.unpack . Text.encodeUtf8

getTextWithLEB128Len :: Get Text
getTextWithLEB128Len = label "Text with LEB128 Length" $ do
  len <- getLEB128Word32le
  txt <- Text.decodeUtf8' . BS.pack <$> getListOfWithKnownLen len get
  case txt of
    Left err -> fail [i|Could not decode Text with LEB128 len: #{err}|]
    Right txt' -> pure txt'

-- |Get a contract name from an InitName, i.e. extracting the text and removing the "init_" prefix.
-- If the stripping the prefix fails, it simply returns the extracted text
-- (this should never happen, unless the InitName was incorrectly constructed).
contractNameFromInitName :: Wasm.InitName -> Text
contractNameFromInitName initName = case Text.stripPrefix "init_" initNameText of
  Nothing -> initNameText
  Just contrName -> contrName
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

decodeEmbeddedSchema :: ByteString -> Either String Module
decodeEmbeddedSchema = S.runGet getEmbeddedSchemaFromModule

decodeSchema :: ByteString -> Either String Module
decodeSchema = S.decode

data FuncName
  = InitName Text
  | ReceiveName Text Text
  deriving Eq

lookupSchemaForParams :: Module -> FuncName -> Maybe SchemaType
lookupSchemaForParams Module{..} funcName = case funcName of
  InitName contrName -> Map.lookup contrName contracts >>= initSig
  ReceiveName contrName receiveName -> do
    contract <- Map.lookup contrName contracts
    Map.lookup receiveName (receiveSigs contract)

runGetVersionedModuleSource :: ByteString -> Either String (Word32, ByteString)
runGetVersionedModuleSource = S.runGet getVersionedModuleSource
