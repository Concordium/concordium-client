{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.Contract.Schema where

import Control.Monad (unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Word (Word8, Word32, Word64)
import GHC.Generics

-- |Try to find an embedded schema in a module and decode it.
decodeEmbeddedSchema :: BS.ByteString -> Either String (Maybe ModuleSchema)
decodeEmbeddedSchema = S.runGet getEmbeddedSchemaFromModule

-- |Decode a `ModuleSchema`.
decodeModuleSchema :: BS.ByteString -> Either String ModuleSchema
decodeModuleSchema = S.decode

-- |Tries to find the signature, i.e. `SchemaType`, for a contract function by its name.
lookupSignatureForFunc :: ModuleSchema -> FuncName -> Maybe SchemaType
lookupSignatureForFunc ModuleSchema{..} funcName = case funcName of
  InitFuncName contrName -> Map.lookup contrName contractSchemas >>= initSig
  ReceiveFuncName contrName receiveName -> do
    contract <- Map.lookup contrName contractSchemas
    Map.lookup receiveName (receiveSigs contract)

-- |Parallel to Module defined in contracts-common (Rust).
-- Must stay in sync.
newtype ModuleSchema
  = ModuleSchema { contractSchemas :: Map Text ContractSchema }
  deriving (Eq, Show, Generic)

instance AE.ToJSON ModuleSchema

instance S.Serialize ModuleSchema where
  get = S.label "ModuleSchema" $ ModuleSchema <$> getMapOfWithSizeLen Four getText S.get
  put ModuleSchema {..} = putMapOfWithSizeLen Four putText S.put contractSchemas

-- |Parallel to ContractSchema defined in contracts-common (Rust).
-- Must stay in sync.
data ContractSchema
  =  ContractSchema -- ^ Describes the schemas of a smart contract.
  {  state :: Maybe SchemaType -- ^ The optional contract state
  ,  initSig :: Maybe SchemaType -- ^ Type signature for the init function
  ,  receiveSigs :: Map Text SchemaType -- ^ Type signatures for the receive functions
  }
  deriving (Eq, Show, Generic)

instance AE.ToJSON ContractSchema

instance S.Serialize ContractSchema where
  get = S.label "ContractSchema" $ do
    state <- S.label "state" S.get
    initSig <- S.label "initSig" S.get
    receiveSigs <- S.label "receiveSigs" $ getMapOfWithSizeLen Four getText S.get
    pure ContractSchema{..}
  put ContractSchema {..} = S.put state <> S.put initSig <> putMapOfWithSizeLen Four putText S.put receiveSigs

-- |Parallel to Fields defined in contracts-common (Rust).
-- Must stay in sync.
data Fields
  = Named [(Text, SchemaType)] -- ^ Represents a named enum or struct.
  | Unnamed [SchemaType] -- ^ Represents an unnamed enum or struct.
  | None -- ^ Represents an empty enum or struct.
  deriving (Eq, Generic, Show)

instance Hashable Fields

instance AE.ToJSON Fields where
  toJSON (Named fields) = AE.object . map (\(name, value) -> name .= value) $ fields
  toJSON (Unnamed fields) = AE.toJSON fields
  toJSON None = AE.Array . V.fromList $ []

instance S.Serialize Fields where
  get = S.label "Fields" $ do
    tag <- S.getWord8
    case tag of
      0 -> S.label "Named" $ Named <$> getListOfWithSizeLen Four (S.getTwoOf getText S.get)
      1 -> S.label "Unnamed" $ Unnamed <$> getListOfWithSizeLen Four S.get
      2 -> S.label "None" $ pure None
      x -> fail [i|Invalid Fields tag: #{x}|]

  put fields = case fields of
    Named pairs -> S.putWord8 0 <> putListOfWithSizeLen Four (S.putTwoOf putText S.put) pairs
    Unnamed types -> S.putWord8 1 <> putListOfWithSizeLen Four S.put types
    None -> S.putWord8 2

-- |Parallel to Type defined in contracts-common (Rust).
-- Must stay in sync.
data SchemaType =
    Unit
  | Bool
  | UInt8
  | UInt16
  | UInt32
  | UInt64
  | Int8
  | Int16
  | Int32
  | Int64
  | Amount
  | AccountAddress
  | ContractAddress
  | Timestamp
  | Duration
  | Pair SchemaType SchemaType
  | List SizeLength SchemaType
  | Set SizeLength SchemaType
  | Map SizeLength SchemaType SchemaType
  | Array Word32 SchemaType
  | Struct Fields
  | Enum [(Text, Fields)]
  | String SizeLength
  | UInt128
  | Int128
  | ContractName SizeLength
  | ReceiveName SizeLength
  deriving (Eq, Generic, Show)

instance Hashable SchemaType

-- |This should _mostly_ match the format used in `getJSONUsingSchema` so the
-- user can copy this and use it for creating a parameter file in json format.
-- It differs from the expected parameter format in the following ways:
--   - Enums are shown with all of its variants in a list,
--     but only one variant should be used in the parameter file.
--   - All placeholders are surrounded with <> and shown as strings,
--     even when the expected value is not a string.
--     For example: "<UInt8>" which should be replaced with an unquoted number.
instance AE.ToJSON SchemaType where
  toJSON = \case
    Unit -> AE.Array . V.fromList $ []
    Bool -> AE.String "<Bool>"
    UInt8 -> AE.String "<UInt8>"
    UInt16 -> AE.String "<UInt16>"
    UInt32 -> AE.String "<UInt32>"
    UInt64 -> AE.String "<UInt64>"
    Int8 -> AE.String "<Int8>"
    Int16 -> AE.String "<Int16>"
    Int32 -> AE.String "<Int32>"
    Int64 -> AE.String "<Int64>"
    Amount -> AE.String "<Amount>"
    AccountAddress -> AE.String "<AccountAddress>"
    ContractAddress -> AE.object [ "index" .= AE.toJSON UInt64, "subindex" .= AE.toJSON UInt64 ]
    Timestamp -> AE.String "<Timestamp>"
    Duration -> AE.String "<Duration>"
    Pair typA typB -> toJsonArray [AE.toJSON typA, AE.toJSON typB]
    List _ typ -> toJsonArray [AE.toJSON typ]
    Set _ typ -> toJsonArray [AE.toJSON typ]
    Map _ typK typV -> toJsonArray [toJsonArray [AE.toJSON typK, AE.toJSON typV]]
    Array _ typ -> toJsonArray [AE.toJSON typ]
    Struct fields -> AE.toJSON fields
    Enum variants -> AE.object ["Enum" .= (toJsonArray . map (\(k, v) -> AE.object [k .= v]) $ variants)]
    String _ -> AE.String "<String>"
    UInt128 -> AE.String "<UInt128>"
    Int128 -> AE.String "<Int128>"
    ContractName _ -> AE.object [ "contract" .= AE.String "<String>" ]
    ReceiveName _ -> AE.object [ "contract" .= AE.String "<String>", "func" .= AE.toJSON "<String>" ]
    where toJsonArray = AE.Array . V.fromList

instance S.Serialize SchemaType where
  get = S.label "SchemaType" $ do
    tag <- S.label "tag" S.getWord8
    case tag of
      0  -> S.label "Unit" $ pure Unit
      1  -> S.label "Bool" $ pure Bool
      2  -> S.label "UInt8"   $ pure UInt8
      3  -> S.label "UInt16"  $ pure UInt16
      4  -> S.label "UInt32"  $ pure UInt32
      5  -> S.label "UInt64"  $ pure UInt64
      6  -> S.label "Int8"    $ pure Int8
      7  -> S.label "Int16"   $ pure Int16
      8  -> S.label "Int32"   $ pure Int32
      9  -> S.label "Int64"   $ pure Int64
      10 -> S.label "Amount"  $ pure Amount
      11 -> S.label "AccountAddress"  $ pure AccountAddress
      12 -> S.label "ContractAddress" $ pure ContractAddress
      13 -> S.label "Timestamp" $ pure Timestamp
      14 -> S.label "Duration"  $ pure Duration
      15 -> S.label "Pair"   $ Pair <$> S.get <*> S.get
      16 -> S.label "List"   $ List <$> S.get <*> S.get
      17 -> S.label "Set"    $ Set <$> S.get <*> S.get
      18 -> S.label "Map"    $ Map <$> S.get <*> S.get <*> S.get
      19 -> S.label "Array"  $ Array <$> S.getWord32le <*> S.get
      20 -> S.label "Struct" $ Struct <$> S.get
      21 -> S.label "Enum"   $ Enum <$> getListOfWithSizeLen Four (S.getTwoOf getText S.get)
      22 -> S.label "String" $ String <$> S.get
      23 -> S.label "UInt128" $ pure UInt128
      24 -> S.label "Int128"  $ pure Int128
      25 -> S.label "ContractName" $ ContractName <$> S.get
      26 -> S.label "ReceiveName"  $ ReceiveName <$> S.get
      x  -> fail [i|Invalid SchemaType tag: #{x}|]

  put typ = case typ of
    Unit -> S.putWord8 0
    Bool -> S.putWord8 1
    UInt8   -> S.putWord8 2
    UInt16  -> S.putWord8 3
    UInt32  -> S.putWord8 4
    UInt64  -> S.putWord8 5
    Int8   -> S.putWord8 6
    Int16  -> S.putWord8 7
    Int32  -> S.putWord8 8
    Int64  -> S.putWord8 9
    Amount -> S.putWord8 10
    AccountAddress  -> S.putWord8 11
    ContractAddress -> S.putWord8 12
    Timestamp -> S.putWord8 13
    Duration  -> S.putWord8 14
    Pair a b  -> S.putWord8 15 <> S.put a <> S.put b
    List sl a -> S.putWord8 16 <> S.put sl <> S.put a
    Set sl a  -> S.putWord8 17 <> S.put sl <> S.put a
    Map sl k v    -> S.putWord8 18 <> S.put sl <> S.put k <> S.put v
    Array len a   -> S.putWord8 19 <> S.putWord32le len <> S.put a
    Struct fields -> S.putWord8 20 <> S.put fields
    Enum enum     -> S.putWord8 21 <> putListOfWithSizeLen Four (S.putTwoOf putText S.put) enum
    String sl     -> S.putWord8 22 <> S.put sl
    UInt128 -> S.putWord8 23
    Int128  -> S.putWord8 24
    ContractName sl     -> S.putWord8 25 <> S.put sl
    ReceiveName sl      -> S.putWord8 26 <> S.put sl

-- |Parallel to SizeLength defined in contracts-common (Rust).
-- Must stay in sync.
data SizeLength
  = One
  | Two
  | Four
  | Eight
  deriving (Eq, Generic, Show)

instance AE.ToJSON SizeLength where
  toJSON One   = AE.String "One"
  toJSON Two   = AE.String "Two"
  toJSON Four  = AE.String "Four"
  toJSON Eight = AE.String "Eight"

instance Hashable SizeLength

instance S.Serialize SizeLength where
  get = S.label "SizeLength" $ do
    tag <- S.label "tag" S.getWord8
    case tag of
      0 -> S.label "One"   $ pure One
      1 -> S.label "Two"   $ pure Two
      2 -> S.label "Four"  $ pure Four
      3 -> S.label "Eight" $ pure Eight
      x  -> fail [i|Invalid SizeLength tag: #{x}|]

  put sizeLen = case sizeLen of
    One   -> S.putWord8 0
    Two   -> S.putWord8 1
    Four  -> S.putWord8 2
    Eight -> S.putWord8 3

-- |A function name for a function inside a smart contract.
data FuncName
  = InitFuncName !Text -- ^ Name of an init function.
  | ReceiveFuncName !Text !Text -- ^ Name of a receive function.
  deriving Eq

-- |Try to find an embedded `ModuleSchema` inside a WasmModule and decode it.
-- Returns `Nothing` if no schema is embedded.
getEmbeddedSchemaFromModule :: S.Get (Maybe ModuleSchema)
getEmbeddedSchemaFromModule = do
  mhBs <- S.getByteString 4
  unless (mhBs == wasmMagicHash) $ fail "Unknown magic value. This is likely not a Wasm module."
  vBs <- S.getByteString 4
  unless (vBs == wasmVersion) $ fail "Unsupported Wasm version."
  go

  where
    go :: S.Get (Maybe ModuleSchema)
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

    getTextWithLEB128Len :: S.Get Text
    getTextWithLEB128Len = S.label "Text with LEB128 Length" $ do
      len <- getLEB128Word32le
      txt <- Text.decodeUtf8' . BS.pack <$> getListOfWithKnownLen len S.get
      case txt of
        Left err -> fail [i|Could not decode Text with LEB128 len: #{err}|]
        Right txt' -> pure txt'

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

    -- |4 bytes that start every valid Wasm module in binary.
    wasmMagicHash :: BS.ByteString
    wasmMagicHash = BS.pack [0x00, 0x61, 0x73, 0x6D]

    -- |The currently supported version of the Wasm specification.
    wasmVersion :: BS.ByteString
    wasmVersion = BS.pack [0x01, 0x00, 0x00, 0x00]


-- HELPERS

-- Nearly identical to Data.Serialize.getListOf implementation (except for length).
getListOfWithKnownLen :: (Integral len, Show len) => len -> S.Get a -> S.Get [a]
getListOfWithKnownLen len ga = S.label ("List of known length " ++ show len) $ go [] len
  where
    go as 0 = return $! reverse as
    go as l = do x <- ga
                 x `seq` go (x:as) (l - 1)

getListOfWithSizeLen :: SizeLength -> S.Get a -> S.Get [a]
getListOfWithSizeLen sl ga = S.label "List" $ do
  len <- S.label "Length" $ case sl of
    One   -> toInteger <$> S.getWord8
    Two   -> toInteger <$> S.getWord16le
    Four  -> toInteger <$> S.getWord32le
    Eight -> toInteger <$> S.getWord64le
  S.label [i|#{len} elements|] $ getListOfWithKnownLen len ga

putListOfWithSizeLen :: SizeLength -> S.Putter a -> S.Putter [a]
putListOfWithSizeLen sl pa ls = do
  putLenWithSizeLen sl $ length ls
  mapM_ pa ls

putLenWithSizeLen :: SizeLength -> S.Putter Int
putLenWithSizeLen sl len = case sl of
  One   -> S.putWord8    $ fromIntegral len
  Two   -> S.putWord16le $ fromIntegral len
  Four  -> S.putWord32le $ fromIntegral len
  Eight -> S.putWord64le $ fromIntegral len


-- * Map *

getMapOfWithSizeLen :: Ord k => SizeLength -> S.Get k -> S.Get v -> S.Get (Map k v)
getMapOfWithSizeLen sl gt gv = S.label "Map" $ Map.fromList <$> getListOfWithSizeLen sl (S.getTwoOf gt gv)

putMapOfWithSizeLen :: SizeLength -> S.Putter k -> S.Putter v -> S.Putter (Map k v)
putMapOfWithSizeLen sl pv pk = putListOfWithSizeLen sl (S.putTwoOf pv pk) . Map.toList


-- * Text *

getText :: S.Get Text
getText = do
  txt <- S.label "Text" $ Text.decodeUtf8' . BS.pack <$> getListOfWithSizeLen Four S.get
  case txt of
    Left err -> fail [i|Could not decode Text: #{err}|]
    Right txt' -> pure txt'

-- Serialize text in utf8 encoding. The liength is output as 4 bytes, little endian.
putText :: S.Putter Text
putText = putListOfWithSizeLen Four S.put . BS.unpack . Text.encodeUtf8
