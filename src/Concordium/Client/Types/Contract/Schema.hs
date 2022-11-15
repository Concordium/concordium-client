{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.Contract.Schema(
  ContractSchemaV0(..),
  ContractSchemaV1(..),
  ContractSchemaV2(..),
  ContractSchemaV3(..),
  Fields(..),
  FuncName(..),
  ModuleSchema(..),
  SchemaType(..),
  SizeLength(..),
  FunctionSchemaV1(..),
  FunctionSchemaV2(..),
  EventSchemaV3,
  decodeEmbeddedSchema,
  decodeEmbeddedSchemaAndExports,
  decodeModuleSchema,
  decodeVersionedModuleSchema,
  getListOfWithKnownLen,
  getListOfWithSizeLen,
  lookupFunctionSchemaV1,
  lookupFunctionSchemaV2,
  lookupFunctionSchemaV3,
  lookupEventSchema,
  lookupParameterSchema,
  lookupReturnValueSchema,
  lookupErrorSchema,
  putLenWithSizeLen) where

import Control.Monad (unless)
import qualified Concordium.Wasm as Wasm
import Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Key as AE
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
import Data.Maybe(isJust)
import Control.Arrow (Arrow(first))
import Data.List (sort, group)

-- |Try to find an embedded schema in a module and decode it.
decodeEmbeddedSchema :: Wasm.WasmModule -> Either String (Maybe ModuleSchema)
decodeEmbeddedSchema = fmap fst . decodeEmbeddedSchemaAndExports

-- |Decode a `ModuleSchema`.
decodeModuleSchema :: Wasm.WasmVersion -> BS.ByteString -> Either String ModuleSchema
decodeModuleSchema wasmVersion = S.runGet $ getModuleSchema wasmVersion

-- |Decode a `ModuleSchema` that is explicitly versioned.
decodeVersionedModuleSchema :: BS.ByteString -> Either String ModuleSchema
decodeVersionedModuleSchema = S.runGet getVersionedModuleSchema

-- |Try to find an embedded schema and a list of exported function names and decode them.
decodeEmbeddedSchemaAndExports :: Wasm.WasmModule -> Either String (Maybe ModuleSchema, [Text])
decodeEmbeddedSchemaAndExports wasmModule = S.runGet (getEmbeddedSchemaAndExportsFromModule wasmVersion) moduleSource
  where moduleSource = Wasm.wasmSource wasmModule
        wasmVersion = Wasm.wasmVersion wasmModule

-- |Lookup schema for the parameter of a function.
lookupParameterSchema :: ModuleSchema -> FuncName -> Maybe SchemaType
lookupParameterSchema moduleSchema funcName =
  case moduleSchema of
    ModuleSchemaV0 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms0ContractSchemas >>= cs0InitSig
      ReceiveFuncName contrName receiveName -> do
        contract <- Map.lookup contrName ms0ContractSchemas
        Map.lookup receiveName (cs0ReceiveSigs contract)
    ModuleSchemaV1 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms1ContractSchemas >>= cs1InitSig >>= getParameterSchemaV1
      ReceiveFuncName contrName receiveName -> do
        contract <- Map.lookup contrName ms1ContractSchemas
        Map.lookup receiveName (cs1ReceiveSigs contract) >>= getParameterSchemaV1
    ModuleSchemaV2 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms2ContractSchemas >>= cs2InitSig >>= getParameterSchemaV2
      ReceiveFuncName contrName receiveName -> do
        contract <- Map.lookup contrName ms2ContractSchemas
        Map.lookup receiveName (cs2ReceiveSigs contract) >>= getParameterSchemaV2
    ModuleSchemaV3 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms3ContractSchemas >>= cs3InitSig >>= getParameterSchemaV2
      ReceiveFuncName contrName receiveName -> do
        contract <- Map.lookup contrName ms3ContractSchemas
        Map.lookup receiveName (cs3ReceiveSigs contract) >>= getParameterSchemaV2

-- |Lookup schema for the return value of a function.
--  Always returns Nothing on V0 contracts as they do not have return values.
lookupReturnValueSchema :: ModuleSchema -> FuncName -> Maybe SchemaType
lookupReturnValueSchema moduleSchema funcName =
  case moduleSchema of
    ModuleSchemaV0 {} -> Nothing
    ModuleSchemaV1 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms1ContractSchemas >>= cs1InitSig >>= getReturnValueSchemaV1
      ReceiveFuncName contrName _ -> do
        contrSchema <- Map.lookup contrName ms1ContractSchemas
        lookupFunctionSchemaV1 contrSchema funcName >>= getReturnValueSchemaV1
    ModuleSchemaV2 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms2ContractSchemas >>= cs2InitSig >>= getReturnValueSchemaV2
      ReceiveFuncName contrName _ -> do
        contrSchema <- Map.lookup contrName ms2ContractSchemas
        lookupFunctionSchemaV2 contrSchema funcName >>= getReturnValueSchemaV2
    ModuleSchemaV3 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms3ContractSchemas >>= cs3InitSig >>= getReturnValueSchemaV2
      ReceiveFuncName contrName _ -> do
        contrSchema <- Map.lookup contrName ms3ContractSchemas
        lookupFunctionSchemaV3 contrSchema funcName >>= getReturnValueSchemaV2

-- |Lookup schema for the error of a function.
--  Always returns Nothing on schemas with version < 2 as they don't have error schemas.
lookupErrorSchema :: ModuleSchema -> FuncName -> Maybe SchemaType
lookupErrorSchema moduleSchema funcName =
  case moduleSchema of
    ModuleSchemaV0 {} -> Nothing
    ModuleSchemaV1 {} -> Nothing
    ModuleSchemaV2 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms2ContractSchemas >>= cs2InitSig >>= getReturnValueSchemaV2
      ReceiveFuncName contrName _ -> do
        contrSchema <- Map.lookup contrName ms2ContractSchemas
        lookupFunctionSchemaV2 contrSchema funcName >>= getErrorSchemaV2
    ModuleSchemaV3 {..} -> case funcName of
      InitFuncName contrName -> Map.lookup contrName ms3ContractSchemas >>= cs3InitSig >>= getReturnValueSchemaV2
      ReceiveFuncName contrName _ -> do
        contrSchema <- Map.lookup contrName ms3ContractSchemas
        lookupFunctionSchemaV3 contrSchema funcName >>= getErrorSchemaV2

-- |Lookup event schema for a contract
--  Always returns Nothing on schemas with version < 3 as they don't have event schemas.
lookupEventSchema :: ModuleSchema -> Text -> Maybe SchemaType
lookupEventSchema moduleSchema contrName =
  case moduleSchema of
    ModuleSchemaV0 {} -> Nothing
    ModuleSchemaV1 {} -> Nothing
    ModuleSchemaV2 {} -> Nothing
    ModuleSchemaV3 {..} -> Map.lookup contrName ms3ContractSchemas >>= cs3EventSchema

-- |Lookup the 'FunctionSchemaV1' of a 'ContractSchemaV1'.
lookupFunctionSchemaV1 :: ContractSchemaV1 -> FuncName -> Maybe FunctionSchemaV1
lookupFunctionSchemaV1 ContractSchemaV1{..} = \case
  InitFuncName _ -> cs1InitSig
  ReceiveFuncName _ rcvName -> Map.lookup rcvName cs1ReceiveSigs

-- |Lookup the 'FunctionSchemaV2' of a 'ContractSchemaV2'.
lookupFunctionSchemaV2 :: ContractSchemaV2 -> FuncName -> Maybe FunctionSchemaV2
lookupFunctionSchemaV2 ContractSchemaV2{..} = \case
  InitFuncName _ -> cs2InitSig
  ReceiveFuncName _ rcvName -> Map.lookup rcvName cs2ReceiveSigs

-- |Look up either the schema of an init or a receive function.
lookupFunctionSchemaV3 :: ContractSchemaV3 -> FuncName -> Maybe FunctionSchemaV2
lookupFunctionSchemaV3 ContractSchemaV3{..} = \case
  InitFuncName _ -> cs3InitSig
  ReceiveFuncName _ rcvName -> Map.lookup rcvName cs3ReceiveSigs

-- |Represents the schema for a module.
-- V0 is a parallel to `Module` defined in concordium-contracts-common version <= 2.
-- V1 is a parallel to `Module` defined in concordium-contracts-common version > 2.
data ModuleSchema
  = ModuleSchemaV0 { ms0ContractSchemas :: Map Text ContractSchemaV0 }
  | ModuleSchemaV1 { ms1ContractSchemas :: Map Text ContractSchemaV1 }
  | ModuleSchemaV2 { ms2ContractSchemas :: Map Text ContractSchemaV2 }
  | ModuleSchemaV3 { ms3ContractSchemas :: Map Text ContractSchemaV3 }
  deriving (Eq, Show, Generic)

-- |Create a getter based on the wasm version.
-- Will first attempt to parse the module schema as versioned and if it fails,
-- it will attempt parsing based on the wasmVersion.
getModuleSchema :: Wasm.WasmVersion -> S.Get ModuleSchema
getModuleSchema wasmVersion = S.label "ModuleSchema" $ do
  prefix :: Word16 <- S.lookAhead S.get
  if prefix == maxBound then
    getVersionedModuleSchema
  else
    getUnversionedModuleSchema wasmVersion

-- |Getter for module schema with magic prefix and version.
getVersionedModuleSchema :: S.Get ModuleSchema
getVersionedModuleSchema = S.label "ModuleSchema" $ do
  prefix :: Word16 <- S.get
  if prefix /= maxBound then
    fail "Failed parsing versioned schema. A versioned schema must be prefixed with two maxed-out bytes."
  else do
    version :: Word8 <- S.get
    case version of
      0 -> ModuleSchemaV0 <$> getMapOfWithSizeLen Four getText S.get
      1 -> ModuleSchemaV1 <$> getMapOfWithSizeLen Four getText S.get
      2 -> ModuleSchemaV2 <$> getMapOfWithSizeLen Four getText S.get
      3 -> ModuleSchemaV3 <$> getMapOfWithSizeLen Four getText S.get
      v -> fail $ "Unsupported schema version: " ++ show v

-- |Create a getter based on the wasm version.
getUnversionedModuleSchema :: Wasm.WasmVersion -> S.Get ModuleSchema
getUnversionedModuleSchema wasmVersion = S.label "ModuleSchema" $
  case wasmVersion of
    Wasm.V0 -> ModuleSchemaV0 <$> getMapOfWithSizeLen Four getText S.get
    Wasm.V1 -> ModuleSchemaV1 <$> getMapOfWithSizeLen Four getText S.get

-- |Function and event signatures of a smart contract with event schema V0.
-- Parallel to schema::ContractV0 defined in concordium-contracts-common (Rust) version <= 2.
data ContractSchemaV0
  =  ContractSchemaV0 -- ^ Describes the schemas of a V0 smart contract.
  {  cs0State :: Maybe SchemaType -- ^ The optional contract state.
  ,  cs0InitSig :: Maybe SchemaType -- ^ Type signature for the init function.
  ,  cs0ReceiveSigs :: Map Text SchemaType -- ^ Type signatures for the receive functions.
  }
  deriving (Eq, Show, Generic)

instance AE.ToJSON ContractSchemaV0

-- |Function and event signatures of a smart contract with event schema V1.
-- Parallel to schema::ContractV1 defined in concordium-contracts-common (Rust) version > 2.
data ContractSchemaV1
  = ContractSchemaV1 -- ^ Describes the schemas of a V1 smart contract.
  { cs1InitSig :: Maybe FunctionSchemaV1 -- ^ Schema for the init function.
  , cs1ReceiveSigs :: Map Text FunctionSchemaV1 -- ^ Schemas for the receive functions.
  }
  deriving (Eq, Show, Generic)

instance AE.ToJSON ContractSchemaV1

-- |Function and event signatures of a smart contract with event schema V2.
-- Parallel to schema::ContractV2 defined in concordium-contracts-common (Rust) version > 2.
data ContractSchemaV2
  = ContractSchemaV2 -- ^ Describes the schemas of a V1 smart contract.
  { cs2InitSig :: Maybe FunctionSchemaV2 -- ^ Schema for the init function.
  , cs2ReceiveSigs :: Map Text FunctionSchemaV2 -- ^ Schemas for the receive functions.
  }
  deriving (Eq, Show, Generic)

instance AE.ToJSON ContractSchemaV2

-- |Function and event signatures of a smart contract with event schema V3.
-- Parallel to schema::ContractV3 defined in concordium-contracts-common (Rust) version >= 5.
data ContractSchemaV3
  = ContractSchemaV3 -- ^ Describes the schemas of a V1 smart contract.
  { cs3InitSig :: Maybe FunctionSchemaV2 -- ^ Schema for the init function.
  , cs3ReceiveSigs :: Map Text FunctionSchemaV2 -- ^ Schemas for the receive functions.
  , cs3EventSchema :: Maybe SchemaType -- ^ Schemas for the events.
  }
  deriving (Eq, Show, Generic)

instance AE.ToJSON ContractSchemaV3

instance S.Serialize ContractSchemaV0 where
  get = S.label "ContractSchemaV0" $ do
    cs0State <- S.label "cs0State" S.get
    cs0InitSig <- S.label "cs0InitSig" S.get
    cs0ReceiveSigs <- S.label "cs0ReceiveSigs" $ getMapOfWithSizeLen Four getText S.get
    pure ContractSchemaV0{..}
  put ContractSchemaV0 {..} = S.put cs0State <> S.put cs0InitSig <> putMapOfWithSizeLen Four putText S.put cs0ReceiveSigs

instance S.Serialize ContractSchemaV1 where
  get = S.label "ContractSchemaV1" $ do
    cs1InitSig <- S.label "cs1InitSig" S.get
    cs1ReceiveSigs <- S.label "cs1ReceiveSigs" $ getMapOfWithSizeLen Four getText S.get
    pure ContractSchemaV1{..}
  put ContractSchemaV1 {..} = S.put cs1InitSig <> putMapOfWithSizeLen Four putText S.put cs1ReceiveSigs

instance S.Serialize ContractSchemaV2 where
  get = S.label "ContractSchemaV2" $ do
    cs2InitSig <- S.label "cs2InitSig" S.get
    cs2ReceiveSigs <- S.label "cs2ReceiveSigs" $ getMapOfWithSizeLen Four getText S.get
    pure ContractSchemaV2{..}
  put ContractSchemaV2 {..} = S.put cs2InitSig <> putMapOfWithSizeLen Four putText S.put cs2ReceiveSigs

instance S.Serialize ContractSchemaV3 where
  get = S.label "ContractSchemaV3" $ do
    cs3InitSig <- S.label "cs3InitSig" S.get
    cs3ReceiveSigs <- S.label "cs3ReceiveSigs" $ getMapOfWithSizeLen Four getText S.get
    cs3EventSchema <- S.label "cs3EventSigs" S.get
    pure ContractSchemaV3{..}
  put ContractSchemaV3 {..} = S.put cs3InitSig <> putMapOfWithSizeLen Four putText S.put cs3ReceiveSigs

-- |V1 Schema for a function in a V1 smart contract.
-- Can contain a schema for the parameter, return value, or both.
-- Parallel to the schema::FunctionV1 defined in concordium-contract-common (Rust).
data FunctionSchemaV1
  = Parameter SchemaType
  | ReturnValue SchemaType
  | Both { fs1Parameter :: SchemaType
         , fs1ReturnValue :: SchemaType
         }
  deriving (Eq, Show, Generic)

instance AE.ToJSON FunctionSchemaV1

-- |Try to get the parameter schema of a FunctionSchemaV1.
getParameterSchemaV1 :: FunctionSchemaV1 -> Maybe SchemaType
getParameterSchemaV1 = \case
  Parameter schemaType -> Just schemaType
  ReturnValue _ -> Nothing
  Both {..} -> Just fs1Parameter

-- |Try to get the return value schema of a FunctionSchemaV1.
getReturnValueSchemaV1 :: FunctionSchemaV1 -> Maybe SchemaType
getReturnValueSchemaV1 = \case
  Parameter _ -> Nothing
  ReturnValue schemaType -> Just schemaType
  Both {..} -> Just fs1ReturnValue

instance S.Serialize FunctionSchemaV1 where
  get = S.label "FunctionSchemaV1" $ do
    tag <- S.getWord8
    case tag of
      0 -> S.label "Parameter" $ Parameter <$> S.get
      1 -> S.label "ReturnValue" $ ReturnValue <$> S.get
      2 -> S.label "Both" $ do
        fs1Parameter <- S.get
        fs1ReturnValue <- S.get
        return Both {..}
      _ -> fail [i|Invalid FunctionSchemaV1 tag: #{tag}|]
  put fs = case fs of
    Parameter schemaType -> S.putWord8 0 <> S.put schemaType
    ReturnValue schemaType -> S.putWord8 1 <> S.put schemaType
    Both {..} -> S.putWord8 2 <> S.put fs1Parameter <> S.put fs1ReturnValue

-- |V2 Schema for a function in a V1 smart contract.
-- Can contain a schema for the parameter, return value, error, or a combination of these.
-- Parallel to the schema::FunctionV2 defined in concordium-contract-common (Rust).
data FunctionSchemaV2
  = Param SchemaType
  | Rv SchemaType
  | ParamRv { fs2Parameter :: SchemaType
            , fs2ReturnValue :: SchemaType
            }
  | Error SchemaType
  | ParamError { fs2Parameter :: SchemaType
               , fs2Error :: SchemaType
               }
  | RvError { fs2ReturnValue :: SchemaType
            , fs2Error :: SchemaType
            }
  | ParamRvError { fs2Parameter :: SchemaType
                 , fs2ReturnValue :: SchemaType
                 , fs2Error :: SchemaType
                 }
  deriving (Eq, Show, Generic)

-- |V3 Schema for events in a V1 smart contract.
type EventSchemaV3 = SchemaType

instance AE.ToJSON FunctionSchemaV2

-- |Try to get the parameter schema of a FunctionSchemaV2.
getParameterSchemaV2 :: FunctionSchemaV2 -> Maybe SchemaType
getParameterSchemaV2 fs = let (param, _, _) = getMaybeSchemas fs in param

-- |Try to get the return value schema of a FunctionSchemaV2.
getReturnValueSchemaV2 :: FunctionSchemaV2 -> Maybe SchemaType
getReturnValueSchemaV2 fs = let (_, rv, _) = getMaybeSchemas fs in rv

-- |Try to get the error schema of a FunctionSchemaV2.
getErrorSchemaV2 :: FunctionSchemaV2 -> Maybe SchemaType
getErrorSchemaV2 fs = let (_, _, err) = getMaybeSchemas fs in err

-- |Get the schemas for parameter, return value, and error as a triple of Maybes.
getMaybeSchemas :: FunctionSchemaV2 -> (Maybe SchemaType, Maybe SchemaType, Maybe SchemaType)
getMaybeSchemas = \case
  Param param -> (Just param, Nothing, Nothing)
  Rv rv -> (Nothing, Just rv, Nothing)
  ParamRv {..} -> (Just fs2Parameter, Just fs2ReturnValue, Nothing)
  Error err -> (Nothing, Nothing, Just err)
  ParamError {..} -> (Just fs2Parameter, Nothing, Just fs2Error)
  RvError {..} -> (Nothing, Just fs2ReturnValue, Just fs2Error)
  ParamRvError {..} -> (Just fs2Parameter, Just fs2ReturnValue, Just fs2Error)

instance S.Serialize FunctionSchemaV2 where
  get = S.label "FunctionSchemaV1" $ do
    tag <- S.getWord8
    case tag of
      0 -> S.label "Param" $ Param <$> S.get
      1 -> S.label "Rv" $ Rv<$> S.get
      2 -> S.label "ParamRv" $ do
        fs2Parameter <- S.get
        fs2ReturnValue <- S.get
        return ParamRv {..}
      3 -> S.label "Error" $ Error <$> S.get
      4 -> S.label "ParamError" $ do
        fs2Parameter <- S.get
        fs2Error <- S.get
        return ParamError {..}
      5 -> S.label "RvError" $ do
        fs2ReturnValue <- S.get
        fs2Error <- S.get
        return RvError {..}
      6 -> S.label "ParamRvError" $ do
        fs2Parameter <- S.get
        fs2ReturnValue <- S.get
        fs2Error <- S.get
        return ParamRvError {..}
      _ -> fail [i|Invalid FunctionSchemaV2 tag: #{tag}|]
  put fs = case fs of
    Param schemaType -> S.putWord8 0 <> S.put schemaType
    Rv schemaType -> S.putWord8 1 <> S.put schemaType
    ParamRv {..} -> S.putWord8 2 <> S.put fs2Parameter <> S.put fs2ReturnValue
    Error schemaType -> S.putWord8 3 <> S.put schemaType
    ParamError {..} -> S.putWord8 4 <> S.put fs2Parameter <> S.put fs2Error
    RvError {..} -> S.putWord8 5 <> S.put fs2ReturnValue <> S.put fs2Error
    ParamRvError {..} -> S.putWord8 6 <> S.put fs2Parameter <> S.put fs2ReturnValue <> S.put fs2Error

-- |Parallel to Fields defined in contracts-common (Rust).
-- Must stay in sync.
data Fields
  = Named [(Text, SchemaType)] -- ^ Represents a named enum or struct.
  | Unnamed [SchemaType] -- ^ Represents an unnamed enum or struct.
  | None -- ^ Represents an empty enum or struct.
  deriving (Eq, Generic, Show)

instance Hashable Fields

instance AE.ToJSON Fields where
  toJSON (Named fields) = AE.object . map (\(name, value) -> AE.fromText name .= value) $ fields
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
  | ULeb128 Word32
  | ILeb128 Word32
  | ByteList SizeLength
  | ByteArray Word32
  | TaggedEnum (Map Word8 (Text, Fields))
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
    Enum variants -> AE.object ["Enum" .= (toJsonArray . map (\(k, v) -> AE.object [AE.fromText k .= v]) $ variants)]
    String _ -> AE.String "<String>"
    UInt128 -> AE.String "<UInt128>"
    Int128 -> AE.String "<Int128>"
    ContractName _ -> AE.object [ "contract" .= AE.String "<String>" ]
    ReceiveName _ -> AE.object [ "contract" .= AE.String "<String>", "func" .= AE.String "<String>" ]
    ULeb128 _ -> AE.String "<String with unsigned integer>"
    ILeb128 _ -> AE.String "<String with signed integer>"
    ByteList _ -> AE.String "<String with lowercase hex>"
    ByteArray _ -> AE.String "<String with lowercase hex>"
    TaggedEnum taggedVariants ->
      let
        variants = first (pack . show) <$> Map.toList taggedVariants
      in
        AE.object ["TaggedEnum" .= (toJsonArray . map (\(_k, (name, value)) -> AE.object [AE.fromText name .= value]) $ variants)]
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
      27 -> S.label "ULeb128" $ ULeb128 <$> S.getWord32le
      28 -> S.label "ILeb128"  $ ILeb128 <$> S.getWord32le
      29 -> S.label "ByteList" $ ByteList <$> S.get
      30 -> S.label "ByteArray"  $ ByteArray <$> S.getWord32le
      31 -> S.label "TaggedEnum" $ TaggedEnum
        <$> getMapOfWithSizeLenAndPred tEnumPred Four S.getWord8 (S.getTwoOf getText S.get)
      x  -> fail [i|Invalid SchemaType tag: #{x}|]
    where
      -- Predicate for tagged enums. Tags and variant names should be unique.
      allUnique :: (Ord a) => [a] -> Bool
      allUnique xs = length ((group . sort) xs) == length xs
      tEnumPred :: (Ord a, Ord b) => [(a, (b, c))] -> Bool
      tEnumPred ls = allUnique (map fst ls) && allUnique (map (fst . snd) ls)

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
    ULeb128 sl          -> S.putWord8 27 <> S.putWord32le sl
    ILeb128 sl          -> S.putWord8 28 <> S.putWord32le sl
    ByteList sl         -> S.putWord8 29 <> S.put sl
    ByteArray sl        -> S.putWord8 30 <> S.putWord32le sl
    TaggedEnum m        -> S.putWord8 31 <> putMapOfWithSizeLen Four S.putWord8 (S.putTwoOf putText S.put) m

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

-- |Try to find and decode an embedded `ModuleSchema` and a list of exported function
-- names from inside a Wasm module.
getEmbeddedSchemaAndExportsFromModule :: Wasm.WasmVersion -> S.Get (Maybe ModuleSchema, [Text])
getEmbeddedSchemaAndExportsFromModule wasmVersion = do
  mhBs <- S.getByteString 4
  unless (mhBs == wasmMagicHash) $ fail "Unknown magic value. This is likely not a Wasm module."
  vBs <- S.getByteString 4
  unless (vBs == wasmSpecVersion) $ fail "Unsupported Wasm standard version."
  go (Nothing, [])

  where
    schemaIdentifierUnversioned = case wasmVersion of
        Wasm.V0 -> "concordium-schema-v1"
        Wasm.V1 -> "concordium-schema-v2"

    -- |Try to extract a module schema and a list of exported function names from a Wasm module.
    -- According to the WASM specification, there can be at most one export section
    -- and zero or more custom sections. The sections can be located at any position.
    -- This function will fail if multiple custom sections have the name
    -- 'concordium-schema-v1', which is where the schema is stored.
    go :: (Maybe ModuleSchema, [Text]) -> S.Get (Maybe ModuleSchema, [Text])
    go schemaAndFuncNames@(mSchema, mFuncNames) = case schemaAndFuncNames of
      -- Return if both values are found.
      (Just _, _:_) -> return schemaAndFuncNames
      -- Otherwise, keep looking.
      _ -> do
        isEmpty <- S.isEmpty
        if isEmpty then
          -- End of module reached; return the values found.
          return schemaAndFuncNames
        else do
          sectionId <- S.label "sectionId" S.getWord8
          sectionSize <- S.label "sectionSize" $ fromIntegral <$> getLEB128Word32le
          case sectionId of
            -- Custom section (which is where we store the schema).
            0 -> do
              name <- S.label "Custom Section Name" getTextWithLEB128Len
              if name == schemaIdentifierUnversioned || name == "concordium-schema"
              then
                if isJust mSchema
                then fail [i|Module cannot contain multiple custom sections named 'concordium-schema' or '#{schemaIdentifierUnversioned}'.|]
                else do
                  schemaFound <- if name == "concordium-schema" then getVersionedModuleSchema else getUnversionedModuleSchema wasmVersion
                  -- Return if both the schema and funcNames are found, otherwise keep looking for the funcNames.
                  if not $ null mFuncNames
                    then return (Just schemaFound, mFuncNames)
                    else go (Just schemaFound, mFuncNames)
              else S.skip sectionSize *> go schemaAndFuncNames
            -- Export section
            7 -> do
              exports <- getListOfWithLEB128Len (S.getTwoOf getTextWithLEB128Len getExportDescription)

              -- Four types of exports exist. Filter out anything but function exports.
              let funcNamesFound = map fst . filter ((==) Func . snd) $ exports

              -- Return if both the schema and funcNames are found, otherwise keep looking for the schema.
              if isJust mSchema
                then return (mSchema, funcNamesFound)
                else go (mSchema, funcNamesFound)

            -- Any other type of section
            _ -> S.skip sectionSize *> go schemaAndFuncNames

    -- |Get an export description, which has a one-byte tag followed by a LEB128-Word32 index.
    getExportDescription :: S.Get ExportDescription
    getExportDescription = S.label "Export Description" $ do
      tag <- S.getWord8
      _ <- getLEB128Word32le -- Read and skip the indices, as we do not need them.
      case tag of
        0 -> return Func
        1 -> return Table
        2 -> return Memory
        3 -> return Global
        _ -> fail [i|"Invalid Export Description Tag: #{tag}"|]

    -- |Get Text where the length is encoded as LEB128-Word32.
    getTextWithLEB128Len :: S.Get Text
    getTextWithLEB128Len = S.label "Text with LEB128 Length" $ do
      txt <- Text.decodeUtf8' . BS.pack <$> getListOfWithLEB128Len S.get
      case txt of
        Left err -> fail [i|Could not decode Text with LEB128 len: #{err}|]
        Right txt' -> pure txt'

    -- |Get a list of items where the length of the list is encoded as LEB128-Word32.
    getListOfWithLEB128Len :: S.Get a -> S.Get [a]
    getListOfWithLEB128Len getElem = S.label "List with LEB128 length" $ do
      len <- getLEB128Word32le
      getListOfWithKnownLen len getElem

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
    wasmSpecVersion :: BS.ByteString
    wasmSpecVersion = BS.pack [0x01, 0x00, 0x00, 0x00]


-- |The four types of exports allowed in WASM.
data ExportDescription
  = Func
  | Table
  | Memory
  | Global
  deriving Eq

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

-- |Get a map with a specified size length. Fails if the predicate is false when applied to the list of tuples in the map.
getMapOfWithSizeLenAndPred :: (Ord k) => ([(k,v)] -> Bool) -> SizeLength -> S.Get k -> S.Get v -> S.Get (Map k v)
getMapOfWithSizeLenAndPred p sl gt gv = do
  ls <- getListOfWithSizeLen sl (S.getTwoOf gt gv)
  if p ls
  then S.label "Map" $ pure $ Map.fromList ls
  else fail "Predicate failed in deserialization of map."

-- |Get a map with a specified size length.
getMapOfWithSizeLen :: (Ord k) => SizeLength -> S.Get k -> S.Get v -> S.Get (Map k v)
getMapOfWithSizeLen = getMapOfWithSizeLenAndPred (const True)

-- |Put a map with a specified size length.
putMapOfWithSizeLen :: SizeLength -> S.Putter k -> S.Putter v -> S.Putter (Map k v)
putMapOfWithSizeLen sl pv pk = putListOfWithSizeLen sl (S.putTwoOf pv pk) . Map.toList


-- * Text *

getText :: S.Get Text
getText = do
  txt <- S.label "Text" $ Text.decodeUtf8' . BS.pack <$> getListOfWithSizeLen Four S.get
  case txt of
    Left err -> fail [i|Could not decode Text: #{err}|]
    Right txt' -> pure txt'

-- Serialize text in utf8 encoding. The length is output as 4 bytes, little endian.
putText :: S.Putter Text
putText = putListOfWithSizeLen Four S.put . BS.unpack . Text.encodeUtf8
