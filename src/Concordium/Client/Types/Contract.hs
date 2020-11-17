{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Types.Contract
  ( addSchemaToInfo
  , decodeSchema
  , encodeSchema
  , runGetRsValueAsJSON
  , serializeParams
  , Fields(..)
  , Info(..)
  , Model(..)
  , RsType(..)
  , Schema(..)
  , SizeLength(..)
  ) where

import Concordium.ID.Types (addressFromText)
import Concordium.Types (AccountAddress, Amount, ContractAddress(..), ContractIndex(..), ContractSubindex(..))

import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, object, toJSON, (.=), (.:))
import qualified Data.Aeson as AE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Serialize (Get, Put, Putter, Serialize, get, getInt8, getInt16le, getInt32le, getInt64le,
                       getTwoOf, getWord8, getWord16le, getWord32le, getWord64le, put, putInt8, putInt16le,
                       putInt32le, putInt64le, putTwoOf, putWord8, putWord16le, putWord32le, putWord64le)
import qualified Data.Serialize as S
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import Data.Word (Word32)
import GHC.Generics

-- ** Data Types and Instances **

-- |Parallel to Contract defined in contracts-common (Rust).
-- Must stay in sync.
data Schema
  =  Schema -- ^ Describes all the schemas of a smart contract.
  {  state :: Maybe RsType -- ^ The optional contract state
  ,  methodParameter :: HashMap Text RsType -- ^ Named method parameters
  }
  deriving (Eq, Show)

instance Serialize Schema where
  get = Schema <$> get <*> getMapOfWith32leLen getText get
  put Schema {..} = put state *> putMapOfWith32leLen putText put methodParameter

-- |Parallel to Fields defined in contracts-common (Rust).
-- Must stay in sync.
data Fields
  = Named [(Text, RsType)] -- ^ Represents an unnamed enum or struct.
  | Unnamed [RsType] -- ^ Represents an unnamed struct or struct.
  | Unit -- ^ The leaf node.
  deriving (Eq, Generic, Show)

instance Hashable Fields

instance ToJSON Fields

instance Serialize Fields where
  get = do
    tag <- getWord8
    case tag of
      0 -> Named <$> getListOfWith32leLen (getTwoOf getText get)
      1 -> Unnamed <$> getListOfWith32leLen get
      2 -> pure Unit
      x -> fail [i|Invalid Fields tag: #{x}|]

  put schema = case schema of
    Named pairs -> putWord8 0 *> putListOfWith32leLen (putTwoOf putText put) pairs
    Unnamed rsTypes -> putWord8 1 *> putListOfWith32leLen put rsTypes
    Unit -> putWord8 2

-- |Parallel to Type defined in contracts-common (Rust).
-- Must stay in sync.
data RsType =
    RsUnit
  | RsBool
  | RsU8
  | RsU16
  | RsU32
  | RsU64
  | RsI8
  | RsI16
  | RsI32
  | RsI64
  | RsAccountAddress
  | RsContractAddress
  | RsOption RsType
  | RsPair RsType RsType
  | RsString SizeLength
  | RsList SizeLength RsType
  | RsSet SizeLength RsType
  | RsMap SizeLength RsType RsType
  | RsArray Word32 RsType
  | RsStruct Fields
  | RsEnum [(Text, Fields)]
  deriving (Eq, Generic, Show)

instance Hashable RsType

instance ToJSON RsType

instance Serialize RsType where
  get = do
    tag <- getWord8
    case tag of
      0  -> pure RsUnit
      1  -> pure RsBool
      2  -> pure RsU8
      3  -> pure RsU16
      4  -> pure RsU32
      5  -> pure RsU64
      6  -> pure RsI8
      7  -> pure RsI16
      8  -> pure RsI32
      9  -> pure RsI64
      10 -> pure RsAccountAddress
      11 -> pure RsContractAddress
      12 -> RsOption <$> get
      13 -> RsPair <$> get <*> get
      14 -> RsString <$> get
      15 -> RsList <$> get <*> get
      16 -> RsSet <$> get <*> get
      17 -> RsMap <$> get <*> get <*> get
      18 -> RsArray <$> get <*> get
      19 -> RsStruct <$> get
      20 -> RsEnum <$> getListOfWith32leLen (getTwoOf getText get)
      x  -> fail [i|Invalid RsType tag: #{x}|]

  put rsType = case rsType of
    RsUnit -> putWord8 0
    RsBool -> putWord8 1
    RsU8   -> putWord8 2
    RsU16  -> putWord8 3
    RsU32  -> putWord8 4
    RsU64  -> putWord8 5
    RsI8   -> putWord8 6
    RsI16  -> putWord8 7
    RsI32  -> putWord8 8
    RsI64  -> putWord8 9
    RsAccountAddress  -> putWord8 10
    RsContractAddress -> putWord8 11
    RsOption a  -> putWord8 12 *> put a
    RsPair a b  -> putWord8 13 *> put a *> put b
    RsString sl -> putWord8 14 *> put sl
    RsList sl a -> putWord8 15 *> put sl *> put a
    RsSet sl a  -> putWord8 16 *> put sl *> put a
    RsMap sl k v    -> putWord8 17 *> put sl *> put k *> put v
    RsArray len a   -> putWord8 18 *> put len *> put a
    RsStruct fields -> putWord8 19 *> put fields
    RsEnum enum     -> putWord8 20 *> putListOfWith32leLen (putTwoOf putText put) enum

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
      ciAmount :: !Amount
      -- |The owner of the contract.
    , ciOwner  :: !AccountAddress
      -- |The contract state.
    , ciModel  :: !Model }
  deriving (Eq, Show)

instance FromJSON Info where
  parseJSON = AE.withObject "Info" $ \v -> Info
      <$> v .: "amount"
      <*> v .: "owner"
      <*> v .: "model"

instance ToJSON Info where
  toJSON Info {..} = object
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
      Nothing -> return AE.Null
      Just rsType -> getRsValueAsJSON rsType
  return $ object ["state" .= state', "method_parameters" .= methodParameter]


getRsValueAsJSON :: RsType -> Get AE.Value
getRsValueAsJSON rsType = case rsType of
  RsUnit -> return AE.Null
  RsBool -> AE.Bool <$> get
  RsU8   -> toJSON <$> getWord8
  RsU16  -> toJSON <$> getWord16le
  RsU32  -> toJSON <$> getWord32le
  RsU64  -> toJSON <$> getWord64le
  RsI8   -> toJSON <$> getInt8
  RsI16  -> toJSON <$> getInt16le
  RsI32  -> toJSON <$> getInt32le
  RsI64  -> toJSON <$> getInt64le
  RsAccountAddress  -> toJSON <$> (get :: Get AccountAddress)
  RsContractAddress -> toJSON <$> (ContractAddress <$> (ContractIndex <$> getWord64le) <*> (ContractSubindex <$> getWord64le))
  RsOption optType -> do
    some <- getWord8
    case some of
      0 -> return AE.Null
      1 -> getRsValueAsJSON optType
      _ -> fail "Invalid some tag for Option."
  RsPair a b -> do
    l <- getRsValueAsJSON a
    r <- getRsValueAsJSON b
    return $ AE.toJSON [l, r]
  RsString sl        -> toJSON <$> getTextWithSizeLen sl
  RsList sl elemType -> toJSON <$> getListOfWithSizeLen sl (getRsValueAsJSON elemType)
  RsSet sl elemType  -> toJSON <$> getListOfWithSizeLen sl (getRsValueAsJSON elemType)
  RsMap sl keyType valType -> toJSON <$> getListOfWithSizeLen sl (getTwoOf (getRsValueAsJSON keyType) (getRsValueAsJSON valType))
  RsArray len elemType     -> toJSON <$> getListOfWithKnownLen len (getRsValueAsJSON elemType)
  RsStruct fields -> toJSON <$> getFieldsAsJSON fields
  RsEnum variants -> do
    idx <- if length variants <= 256
           then fromIntegral <$> getWord8
           else fromIntegral <$> getWord32le
    (name, fields) <- case variants !? idx of
                      Just v -> return v
                      Nothing -> fail [i|Variant with index #{idx} does not exist for Enum.|]
    fields' <- getFieldsAsJSON fields
    return $ object [name .= fields']
  where
    getFieldsAsJSON :: Fields -> Get AE.Value
    getFieldsAsJSON fields = case fields of
      Named pairs -> toJSON <$> traverse getPair pairs
      Unnamed xs  -> toJSON <$> traverse getRsValueAsJSON xs
      Unit        -> return $ AE.Array mempty
      where getPair (k, v) = (\val -> object [k .= val]) <$> getRsValueAsJSON v

    -- Slightly simplified version of Data.List.Safe.(!!)
    (!?) :: [a] -> Integer -> Maybe a
    (!?) [] _ = Nothing
    (!?) (x:xs) n | n == 0 = Just x
                  | n < 0 = Nothing
                  | otherwise = (!?) xs (n-1)

putJSONParams :: RsType -> AE.Value -> Either String Put
putJSONParams rsType json = case (rsType, json) of
  (RsUnit, AE.Null)    -> pure mempty
  (RsBool, AE.Bool b)  -> pure $ put b
  (RsU8,  AE.Number x) -> putWord8    <$> fromScientific x
  (RsU16, AE.Number x) -> putWord16le <$> fromScientific x
  (RsU32, AE.Number x) -> putWord32le <$> fromScientific x
  (RsU64, AE.Number x) -> putWord64le <$> fromScientific x
  (RsI8,  AE.Number x) -> putInt8     <$> fromScientific x
  (RsI16, AE.Number x) -> putInt16le  <$> fromScientific x
  (RsI32, AE.Number x) -> putInt32le  <$> fromScientific x
  (RsI64, AE.Number x) -> putInt64le  <$> fromScientific x

  (RsAccountAddress, AE.String s)    -> put <$> addressFromText s -- TODO: better error msg?

  (RsContractAddress, AE.Object obj) -> case HM.toList obj of
    [("index", AE.Number idx)] -> putContrAddr idx 0
    [("index", AE.Number idx), ("subindex", AE.Number subidx)] -> putContrAddr idx subidx
    [("subindex", AE.Number subidx), ("index", AE.Number idx)] -> putContrAddr idx subidx
    _ -> Left [i|Invalid ContractAddress: #{obj}|]

  (RsOption optType, opt)  -> case opt of -- TODO: Remove
    AE.Null -> pure $ putWord8 0
    val -> do
      let putTag = putWord8 1
      putVal <- putJSONParams optType val
      pure $ putTag <> putVal

  (RsPair ta tb, AE.Array vec) -> case V.toList vec of
    [a, b] -> do
      putA <- putJSONParams ta a
      putB <- putJSONParams tb b
      pure $ putA <> putB
    xs -> Left [i|#{xs} is not a pair.|]

  (RsString sl, AE.String str) -> do
    let len = fromIntegral . Text.length $ str
        maxLen = maxSizeLen sl
    when (len > maxLen) $ Left $ tooLongError "String" maxLen len
    pure $ putTextWithSizeLen sl str

  (RsList sl elemType, AE.Array vec) -> do
    let len = fromIntegral . V.length $ vec
        maxLen = maxSizeLen sl
    when (len > maxLen) $ Left $ tooLongError "List" maxLen len
    putListLike sl elemType (V.toList vec)

  (RsSet sl elemType, AE.Array vec) -> do
    let len = fromIntegral . V.length $ vec
        maxLen = maxSizeLen sl
        ls = V.toList vec
    when (len > maxLen) $ Left $ tooLongError "Set" maxLen len
    unless (allUnique ls) $ Left [i|Invalid set. Can only contain unique elements: #{ls}|]
    putListLike sl elemType ls

  (RsMap sl keyType valType, AE.Array vec) -> do
    let len = fromIntegral . V.length $ vec
        maxLen = maxSizeLen sl
        putLen = putLenWithSizeLen sl $ V.length vec
    when (len > maxLen) $ Left $ tooLongError "Map" maxLen len
    putElems <- traverse (putJSONParams (RsPair keyType valType)) vec
    pure . sequence_ $ V.cons putLen putElems

  (RsArray len elemType, AE.Array vec) -> do
    let ls = V.toList vec
        actualLen = length ls
    unless (fromIntegral len == actualLen) $ Left [i|Expected the array to have length #{len}, but it had length #{actualLen}|]
    sequence_ <$> traverse (putJSONParams elemType) ls

  (RsStruct fields, val) -> putJSONFields fields val

  (RsEnum variants, AE.Object obj) -> case HM.toList obj of
    [] -> Left "fail, empty obj"
    [(name, fields)] -> case lookupItemAndIndex name variants of
      Nothing -> Left [i|Enum variant '#{name}' does not exist in enum #{variants}|] -- TODO: too verbose?
      Just (fieldTypes, idx) -> do
        let putLen = if length variants <= 256
                       then putWord8 $ fromIntegral idx
                       else putWord32le $ fromIntegral idx
        putJSONFields' <- putJSONFields fieldTypes fields
        pure $ putLen <> putJSONFields'
    _ -> Left "fail, too many fields to be an enum"

  (type_, val) -> Left [i|#{val} is not of type #{type_}|] -- TODO: implement show manually to match rust type names

  where
    putJSONFields :: Fields -> AE.Value -> Either String Put
    putJSONFields fields val = case (fields, val) of
      (Named pairs, AE.Array vec) -> do
        let ls = V.toList vec
        when (length pairs /= length ls) $ Left "Too few fields were provided." -- TODO: Improve msg
        putNamedUnordered <- traverse (lookupAndPut pairs) ls
        unless (allUnique . map fst $ putNamedUnordered) $ Left "Contains duplicate fields."
        -- The fields entered might be in a different order, so we need to order them correctly.
        pure . traverse_ snd . List.sortOn fst $ putNamedUnordered

      (Unnamed rsTypes, AE.Array vec) -> do
        let ls = V.toList vec
        let expectedLen = length rsTypes
        let actualLen = length ls
        when (expectedLen /= actualLen) $ Left [i|Incorrect number of fields. Expected #{expectedLen} but got #{actualLen} fields.|]
        putUnnamed <- traverse (uncurry putJSONParams) $ zip rsTypes ls
        pure . sequence_ $ putUnnamed

      (Unit, AE.Array vec) -> if V.null vec
                                then pure mempty
                                else Left [i|The type #{Unit} should be represented by an empty Array.|]

      (type_, value) -> Left [i|#{value} is not of type #{type_}|] -- TODO: implement show manually to match rust type names

    putListLike :: SizeLength -> RsType -> [AE.Value] -> Either String Put
    putListLike sl elemType xs = do
      let putLen = putLenWithSizeLen sl $ length xs
      putElems <- traverse (putJSONParams elemType) xs
      pure . sequence_ $ putLen : putElems

    putContrAddr :: Scientific -> Scientific -> Either String Put
    putContrAddr idx subidx = do
      idx' <- fromScientific idx
      subidx' <- fromScientific subidx
      pure $ putWord64le idx' <> putWord64le subidx'

    fromScientific :: (Integral i, Bounded i) => Scientific -> Either String i
    fromScientific x = case toBoundedInteger x of
      Nothing -> Left [i|Could not convert #{x} from a scientific number.|]
      Just x' -> Right x'

    tooLongError :: String -> Integer -> Integer -> String
    tooLongError typeName maxLen actualLen =
      [i|The provided #{typeName} is too long. It has length #{actualLen} and the maximum is #{maxLen}|]

    maxSizeLen :: SizeLength -> Integer
    maxSizeLen = \case
      LenU8 -> 255
      LenU16 -> 65_535
      LenU32 -> 4_294_967_295
      LenU64 -> 18_446_744_073_709_551_615

    lookupAndPut :: [(Text, RsType)] -> AE.Value -> Either String (Int, Put)
    lookupAndPut pairs v = case v of
      AE.Object obj -> case HM.toList obj of
        [] -> Left "fail, empty obj"
        [(name, value)] -> case lookupItemAndIndex name pairs of
          Nothing -> Left "fail, name not found"
          Just (rsType', idx) -> (idx, ) <$> putJSONParams rsType' value
        _ -> Left "fail, too many fields"
      _ -> Left [i|#{v} is not a valid struct.|] -- TODO: improve error msg

    lookupItemAndIndex :: Eq a => a -> [(a, b)] -> Maybe (b, Int)
    lookupItemAndIndex item thePairs = go item thePairs 0
      where go _ [] _ = Nothing
            go x ((a,b):pairs) idx = if x == a
                                    then Just (b, idx)
                                    else go x pairs (idx + 1)

    allUnique :: Eq a => [a] -> Bool
    allUnique xs = length xs == length (List.nub xs)


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
putListOfWithSizeLen sl pa = \ls -> do
  putLenWithSizeLen sl $ length ls
  mapM_ pa ls

putLenWithSizeLen :: SizeLength -> Putter Int
putLenWithSizeLen sl = \len -> case sl of
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
getText = getTextWithSizeLen LenU32

getTextWithSizeLen :: SizeLength -> Get Text
getTextWithSizeLen sl = Text.decodeUtf8 . BS.pack <$> getListOfWithSizeLen sl get

putText :: Putter Text
putText = putTextWithSizeLen LenU32

putTextWithSizeLen :: SizeLength -> Putter Text
putTextWithSizeLen sl = putListOfWithSizeLen sl put . BS.unpack . Text.encodeUtf8


-- ** External API **

-- |Uses a `Schema` to parse the `JustBytes` of `Info`.
-- Returns `Left` if a schema has already been included or the parsing fails.
addSchemaToInfo :: Info -> Schema -> Either String Info
addSchemaToInfo info@Info{..} schema = case ciModel of
  WithSchema _ -> Left "Already contains a schema."
  JustBytes bytes -> (\m -> info {ciModel = WithSchema m}) <$> S.runGet (getModelAsJSON schema) bytes

-- |Serialize JSON parameters to binary using `RsType` or fail with an error message.
serializeParams :: RsType -> AE.Value -> Either String ByteString
serializeParams rsType params = S.runPut <$> putJSONParams rsType params

-- * For Testing *

decodeSchema :: ByteString -> Either String Schema
decodeSchema = S.decode

encodeSchema :: Schema -> ByteString
encodeSchema = S.encode

runGetRsValueAsJSON :: RsType -> ByteString -> Either String AE.Value
runGetRsValueAsJSON rsType = S.runGet (getRsValueAsJSON rsType)
