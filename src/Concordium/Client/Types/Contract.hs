{-# LANGUAGE DeriveGeneric #-}
module Concordium.Client.Types.Contract where

import Concordium.Types (AccountAddress, Amount, ContractAddress)

import Data.Aeson (FromJSON, ToJSON, object, (.=), (.:))
import qualified Data.Aeson as AE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Get, Putter, Serialize, get, getInt8, getInt16le, getInt32le, getInt64le,
                       getTwoOf, getWord8, getWord16le, getWord32le, getWord64le, put, putTwoOf,
                       putWord8, putWord32le)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
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
      x -> fail $ "Invalid Fields tag: " ++ show x

  put schema = case schema of
    Named pairs -> putWord8 0 *> putListOfWith32leLen (putTwoOf putText put) pairs
    Unnamed rsTypes -> putWord8 1 *> putListOfWith32leLen put rsTypes
    Unit -> putWord8 2

-- |Parallel to Type defined in contracts-common (Rust).
-- Must stay in sync.
data RsType =
    RsU8
  | RsU16
  | RsU32
  | RsU64
  | RsString
  | RsUnit
  | RsBool
  | RsBytes
  | RsPair RsType RsType
  | RsStruct Fields
  | RsEnum [(Text, Fields)]
  | RsList RsType
  | RsMap RsType RsType
  | RsSet RsType
  | RsOption RsType
  | RsAccountAddress
  | RsContractAddress
  | RsArray Word32 RsType
  | RsI8
  | RsI16
  | RsI32
  | RsI64
  deriving (Eq, Generic, Show)

instance Hashable RsType

instance ToJSON RsType

instance Serialize RsType where
  get = do
    tag <- getWord8
    case tag of
      0  -> pure RsU8
      1  -> pure RsU16
      2  -> pure RsU32
      3  -> pure RsU64
      4  -> pure RsString
      5  -> pure RsUnit
      6  -> pure RsBool
      7  -> pure RsBytes
      8  -> RsPair <$> get <*> get
      9  -> RsStruct <$> get
      10 -> RsEnum <$> getListOfWith32leLen (getTwoOf getText get)
      11 -> RsList <$> get
      12 -> RsMap <$> get <*> get
      13 -> RsSet <$> get
      14 -> RsOption <$> get
      15 -> pure RsAccountAddress
      16 -> pure RsContractAddress
      17 -> RsArray <$> get <*> get
      18 -> pure RsI8
      19 -> pure RsI16
      20 -> pure RsI32
      21 -> pure RsI64
      x  -> fail $ "Invalid RsType tag: " ++ show x

  put rsType = case rsType of
    RsU8 -> putWord8 0
    RsU16 -> putWord8 1
    RsU32 -> putWord8 2
    RsU64 -> putWord8 3
    RsString -> putWord8 4
    RsUnit -> putWord8 5
    RsBool -> putWord8 6
    RsBytes -> putWord8 7
    RsPair a b -> putWord8 8 *> put a *> put b
    RsStruct fields -> putWord8 9 *> put fields
    RsEnum enum -> putWord8 10 *> putListOfWith32leLen (putTwoOf putText put) enum
    RsList a -> putWord8 11 *> put a
    RsMap k v -> putWord8 12 *> put k *> put v
    RsSet a -> putWord8 13 *> put a
    RsOption a -> putWord8 14 *> put a
    RsAccountAddress -> putWord8 15
    RsContractAddress -> putWord8 16
    RsArray len a -> putWord8 17 *> put len *> put a
    RsI8 -> putWord8 18
    RsI16 -> putWord8 19
    RsI32 -> putWord8 20
    RsI64 -> putWord8 21


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
    , "owner" .= ciOwner
    , "model" .= ciModel ]

data Model =
    WithSchema AE.Value -- ^ The decoded contract state.
  | JustBytes ByteString -- ^ The binary-encoded contract state.
  deriving (Eq, Show)

instance ToJSON Model where
  toJSON (WithSchema val) = val
  toJSON (JustBytes bs) = AE.String $ Text.decodeUtf8 bs

instance FromJSON Model where
  parseJSON (AE.String s) = pure . JustBytes . Text.encodeUtf8 $ s
  parseJSON obj@(AE.Object _) = pure $ WithSchema obj
  parseJSON invalid = fail $ "Invalid model. Should be either a ByteString or a JSON obj: " ++ show invalid

addSchemaToInfo :: Info -> Schema -> Either String Info
addSchemaToInfo info@Info{..} schema = case ciModel of
  WithSchema _ -> Left "Already contains a schema."
  JustBytes bytes -> (\m -> info {ciModel = WithSchema m}) <$> S.runGet (getModel schema) bytes

-- ** VALUES **

decodeSchema :: ByteString -> Either String Schema
decodeSchema = S.decode

encodeSchema :: Schema -> ByteString
encodeSchema = S.encode

getModel :: Schema -> Get AE.Value
getModel Schema{..} = do
  state' <- case state of
      Nothing -> return AE.Null
      Just rsType -> getRsValue rsType
  methodParameter' <- traverse (\(k, v) -> mkNamedUnaryObj k <$> getRsValue v) (HM.toList methodParameter)
  return $ object ["state" .= state', "method_parameters" .= methodParameter']

getFields :: Fields -> Get AE.Value
getFields fields = case fields of
  Named pairs -> mkTypedUnaryObj "named" <$> traverse (\(k, v) -> mkNamedUnaryObj k <$> getRsValue v) pairs
  Unnamed xs  -> mkTypedUnaryObj "unnamed" <$> traverse getRsValue xs
  Unit        -> return $ mkTypedUnaryObj "unit" AE.Null -- TODO: is this correct?

getRsValue :: RsType -> Get AE.Value
getRsValue rsType = case rsType of
  RsU8 -> mkTypedUnaryObj "u8" <$> getWord8
  RsU16 -> mkTypedUnaryObj "u16" <$> getWord16le
  RsU32 -> mkTypedUnaryObj "u32" <$> getWord32le
  RsU64 -> mkTypedUnaryObj "u64" <$> getWord64le
  RsString -> AE.String <$> getText
  RsUnit -> return AE.Null
  RsBool -> AE.Bool <$> get
  RsBytes -> mkTypedUnaryObj "bytes" <$> getListOfWith32leLen getWord8
  RsPair a b -> do
    l <- getRsValue a
    r <- getRsValue b
    return $ object [mkType "pair", "left" .= l, "right" .= r]
  RsStruct fields -> mkTypedUnaryObj "struct" <$> getFields fields
  RsEnum variants -> do
    idx <- if length variants <= 256
           then fromIntegral <$> getWord8
           else fromIntegral <$> getWord32le
    (name, fields) <- case variants !? idx of
                      Just v -> return v
                      Nothing -> fail $ "Variant with index " ++ show idx ++ " does not index for Enum."
    fields' <- getFields fields
    return $ object [mkType "variant", "name" .= name, "fields" .= fields']
  RsList elemType -> mkTypedUnaryObj "list" <$> getListOfWith32leLen (getRsValue elemType)
  RsMap keyType valType -> mkTypedUnaryObj "map" <$> getListOfWith32leLen (getTwoOf (getRsValue keyType) (getRsValue valType))
  RsSet elemType -> mkTypedUnaryObj "set" <$> getListOfWith32leLen (getRsValue elemType)
  RsOption optType -> do
    some <- getWord8
    value <- case some of
               0 -> return AE.Null
               1 -> getRsValue optType
               _ -> fail "Invalid some tag for Option."
    return $ object [mkType "option", "some" .= some, "value" .= value]
  RsAccountAddress -> mkTypedUnaryObj "account_address" <$> (get :: Get AccountAddress)
  RsContractAddress -> mkTypedUnaryObj "contract_address" <$> (get :: Get ContractAddress)
  RsArray len elemType -> mkTypedUnaryObj "array" <$> getListOfWithKnownLen len (getRsValue elemType)
  RsI8 -> mkTypedUnaryObj "i8" <$> getInt8
  RsI16 -> mkTypedUnaryObj "i16" <$> getInt16le
  RsI32 -> mkTypedUnaryObj "i32" <$> getInt32le
  RsI64 -> mkTypedUnaryObj "i64" <$> getInt64le


-- ** Helpers **

mkTypedUnaryObj :: ToJSON v => Text -> v -> AE.Value
mkTypedUnaryObj typeName val = object [mkType typeName, "value" .= val]

mkNamedUnaryObj :: ToJSON v => Text -> v -> AE.Value
mkNamedUnaryObj keyName val = object ["key" .= keyName, "value" .= val]

mkType :: Text -> (Text, AE.Value)
mkType = ("type" .=)


-- ** List **

-- Nearly identical to Data.Serialize.getListOf implementation (except for length).
getListOfWithKnownLen :: Integral len => len -> Get a -> Get [a]
getListOfWithKnownLen len ga = go [] len
  where
    go as 0 = return $! reverse as
    go as i = do x <- ga
                 x `seq` go (x:as) (i - 1)

getListOfWith32leLen :: Get a -> Get [a]
getListOfWith32leLen ga = getWord32le >>= \len -> getListOfWithKnownLen len ga

putListOfWith32leLen :: Putter a -> Putter [a]
putListOfWith32leLen pa = \l -> do
  putWord32le $ fromIntegral $ length l
  mapM_ pa l


-- ** Map **

getMapOfWith32leLen :: (Eq k, Hashable k) => Get k -> Get v -> Get (HashMap k v)
getMapOfWith32leLen gt gv = HM.fromList <$> getListOfWith32leLen (getTwoOf gt gv)

putMapOfWith32leLen :: Putter k -> Putter v -> Putter (HashMap k v)
putMapOfWith32leLen pv pk = putListOfWith32leLen (putTwoOf pv pk) . HM.toList


-- ** Text **

getText :: Get Text
getText = Text.decodeUtf8 . BS.pack <$> getListOfWith32leLen get

putText :: Putter Text
putText = putListOfWith32leLen put . BS.unpack . Text.encodeUtf8


-- ** Other **

-- Slightly simplified version of Data.List.Safe.(!!)
(!?) :: [a] -> Integer -> Maybe a
(!?) [] _ = Nothing
(!?) (x:xs) n | n == 0 = Just x
              | n < 0 = Nothing
              | otherwise = (!?) xs (n-1)
