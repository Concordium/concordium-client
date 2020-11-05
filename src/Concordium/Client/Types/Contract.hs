{-# LANGUAGE DeriveGeneric #-}
module Concordium.Client.Types.Contract where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Serialize (Get, Putter, Serialize, get, getTwoOf, getWord8, getWord32le, put, putTwoOf, putWord8, putWord32le)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word32)
import GHC.Generics

-- ** Data Types and Instances **

-- |Parallel to Contract defined in contracts-common (Rust).
-- Must stay in sync.
data Contract
  =  Contract -- ^ Describes all the schemas of a smart contract.
  {  state :: Maybe RsType -- ^ The optional contract state
  ,  methodParameter :: HashMap Text RsType -- ^ Named method parameters
  }
  deriving (Eq, Show)

instance Serialize Contract where
  get = Contract <$> get <*> getMapOfWith32leLen getText get
  put Contract {..} = put state *> putMapOfWith32leLen putText put methodParameter

-- |Parallel to Fields defined in contracts-common (Rust).
-- Must stay in sync.
data Fields
  = Named [(Text, RsType)] -- ^ Represents an unnamed enum or struct.
  | Unnamed [RsType] -- ^ Represents an unnamed struct or struct.
  | Unit -- ^ The leaf node.
  deriving (Eq, Generic, Show)

instance Hashable Fields

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

-- ** Helpers **

encode :: Contract -> ByteString
encode = S.encode

decode :: ByteString -> Either String Contract
decode = S.decode


-- ** List **

-- Identical to the Data.Serialize.getListOf implementation except for the length format.
getListOfWith32leLen :: Get a -> Get [a]
getListOfWith32leLen ga = go [] =<< getWord32le
  where
    go as 0 = return $! reverse as
    go as i = do x <- ga
                 x `seq` go (x:as) (i - 1)

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
