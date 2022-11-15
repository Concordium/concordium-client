-- This module contains unit tests testing that the client can correctly parse
-- specific JSON files according to the given schema.
{-# OPTIONS_GHC -Wno-deprecations #-}
module SimpleClientTests.SchemaParsingSpec where

import qualified Data.ByteString as BS
import qualified Data.Aeson as AE

import Concordium.Client.Types.Contract.Parameter
import Concordium.Client.Types.Contract.Schema

import Test.Hspec
import Test.HUnit
import Control.Monad.List (forM, forM_, when)
import Data.Serialize (runPut)


schemaParsingSpec :: Spec
schemaParsingSpec = describe "Smart contract schemas" $ do
  schemaV1Parsing
  schemaV2Parsing
  schemaV3Parsing1
  schemaV3Parsing2

-- Test that parsing a parameter that contains a
-- - ULEB128 value in the amount field.
-- - a set of 32-byte bytearrays
-- - an Option<32-byte bytearray> value
-- works correctly.
schemaV1Parsing :: Spec
schemaV1Parsing = specify "Parse v1 schema" $ do
  schemaBytes <- BS.readFile "test/data/schema0.schema"
  json <- AE.decodeFileStrict "test/data/json0.json"
  case (decodeVersionedModuleSchema schemaBytes, json) of
    (Left err, _) -> assertFailure $ "Could not parse schema: " ++ err
    (_, Nothing) -> assertFailure $ "json0 is not valid JSON."
    (Right moduleSchema, Just value) ->
      case lookupParameterSchema moduleSchema (ReceiveFuncName "recorder" "record") of
        Nothing -> assertFailure "Schema missing for 'recorder'."
        Just schema -> case putJSONUsingSchema schema value of
          Left err -> assertFailure $ "Could not convert JSON according to the provided schema: " ++ err
          Right _ -> return ()

-- | Test that parses a simple (String) parameter using a v2 schema.
schemaV2Parsing :: Spec
schemaV2Parsing = specify "Parse v2 schema" $ do
  schemaBytes <- BS.readFile "test/data/schema1.schema"
  json <- AE.decodeFileStrict "test/data/json1.json"
  case (decodeVersionedModuleSchema schemaBytes, json) of
    (Left err, _) -> assertFailure $ "Could not parse schema: " ++ err
    (_, Nothing) -> assertFailure "json1 is not valid JSON."
    (Right moduleSchema, Just value) ->
      case lookupParameterSchema moduleSchema (ReceiveFuncName "schema" "receive") of
        Nothing -> assertFailure "Schema missing for 'schema' contract."
        Just schema -> case putJSONUsingSchema schema value of
          Left err -> assertFailure $ "Could not convert JSON according to the provided schema: " ++ err
          Right _ -> return ()


-- | Module schema V3 test data prefix.
prefix :: FilePath
prefix = "test/data/cis2_wCCD/"

-- | Read and parse the schema file used for testing
getSchema :: IO SchemaType
getSchema = do
  schemaBytes <- BS.readFile $ prefix <> "cis2_wCCD_schema.bin"
  moduleSchema <- case decodeVersionedModuleSchema schemaBytes of
      (Left err) -> assertFailure $ "Could not parse schema: " ++ err
      (Right moduleSchema) -> return moduleSchema
  case lookupEventSchema moduleSchema "cis2_wCCD" of
    Just schema -> return schema
    Nothing -> assertFailure "No schema for named contract 'cis2_wCCD'."

-- | Test that deserializing and serializing a bytestring using a module schema containing TaggedEnum is the identity function.
schemaV3Parsing1 :: Spec
schemaV3Parsing1 = specify "Deserializing and serializing a bytestring using a module schema containing TaggedEnum is the identity function" $ do
  schema <- getSchema
  -- Assert that raw inputs can be deserialized to JSON according to schema.
  json <- forM [0,1] $ \(i :: Integer) -> do
    eventBytes <- BS.readFile $ prefix <> "event" <> show i <> ".bin"
    case deserializeWithSchema schema eventBytes of
      Left err -> assertFailure $ "Unable to decode event " <> show i <> ": " <> err
      Right val -> return (i, eventBytes, val)
  -- Assert that deserialized values can be encoded to the 
  forM_ json $ \(i, bs, val) -> do
    case serializeWithSchema schema val of
      Left err -> assertFailure $ "Unable to encode event " <> show i <> ": " <> err
      Right bytes -> do
        when (bytes /= bs) $ assertFailure ""

-- | Test that serializing JSON output using a module schema containing a TaggedEnum yields the corresponding expected raw output.
schemaV3Parsing2 :: Spec
schemaV3Parsing2 = specify "Serialize JSON using a module schema containing TaggedEnum" $ do
  schema <- getSchema
  -- Assert that we can parse and serialize all JSON files according to schema.
  res <- forM [0,1] $ \(i :: Integer) -> do
    json <- AE.decodeFileStrict $ prefix <> "event" <> show i <> ".json"
    case json of
      Just value -> case putJSONUsingSchema schema value of
        Left err -> assertFailure $ "Could not convert JSON according to the provided schema: " ++ err
        Right str -> return (i, str)
      _ -> assertFailure "Input is not valid JSON."
  -- Assert that serialized JSON files match expected binary output.
  forM_ res $ \(i, put) -> do
    eventBytes <- BS.readFile $ prefix <> "event" <> show i <> ".bin"
    when (runPut put /= eventBytes) $ assertFailure $ "Serialized JSON does not match expected output for event " <> show i <> "."