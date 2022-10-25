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


schemaParsingSpec :: Spec
schemaParsingSpec = describe "Smart contract schemas" $ do
  schemaV1Parsing
  schemaV2Parsing

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
