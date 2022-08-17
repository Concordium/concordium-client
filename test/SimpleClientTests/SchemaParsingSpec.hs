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

-- Test that parsing a parameter that contains a set, a 32-byte array and an
-- enum works correctly.
schemaParsingSpec :: Spec
schemaParsingSpec = specify "Parse schema" $ do
  schemaBytes <- BS.readFile "test/data/schema0.schema"
  json <- AE.decodeFileStrict "test/data/json0.json"
  case (decodeVersionedModuleSchema schemaBytes, json) of
    (Left err, _) -> assertFailure $ "Could not parse schema: " ++ err
    (_, Nothing) -> assertFailure $ "json0 is not valid JSON."
    (Right moduleSchema, Just value) ->
      case lookupParameterSchema moduleSchema (ReceiveFuncName "recorder" "record") of
        Nothing -> assertFailure $ "Schema missing for 'recorder'."
        Just schema -> case putJSONUsingSchema schema value of
          Left err -> assertFailure $ "Could not convert JSON according to the provided schema: " ++ err
          Right _ -> return ()
