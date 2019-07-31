{-# LANGUAGE FlexibleContexts #-}

module Concordium.Client.Runner.Helper
  ( loadContextData
  , writeContextData
  , showLocalModules
  , outputGRPC
  , printJSON
  ) where

import qualified Acorn.Core                   as Core
import qualified Acorn.Interpreter.Primitives as Prim
import qualified Acorn.Parser                 as Parser
import qualified Acorn.Parser.Runner          as PR
import qualified Acorn.Utils.Init             as Init
import qualified Data.HashMap.Strict          as Map

import qualified Proto.Concordium_Fields      as CF

import           Data.Attoparsec.Lazy         (Result (..), parse)
import           Lens.Simple
import           Network.GRPC.Client.Helpers

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Internal     (w2c)
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Char8   as BSL8
import           Data.Text.Encoding
import qualified Data.Text.IO                 as TextIO
import           System.Directory

-- |Loads the ".cache" file in current directory and generates a Context with it
loadContextData :: IO (PR.ContextData Core.UA)
loadContextData = do
  fe <- doesFileExist ".cache"
  if fe
    then do
      res <- PR.readContextData Prim.primFuncs ".cache"
      case res of
        Left err    -> fail err
        Right cdata -> return cdata
    else return Init.initialContextData

-- |Writes the context into a file named ".cache"
writeContextData :: PR.ContextData Core.UA -> IO ()
writeContextData = PR.writeContextData ".cache"

-- |Show the modules contained in the Context omiting the base ones
showLocalModules :: PR.ContextData Core.UA -> IO ()
showLocalModules cdata =
  let modinfos = Parser.modNames (PR.parserEnv cdata)
   in mapM_
        (\(t, (_, mref, _, _)) ->
           case Map.lookup mref (PR.modMap cdata) of
             Just _ ->
               putStr "Module " >> TextIO.putStr t >> putStr " with reference " >>
               print mref
             Nothing -> return ())
        (Map.toList modinfos)

-- The complexity of the first parameter comes from the return type of
-- rawUnary. See the documentation
-- http://hackage.haskell.org/package/http2-client-grpc-0.7.0.0/docs/Network-GRPC-Client-Helpers.html#v:rawUnary
outputGRPC ::
     (Show a1)
  => Either a2 (Either a1 (a3, b, Either String t))
  -> (t -> IO ())
  -> IO ()
outputGRPC ret f =
  case ret of
    Left _ -> fail "Unable to send consensus query: too much concurrency"
    Right (Right val) -> do
      let response = (\(_, _, g) -> g) val
      case response of
        Left e  -> fail $ "gRPC response error: " ++ e
        Right v -> f v
    Right (Left e) -> fail $ "Unable to send consensus query: " ++ show e

printJSON ret =
  case ret ^? unaryOutput . CF.jsonValue of
    Nothing -> outputGRPC ret print
    Just jsonval ->
      putStrLn . map w2c . BSL.unpack . BSL8.unlines . map encodePretty . values .
      BSL.fromStrict .
      encodeUtf8 $
      jsonval

values :: BSL.ByteString -> [Value]
values s =
  case parse json' s of
    Done rest v -> v : values rest
    Fail rest _ _
      | BSL8.null rest -> []
      | otherwise ->
        error ("Error in gRPC output decoding as a json: " ++ show s)
