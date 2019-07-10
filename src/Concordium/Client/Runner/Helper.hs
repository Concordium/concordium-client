module Concordium.Client.Runner.Helper
  ( loadContextData
  , writeContextData
  , showLocalModules
  ) where

import qualified Acorn.Interpreter.Primitives as Prim
import qualified Acorn.Parser                 as Parser
import qualified Acorn.Parser.Runner          as PR
import qualified Acorn.Utils.Init             as Init
import qualified Data.HashMap.Strict          as Map

import qualified Data.Text.IO                 as TextIO
import           System.Directory

-- |Loads the ".cache" file in current directory and generates a Context with it
loadContextData :: IO PR.ContextData
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
writeContextData :: PR.ContextData -> IO ()
writeContextData = PR.writeContextData ".cache"

-- |Show the modules contained in the Context omiting the base ones
showLocalModules :: PR.ContextData -> IO ()
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
