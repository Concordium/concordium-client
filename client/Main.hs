{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Data.Text.IO as TextIO

import Control.Monad.IO.Class

import qualified Acorn.Utils.Init as Init
import qualified Acorn.ParserRunner as PR
import qualified Acorn.Parser as Parser
import Acorn.Interpreter.Primitives(primFuncs)

import Acorn.Types

import System.Directory

import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize as S

import Options.Applicative
import Data.Semigroup ((<>))

data AppState = AppState { 
                           localContext :: PR.ContextData
                         }

data Options = LoadModule { sourceFile :: !FilePath }
             | ListModules
             | SendTransaction { sourceFile :: !FilePath }
             | GetFinalContractList
             | GetFinalAccountList
             | GetFinalAccountState
             | GetFinalContractState

argparser :: Parser Options
argparser = LoadModule
         <$> strOption
         ( long "load-module"
           <> metavar "FILENAME"
           <> help "Parse module and add it to the local database." )
         <|>
         flag' ListModules
         ( long "list-modules" <> help "List local modules and deployed modules.")
         <|>
         SendTransaction 
         <$> strOption
         ( long "send-transaction"
           <> metavar "FILENAME"
           <> help "Parse transaction in current context and send it to the baker." )
         <|>
         flag' GetFinalContractList
         (long "get-final-contract-list" <> help "List addresses of contract instances appearing on the last finalized block.")
         <|>
         flag' GetFinalAccountList
         (long "get-final-account-list" <> help "List addresses of accounts appearing on the last finalized block.")
         <|>
         flag' GetFinalAccountState
         ( long "get-final-account-state"
           <> help "Get account information for the given account.")
         <|>
         flag' GetFinalContractState
         ( long "get-final-contract-state"
           <> help "Get contract information." )

main :: IO ()
main = process =<< execParser opts
  where
    opts = info (argparser <**> helper)
      ( fullDesc
     <> progDesc ""
     <> header "Simple frontend for communicating with a baker." )

whenWithDef :: Monad m => Bool -> a -> m a -> m a
whenWithDef b def m = if b then m else return def

loadContextData :: IO PR.ContextData
loadContextData = do
  fe <- doesFileExist ".cache"
  whenWithDef fe Init.initialContextData $ do
    res <- PR.readContextData primFuncs ".cache"
    case res of
      Left err -> fail err
      Right cdata -> return cdata

writeContextData :: PR.ContextData -> IO ()
writeContextData = PR.writeContextData ".cache"

-- writeInstanceData :: InstanceInfo -> IO ()
-- writeInstanceData i = do
--   BSL.writeFile ".instancedata" (S.runPutLazy (S.put i))

-- loadInstanceData :: IO InstanceInfo
-- loadInstanceData = do
--   source <- BSL.readFile ".instancedata"
--   let res = S.runGetLazy S.get source
--   case res of
--     Left err -> fail err
--     Right idata -> return idata

showLocalModules :: PR.ContextData -> IO ()
showLocalModules cdata =
  let modinfos = Parser.modNames (PR.parserEnv cdata) in
  -- only print the modules which have been loaded (not the base modules that are loaded in Init)
  mapM_ (\(t, (_, mref, _, _)) -> case Map.lookup mref (PR.modMap cdata) of
                                    Just _ -> putStr "Module " >> TextIO.putStr t >> putStr " with reference " >> print mref
                                    Nothing -> return ()
        )
      (Map.toList modinfos)

process :: Options -> IO ()
process (LoadModule fname) = do
  mdata <- loadContextData
  cdata <- PR.execContext mdata $ do
    source <- liftIO $ TextIO.readFile fname
    PR.processModule source
  putStrLn "Module processed.\nThe following modules are currently in the local database and can be deployed.\n"
  showLocalModules cdata
  writeContextData cdata

process ListModules = do
  putStrLn "The following modules are in the local database.\n"
  mdata <- loadContextData
  showLocalModules mdata

process (SendTransaction fname) = do
  mdata  <- loadContextData
  source <- BSL.readFile fname
  t <- PR.evalContext mdata (PR.processTransaction source)
  let ser = S.runPut (S.put (fst t) <> S.put (snd t))
  BS.putStr ser

process GetFinalAccountList = do
  mm <- BS.getContents
  case S.runGet S.get mm of
    Left err -> putStrLn err
    Right (acc :: [AccountAddress]) -> mapM_ print acc 

process GetFinalContractList = do
  mm <- BS.getContents
  case S.runGet S.get mm of
    Left err -> putStrLn err
    Right (acc :: [ContractAddress]) -> mapM_ print acc 

process GetFinalAccountState = do
  mm <- BS.getContents
  if BS.length mm == 0 then putStrLn "Address was malformed."
  else 
    case S.runGet S.get mm of
      Left err -> putStrLn err
      Right Nothing -> putStrLn "No account with this address."
      Right (Just (a :: AccountInfo)) -> print a

process GetFinalContractState = do
  mm <- BS.getContents
  print (BS.length mm)
  case S.runGet S.get mm of
    Left err -> fail err
    Right Nothing -> putStrLn "No contract with given address."
    Right (Just InstanceInfo{..}) -> do
      putStrLn "Received instance information."
      putStr "Message type: " >> print messageType
      putStr "Local state:\n  " >> print (showValue localState)
      putStr "Amount: " >> print instanceAmount
