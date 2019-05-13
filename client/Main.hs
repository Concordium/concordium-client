{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import qualified Data.Text.IO as TextIO

import qualified Data.Text.Encoding as Text

import Control.Monad.IO.Class

import qualified Data.ByteString.Base16 as BS16

import qualified Acorn.Utils.Init as Init
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler.Runner as SR
import qualified Concordium.GlobalState.Transactions as Types
import qualified Acorn.Parser as Parser
import Acorn.Interpreter.Primitives(primFuncs)
import Concordium.GlobalState.Information

import Concordium.Crypto.SignatureScheme
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.ID.AccountHolder as AH

import qualified Data.FixedByteString as FBS

import Acorn.Types

import System.Directory

import qualified Data.HashMap.Strict as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Serialize as S

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad.Fail
import Prelude hiding(fail)


import qualified Data.Aeson as AE
import qualified Data.Aeson.TH as AETH
import Data.ByteString.Lazy(ByteString)

import Data.Aeson(FromJSON, Value(..), (.:), (.:?), parseJSON, parseJSONList)
import Data.Aeson.Types(typeMismatch)
import Concordium.Crypto.Ed25519Signature(randomKeyPair)

import System.Random

kp :: KeyPair
kp = fst (randomKeyPair (mkStdGen 0))

showKP :: KeyPair -> (BS.ByteString, BS.ByteString)
showKP (KeyPair (SignKey k) (VerifyKey kv)) =
  ((BS16.encode k), (BS16.encode kv))

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


processTransaction :: MonadFail m => ByteString -> PR.Context m Types.Transaction
processTransaction txt = do
  case AE.eitherDecode txt of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t -> SR.transactionHelper t


instance AE.FromJSON SR.TransactionJSON where
  parseJSON (Object v) = do
    thSenderKey <- v .: "verifyKey"
    thNonce <- v .: "nonce"
    thGasAmount <- v .: "gasAmount"
    thFinalizedPointer <- v .: "finalizedPointer"
    let tHeader = Types.makeTransactionHeader Ed25519 thSenderKey thNonce thGasAmount thFinalizedPointer
    tPayload <- v .: "payload"
    tSignKey <- SignKey . fst . BS16.decode . Text.encodeUtf8 <$> (v .: "signKey")
    return $ SR.TJSON tHeader tPayload (KeyPair { signKey = tSignKey, verifyKey = thSenderKey })
  
  parseJSON invalid = typeMismatch "Transaction" invalid


instance FromJSON Nonce where
  parseJSON v = Nonce <$> parseJSON v

instance FromJSON Amount where
  parseJSON v = Amount <$> parseJSON v

instance FromJSON VerifyKey where
  parseJSON v = VerifyKey . fst . BS16.decode . Text.encodeUtf8 <$> parseJSON v

instance FromJSON BlockHash where
  parseJSON v = Hash.Hash . FBS.fromByteString . fst . BS16.decode . Text.encodeUtf8 <$> parseJSON v

instance FromJSON ContractAddress where
  parseJSON (Object v) = do
    contractIndex <- ContractIndex <$> v .: "index"
    contractSubindex <- ContractSubindex <$> v .: "subindex"
    return ContractAddress{..}
  
  parseJSON invalid = typeMismatch "ContractAddress" invalid

-- inherit text instances
instance FromJSON AccountAddress where
  parseJSON v = AH.base58decodeAddr <$> parseJSON v
  parseJSONList v = map AH.base58decodeAddr <$> parseJSONList v

instance FromJSON Address where
  parseJSON (Object v) =  do
    r <- v .:? "accountAddress"
    case r of
      Nothing -> AddressContract <$> (v .: "contractAddress")
      Just a -> return (AddressAccount a)

  parseJSON invalid = typeMismatch "Address" invalid


$(AETH.deriveFromJSON (AETH.defaultOptions { AETH.sumEncoding = AETH.TaggedObject "transactionType" "contents"}) ''SR.PayloadJSON)




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
  t <- PR.evalContext mdata (processTransaction source)
  let ser = S.encode t
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
      putStr "Local state:\n  " >> print localState
      putStr "Amount: " >> print instanceAmount
