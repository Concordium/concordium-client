{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import           Concordium.Client.Commands
import           Concordium.Client.GRPC
import           Concordium.Client.Runner
import           Concordium.GlobalState.Transactions
import           Concordium.Types
import           Control.Concurrent
import           Control.Monad.Reader
import           Options.Applicative

import           Acorn.Core                          as Core
import qualified Acorn.Parser                        as Parser
import           Acorn.Utils.Init
import           Acorn.Utils.Init.TH
import           Concordium.Client.Commands          as COM hiding (networkId)
import           Concordium.Crypto.Ed25519Signature  (randomKeyPair)
import           Concordium.Crypto.SHA256            (Hash (..))
import           Concordium.Crypto.SignatureScheme   (KeyPair,
                                                      SchemeId (Ed25519))
import qualified Concordium.Crypto.SignatureScheme   as Sig
import qualified Concordium.Scheduler.Runner         as Runner
import qualified Concordium.Scheduler.Types          as Types
import qualified Data.FixedByteString                as FBS
import           Data.Foldable
import qualified Data.HashMap.Strict                 as Map
import           Data.Maybe
import qualified Data.Text                           as Text
import           System.Random

import           Data.List.Split

blockPointer :: BlockHash
blockPointer = Hash (FBS.pack (replicate 32 (fromIntegral (0 :: Word))))

mateuszKP :: KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))

makeHeader :: KeyPair -> Nonce -> Energy -> Types.TransactionHeader
makeHeader kp nonce amount = Types.makeTransactionHeader Ed25519 (Sig.verifyKey kp) nonce amount blockPointer

baseStateWithCounter :: (Parser.Env, Core.ModuleName, ProcessedModules)
baseStateWithCounter = foldl' handleFile
                             baseState
                             $(embedFiles [Left  "test/contracts/SimpleAccount.acorn"
                                           ,Left  "test/contracts/SimpleCounter.acorn"]
                                          )

first :: (a, b, c) -> a
first (x, _, _) = x

simpleCounterCtx :: Map.HashMap Text.Text Core.Name
simpleCounterCtx = let (_, _, tms, _) = Parser.modNames (first baseStateWithCounter) Map.! "SimpleCounter" in tms

inCtx :: Text.Text -> Core.Name
inCtx txt = fromJust (Map.lookup txt simpleCounterCtx)

inCtxTm :: Text.Text -> Core.Atom origin
inCtxTm = Core.Var . Core.LocalDef . inCtx

mkTransaction :: Nonce -> Core.Expr Core.UA Core.ModuleName -> Types.Transaction
mkTransaction n dat  = Runner.signTx mateuszKP hdr payload
    where
        hdr = makeHeader mateuszKP n 100000
        payload = Types.encodePayload (Types.Update 0 (ContractAddress 0 0) dat (-1))

{-# INLINE mkTransaction #-}

data TxOptions = TxOptions {
  -- |What is the starting nonce.
  startNonce :: Nonce,
  -- |How many transactions to send per batch.
  perBatch   :: Int,
  -- |In seconds.
  delay      :: Int
  }

txOptions :: Parser TxOptions
txOptions = do
  let startNonce :: Parser Word = option auto (value 1 <>
                                               showDefault <>
                                               long "nonce" <>
                                               metavar "NONCE" <>
                                               help "Nonce to start generation with.")
  let perBatch = option auto (value 10 <>
                              showDefault <> long "batch" <> metavar "NUM" <> help "Size of a batch to send at once.")
  let delay = option auto (value 10 <>
                           showDefault <>
                           long "delay" <>
                           metavar "SECONDS" <>
                           help "Delay between batches.")
  TxOptions . fromIntegral <$> startNonce <*> perBatch <*> delay

grpcBackend :: Parser Backend
grpcBackend = GRPC <$> hostParser <*> portParser <*> targetParser

parser :: ParserInfo (Backend, TxOptions)
parser = info (helper <*> ((,) <$> grpcBackend <*> txOptions)) (fullDesc <> progDesc "Generate transactions for a fixed contract.")

sendTx :: MonadIO m => Core.Expr Core.UA Core.ModuleName -> Nonce -> ClientMonad m Transaction
sendTx dat nonce = do
  let tx = mkTransaction nonce dat
  sendTransactionToBaker tx 100
  return tx

{-# INLINE sendTx #-}

processBatch :: Int -> Core.Expr Core.UA Core.ModuleName -> [Nonce] -> ClientMonad IO ()
processBatch delay dat b = do
  liftIO $ print b
  mapM_ (sendTx dat) b
  liftIO $ threadDelay (delay * 10^(6::Int))

go :: TxOptions -> ClientMonad IO ()
go TxOptions{..} = do
  let dat = Core.App (inCtxTm "Inc") [Core.Literal (Core.Int64 10)]
  foldM_ (\_ a -> processBatch delay dat a) undefined (chunksOf perBatch [startNonce..])

main :: IO ()
main = (\(a,b) -> do
          client <- mkGrpcClient $! GrpcConfig (COM.host a) (COM.port a) (COM.target a)
          client `seq` ((runReaderT . _runClientMonad) (go b) (EnvData client))
       ) =<< execParser parser
