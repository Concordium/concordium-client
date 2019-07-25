{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Concordium.Client.Commands
import Concordium.Client.Runner
import Control.Monad.Reader
import Options.Applicative
import Concordium.Client.Types.Transaction
import Concordium.Types
import Concordium.GlobalState.Transactions
import Concordium.Crypto.SignatureScheme
import Concordium.Crypto.Ed25519Signature(randomKeyPair)
import Concordium.Crypto.SHA256
import qualified Concordium.ID.Account as ID
import qualified Concordium.Scheduler.Utils.Init.Example as Example
import Control.Concurrent
import System.Random

mkTransaction :: Nonce -> Transaction
mkTransaction = Example.makeTransaction True (ContractAddress 0 0)

data TxOptions = TxOptions {
  -- |What is the starting nonce.
  startNonce :: Nonce,
  -- |How many transactions to send per batch.
  perBatch :: Int,
  -- |In seconds.
  delay :: Int
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

sendTx :: MonadIO m => Nonce -> ClientMonad m Transaction
sendTx nonce = sendTransactionToBaker (mkTransaction nonce) 100

go backend TxOptions{..} = loop startNonce
  where loop nonce = do
          let nextNonce = nonce + fromIntegral perBatch
          sent <- runInClient backend (mapM sendTx [nonce..nextNonce-1])
          putStrLn "Sent the following transactions."
          mapM_ (print . transactionHeader) sent
          threadDelay (delay * 10^(6::Int))
          loop nextNonce

main :: IO ()
main = uncurry go =<< execParser parser
