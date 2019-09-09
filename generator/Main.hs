{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Concordium.GlobalState.Transactions
import           Concordium.Types
import           Control.Concurrent
import           Control.Monad.Reader
import           Options.Applicative

import qualified Concordium.Scheduler.Types              as Types
import qualified Concordium.Scheduler.Utils.Init.Example as Example

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
                              showDefault <>
                              long "batch" <>
                              metavar "NUM" <>
                              help "Size of a batch to send at once.")
  let delay = option auto (value 10 <>
                           showDefault <>
                           long "delay" <>
                           metavar "SECONDS" <>
                           help "Delay between batches.")
  TxOptions . fromIntegral <$> startNonce <*> perBatch <*> delay

grpcBackend :: Parser Backend
grpcBackend = GRPC <$> hostParser <*> portParser <*> targetParser

parser :: ParserInfo (Backend, TxOptions)
parser = info (helper <*> ((,) <$> grpcBackend <*> txOptions))
         (fullDesc <> progDesc "Generate transactions for a fixed contract.")

mkTransaction :: Nonce -> Types.Transaction
mkTransaction = Example.makeTransaction True (ContractAddress 0 0)
{-# INLINE mkTransaction #-}

sendTx :: MonadIO m =>  Nonce -> ClientMonad m Transaction
sendTx nonce = do
  let tx = mkTransaction nonce
  sendTransactionToBaker tx 100
  return tx
{-# INLINE sendTx #-}

processBatch :: Int -> [Nonce] -> ClientMonad IO ()
processBatch delay b = do
  liftIO $ print b
  mapM_ sendTx b
  liftIO $ threadDelay (delay * 10^(6::Int))

-- The `x` parameter should go away once we find why the threads block at around 450 txs
go :: Backend -> TxOptions -> Nonce -> IO Nonce
go backend TxOptions{..} x =  runInClient backend $ loop startNonce x
  where loop nonce x = do
          if nonce < x then do
            let nextNonce = nonce + fromIntegral perBatch
            sent <- mapM sendTx [nonce..nextNonce-1]
            liftIO $ do
              putStrLn "Sent the following transactions."
              mapM_ (print . transactionHeader) sent
              threadDelay (delay * 10^(6::Int))
            loop nextNonce x
          else
            return nonce

-- The `foldM` should go away once we find why the threads block at around 450 txs
main :: IO ()
main = do
  (a, b) <- execParser parser
  _ <- foldM (\x y -> uncurry go (a,b {startNonce = x}) y) (startNonce b) [399,799..]
  return ()
