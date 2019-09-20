{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Concordium.GlobalState.Transactions
import           Concordium.Types
import           Control.Concurrent
import           Control.Monad.Reader
import           Options.Applicative

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.ByteString.Lazy as BSL

import qualified Concordium.Scheduler.Utils.Init.Example as Example
import qualified Concordium.Crypto.SignatureScheme as Sig

data TxOptions = TxOptions {
  -- |What is the starting nonce.
  startNonce :: Nonce,
  -- |How many transactions to send per batch.
  perBatch   :: Int,
  -- |In seconds.
  delay      :: Int,
  -- |File with JSON encoded keys for the source account.
  keysFile :: FilePath
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
  let keys = strOption (long "keyPair" <> short 'k' <> metavar "FILENAME")
  TxOptions . fromIntegral <$> startNonce <*> perBatch <*> delay <*> keys

grpcBackend :: Parser Backend
grpcBackend = GRPC <$> hostParser <*> portParser <*> targetParser

parser :: ParserInfo (Backend, TxOptions)
parser = info (helper <*> ((,) <$> grpcBackend <*> txOptions))
         (fullDesc <> progDesc "Generate transactions for a fixed contract.")

sendTx :: MonadIO m => BareTransaction -> ClientMonad m BareTransaction
sendTx tx = sendTransactionToBaker tx 100 >> return tx

go :: Backend -> Int -> Int -> (Nonce -> BareTransaction) -> Nonce -> IO ()
go backend delay perBatch sign startNonce =
  runInClient backend $! (loop startNonce)

  where loop nonce = do
          let nextNonce = nonce + fromIntegral perBatch
          _ <- mapM (\nnce -> do
                        tx <- sendTx (sign nnce)
                        liftIO $ do
                          putStrLn "Sent transaction."
                          print tx
                    ) [nonce..nextNonce-1]
          liftIO $ do
            -- putStrLn "Sent the following transactions."
            -- mapM_ (print . transactionHeader) sent
            threadDelay (delay * 10^(6::Int))
          loop nextNonce

main :: IO ()
main = do
  (backend, txoptions) <- execParser parser
  AE.eitherDecode <$> BSL.readFile (keysFile txoptions) >>= \case
    Left err -> putStrLn $ "Could not read the keys because: " ++ err
    Right v ->
      case AE.parseEither parseKeys v of
        Left err' -> putStrLn $ "Could not decode JSON because: " ++ err'
        Right keyPair@Sig.KeyPair{..} -> do
          let txBody = btrPayload (Example.makeTransaction True (ContractAddress 0 0) 0)
          let txHeader nonce = makeTransactionHeader Sig.Ed25519 verifyKey (payloadSize txBody) nonce 1000
          let sign nonce = signTransaction keyPair (txHeader nonce) txBody
          go backend (delay txoptions) (perBatch txoptions) sign (startNonce txoptions)

  where parseKeys = AE.withObject "Account keypair" $ \obj -> do
          verifyKey <- obj AE..: "verifyKey"
          signKey <- obj AE..: "signKey"
          return $ Sig.KeyPair{..}
