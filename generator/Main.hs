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
import           Concordium.Types.Execution
import           Control.Concurrent
import           Control.Monad.Reader
import           Options.Applicative
import Concordium.ID.Account as AH

import Data.Time.Clock
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.ByteString.Lazy as BSL

import qualified Concordium.Crypto.SignatureScheme as Sig

data TxOptions = TxOptions {
  -- |What is the starting nonce.
  startNonce :: Nonce,
  -- |How many transactions to send per second.
  tps :: !Int,
  -- |Whether to output a log after each transaction that is sent.
  logit :: !Bool,
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
  let tps = option auto (value 10 <>
                         showDefault <>
                         long "tps" <>
                         metavar "NUM" <>
                         help "Number of transactions per second.")
  let logit = switch (long "log" <> help "Emit for each transaction sent.")
  let keys = strOption (long "keyPair" <> short 'k' <> metavar "FILENAME")
  TxOptions . fromIntegral <$> startNonce <*> tps <*> logit <*> keys

grpcBackend :: Parser Backend
grpcBackend = GRPC <$> hostParser <*> portParser <*> targetParser

parser :: ParserInfo (Backend, TxOptions)
parser = info (helper <*> ((,) <$> grpcBackend <*> txOptions))
         (fullDesc <> progDesc "Generate transactions for a fixed contract.")

sendTx :: MonadIO m => BareTransaction -> ClientMonad m BareTransaction
sendTx tx = sendTransactionToBaker tx 100 >> return tx

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f a = f a >>= iterateM_ f

go :: Backend -> Bool -> Int -> (Nonce -> BareTransaction) -> Nonce -> IO ()
go backend logit tps sign startNonce = do
  startTime <- getCurrentTime
  runInClient backend (loop startNonce startTime)

  where loop nextNonce nextTime = do
          _ <- sendTx (sign nextNonce)
          liftIO $ do
            when logit $ putStrLn $ "Sent transaction to " ++ show (target backend) ++ " with nonce = " ++ show nextNonce
            ct <- getCurrentTime
            let toWait = diffUTCTime nextTime ct
            when (toWait > 0) $ threadDelay (truncate $ toWait * 1e6)
          loop (nextNonce + 1) (addUTCTime delay nextTime)
        delay = 1 / fromIntegral tps

main :: IO ()
main = do
  (backend, txoptions) <- execParser parser
  AE.eitherDecode <$> BSL.readFile (keysFile txoptions) >>= \case
    Left err -> putStrLn $ "Could not read the keys because: " ++ err
    Right v ->
      case AE.parseEither parseKeys v of
        Left err' -> putStrLn $ "Could not decode JSON because: " ++ err'
        Right keyPair@Sig.KeyPair{..} -> do
          let selfAddress = AH.accountAddress verifyKey Sig.Ed25519
          print $ "Using sender account = " ++ show selfAddress
          let txBody = encodePayload (Transfer (AddressAccount selfAddress) 1) -- transfer 1 GTU to myself.
          let txHeader nonce = makeTransactionHeader Sig.Ed25519 verifyKey (payloadSize txBody) nonce 1000
          let sign nonce = signTransaction keyPair (txHeader nonce) txBody
          go backend (logit txoptions) (tps txoptions) sign (startNonce txoptions)

  where parseKeys = AE.withObject "Account keypair" $ \obj -> do
          verifyKey <- obj AE..: "verifyKey"
          signKey <- obj AE..: "signKey"
          return $ Sig.KeyPair{..}
