{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Concordium.Types.Transactions
import           Concordium.Types
import           Concordium.Types.Execution
import           Control.Concurrent
import           Control.Monad.Reader
import           Options.Applicative
import System.Exit

import Data.Time.Clock
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as Map

data TxOptions = TxOptions {
  -- |What is the starting nonce.
  startNonce :: Nonce,
  -- |How many transactions to send per second.
  tps :: !Int,
  -- |Whether to output a log after each transaction that is sent.
  logit :: !Bool,
  -- |File with JSON encoded keys for the source account.
  keysFile :: !FilePath,
  -- |Optional file with addresses to send to. If not given or empty all
  -- transfers will be self-transfers.
  addressesFile :: !(Maybe FilePath)
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
  let addresses = optional (strOption (long "addresses" <> short 'a' <> metavar "FILENAME"))
  TxOptions . fromIntegral <$> startNonce <*> tps <*> logit <*> keys <*> addresses

parser :: ParserInfo (Backend, TxOptions)
parser = info (helper <*> ((,) <$> backendParser <*> txOptions))
         (fullDesc <> progDesc "Generate transactions for a fixed contract.")

sendTx :: MonadIO m => BareBlockItem -> ClientMonad m BareBlockItem
sendTx tx =
  sendTransactionToBaker tx 100 >>= \case
    Left err -> liftIO $ die err
    Right False -> liftIO $ die $ "Could not send transaction (rejected)."
    Right True -> return tx

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f a = f a >>= iterateM_ f

go :: Backend -> Bool -> Int -> (Nonce -> (Address, BareBlockItem)) -> Nonce -> IO ()
go backend logit tps sign startNonce = do
  startTime <- getCurrentTime
  withClient backend (loop startNonce startTime)

  where loop nextNonce nextTime = do
          let (addr, tx) = sign nextNonce
          _ <- sendTx tx
          liftIO $ do
            when logit $ putStrLn $ "Sent transaction to " ++ show addr ++ " with nonce = " ++ show nextNonce
            ct <- getCurrentTime
            let toWait = diffUTCTime nextTime ct
            when (toWait > 0) $ threadDelay (truncate $ toWait * 1e6)
          loop (nextNonce + 1) (addUTCTime delay nextTime)
        delay = 1 / fromIntegral tps

main :: IO ()
main = do
  (backend, txoptions) <- execParser parser
  
  addresses <-
    case addressesFile txoptions of
      Nothing -> return Nothing
      Just f ->
        AE.eitherDecode <$> BSL.readFile f >>= \case
          Left err -> die $ "Cannot read addresses file: " ++ err
          Right [] -> return Nothing
          Right addrs -> return (Just addrs)

  v <- AE.eitherDecode <$> BSL.readFile (keysFile txoptions) >>= \case
         Left err -> die $ "Could not read the keys because: " ++ err
         Right v -> return v

  case AE.parseEither accountKeysParser v of
    Left err' -> putStrLn $ "Could not decode JSON because: " ++ err'
    Right (selfAddress, keyMap) -> do
      print $ "Using sender account = " ++ show selfAddress
      let txRecepient (Nonce n) =
            case addresses of
              Just addrs -> AddressAccount (addrs !! (fromIntegral n `mod` length addrs))
              Nothing -> AddressAccount selfAddress
      let txBody n = encodePayload (Transfer (txRecepient n) 1)
      let txHeader nonce = TransactionHeader {
            thSender = selfAddress,
            thNonce = nonce,
            thEnergyAmount = 1000,
            thPayloadSize = payloadSize (txBody nonce),
            thExpiry = TransactionExpiryTime maxBound
            }
      let sign nonce = (txRecepient nonce, NormalTransaction $ signTransaction (Map.toList keyMap) (txHeader nonce) (txBody nonce))
      go backend (logit txoptions) (tps txoptions) sign (startNonce txoptions)

  where accountKeysParser = AE.withObject "Account keys" $ \v -> do
          accountAddr <- v AE..: "address"
          accountData <- v AE..: "accountData"
          keyMap <- accountData AE..: "keys"
          return (accountAddr, keyMap)
