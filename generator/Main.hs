{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Concordium.Client.Commands
import Concordium.Client.Runner
import Concordium.Types.Transactions
import Concordium.Types
import Concordium.Types.Execution
import Concordium.ID.Types
import Concordium.Crypto.EncryptedTransfers
import Concordium.Client.Cli
import Concordium.Client.Types.Transaction(encryptedTransferEnergyCost)

import           Control.Concurrent
import           Control.Monad.Reader
import           Options.Applicative
import System.Exit
import Data.Time.Clock
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.HashMap.Strict as Map

data TxOptions = TxOptions {
  -- |How many transactions to send per second.
  tps :: !Int,
  -- |Whether to output a log after each transaction that is sent.
  logit :: !Bool,
  -- |File with JSON encoded keys for the source account.
  keysFile :: !FilePath,
  -- |Optional file with addresses and (optionally) public keys of the accounts to send to.
  -- If not given or empty all transfers will be self-transfers.
  -- This option is mandatory if encrypted transfers are desired.
  -- The format of the file should be a JSON array of objects with keys "address" and "encryptionPublicKey", the latter being optional.
  receiversFile :: !(Maybe FilePath),
  -- |If this is `True` then we send encrypted transfer transactions.
  encrypted :: !Bool
  }

txOptions :: Parser TxOptions
txOptions = do
  let tps = option auto (value 10 <>
                         showDefault <>
                         long "tps" <>
                         metavar "NUM" <>
                         help "Number of transactions per second.")
  let logit = switch (long "log" <> help "Emit for each transaction sent.")
  let keys = strOption (long "keyPair" <> short 'k' <> metavar "FILENAME")
  let addresses = optional (strOption (long "addresses" <> short 'a' <> metavar "FILENAME"))
  let sendEncrypted = switch (long "encrypted" <> help "Whether to send encrypted transfers.")
  TxOptions <$> tps <*> logit <*> keys <*> addresses <*> sendEncrypted

parser :: ParserInfo (Backend, TxOptions)
parser = info (helper <*> ((,) <$> backendParser <*> txOptions))
         (fullDesc <> progDesc "Generate transactions for a fixed contract.")

sendTx :: MonadIO m => BareBlockItem -> ClientMonad m BareBlockItem
sendTx tx =
  sendTransactionToBaker tx 100 >>= \case
    Left err -> liftIO $ die err
    Right False -> liftIO $ die "Could not send transaction (rejected)."
    Right True -> return tx

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ f a = f a >>= iterateM_ f

go :: Backend -> Bool -> Int -> a -> (Nonce -> a -> IO (AccountAddress, BareBlockItem, a)) -> Nonce -> IO ()
go backend logit tps startValue sign startNonce = do
  startTime <- getCurrentTime
  withClient backend (loop startNonce startValue startTime)

  where loop nextNonce nextValue nextTime = do
          (addr, tx, nextValue') <- liftIO $ sign nextNonce nextValue
          _ <- sendTx tx
          liftIO $ do
            when logit $ putStrLn $ "Sent transaction to " ++ show addr ++ " with nonce = " ++ show nextNonce
            ct <- getCurrentTime
            let toWait = diffUTCTime nextTime ct
            when (toWait > 0) $ threadDelay (truncate $ toWait * 1e6)
          loop (nextNonce + 1) nextValue' (addUTCTime delay nextTime)
        delay = 1 / fromIntegral tps

main :: IO ()
main = do
  (backend, txoptions) <- execParser parser

  v <- AE.eitherDecodeFileStrict (keysFile txoptions) >>= \case
         Left err -> die $ "Could not read the keys because: " ++ err
         Right v -> return v

  case AE.parseEither accountKeysParser v of
    Left err' -> putStrLn $ "Could not decode JSON because: " ++ err'
    Right (selfAddress, keyMap, encryptionSecretKey) -> do
      print $ "Using sender account = " ++ show selfAddress

      case encrypted txoptions of
        False -> do
          addresses <-
            case receiversFile txoptions of
              Nothing -> return Nothing
              Just f ->
                AE.eitherDecodeFileStrict f >>= \case
                  Left err -> die $ "Cannot read receivers file: " ++ err
                  Right [] -> return Nothing
                  Right addrs -> return (Just addrs)

          accInfo <- withClient backend (getAccountInfoOrDie selfAddress)
          let txRecepient (Nonce n) =
                case addresses of
                  Just addrs -> addrs !! (fromIntegral n `mod` length addrs)
                  Nothing -> selfAddress
          let txBody n = encodePayload (Transfer (txRecepient n) 1)
          let txHeader nonce = TransactionHeader {
                thSender = selfAddress,
                thNonce = nonce,
                thEnergyAmount = 1000,
                thPayloadSize = payloadSize (txBody nonce),
                thExpiry = TransactionTime maxBound
                }
          let sign nonce () = return (txRecepient nonce, NormalTransaction $ signTransaction (Map.toList keyMap) (txHeader nonce) (txBody nonce), ())
          go backend (logit txoptions) (tps txoptions) () sign (airNonce accInfo)
        True -> do
          globalParameters <- withClient backend (getBestBlockHash >>= getParseCryptographicParameters) >>= \case
              Left err -> die err
              Right gp -> return gp

          addresses <-
            case receiversFile txoptions of
              Nothing -> die "Receivers must be present when encrypted transfers are selected."
              Just f ->
                AE.eitherDecodeFileStrict f >>= \case
                  Left err -> die $ "Cannot read receivers file: " ++ err
                  Right [] -> die "Receivers must be non-empty when encrypted transfers are selected."
                  Right addrs -> case filter (/= selfAddress) addrs of
                    [] -> die "There must be at least one other receiver."
                    xs -> withClient backend $ forM xs $ \addr -> do
                      accInfo <- getAccountInfoOrDie addr
                      return (addr, airEncryptionKey accInfo)

          accInfo <- withClient backend $ getAccountInfoOrDie selfAddress

          let encAmount = airEncryptedAmount accInfo
          let ownAmount = _selfAmount encAmount
          let table = computeTable globalParameters (2^(16 :: Int))
          let plain = decryptAmount table encryptionSecretKey ownAmount
          let txRecepient (Nonce n) = addresses !! (fromIntegral n `mod` length addresses)
          let txBody n (encryptedAmount, decryptedAmount) = do
                let (address, AccountEncryptionKey encryptionKey) = txRecepient n
                    aggAmount = makeAggregatedDecryptedAmount encryptedAmount decryptedAmount 0
                Just eatd <- makeEncryptedAmountTransferData globalParameters encryptionKey encryptionSecretKey aggAmount 0
                return (encodePayload (EncryptedAmountTransfer address eatd), (eatdRemainingAmount eatd, decryptedAmount))
          let txHeader nonce body = TransactionHeader {
                thSender = selfAddress,
                thNonce = nonce,
                thEnergyAmount = 11 + encryptedTransferEnergyCost (Map.size keyMap),
                thPayloadSize = payloadSize body,
                thExpiry = TransactionTime maxBound
                }
          let sign nonce carry = do
                (body, newSelfAmount) <- txBody nonce carry
                return (fst (txRecepient nonce), NormalTransaction $ signTransaction (Map.toList keyMap) (txHeader nonce body) body, newSelfAmount)
          go backend (logit txoptions) (tps txoptions) (ownAmount, plain) sign (airNonce accInfo)

  where accountKeysParser = AE.withObject "Account keys" $ \v -> do
          accountAddr <- v AE..: "address"
          accountData <- v AE..: "accountData"
          secretKey <- v AE..: "encryptionSecretKey"
          keyMap <- accountData AE..: "keys"
          return (accountAddr, keyMap, secretKey)
