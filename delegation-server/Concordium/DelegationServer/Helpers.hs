{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Concordium.DelegationServer.Helpers where

import Concordium.Client.Cli
import Concordium.Client.Config
import Concordium.Client.GRPC (getBirkParameters, getBlockInfo, getTransactionStatus, runClient, withBestBlockHash)
import Concordium.Client.Runner
import qualified Concordium.Client.Types.Transaction as Costs
import Concordium.Client.Types.TransactionStatus
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Execution
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar, readTMVar, takeTMVar)
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson
import Data.HashMap.Strict as HM hiding (filter, mapMaybe)
import Data.Maybe
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as Vec

-- Helpers

type DelegationAccounts = Vec.Vector AccountConfig

jsonAnswer ::
  (Show a, FromJSON t) =>
  EnvData ->
  ClientMonad IO (Either a Value) ->
  (t -> IO b) ->
  IO b
jsonAnswer grpc f g = do
  eeValue <- runClient grpc f
  case eeValue of
    Left e -> throwError . userError . show $ e
    Right (Left e) -> throwError . userError . show $ e
    Right (Right jValue) -> case fromJSON jValue of
      Error e -> throwError . userError $ "Unable to parse JSON value received from the grpc backend. \nValue: " ++ show jValue ++ "\nError: " ++ show e
      Success value -> g value

-- | When checking for a transaction it might be in different states:
--
-- - TxAbsent: The transaction was dropped either because of insufficient funds
--   or because an issue with the nonce. We have to retry this transaction.
-- - TxAccepted: The transaction was finalized and accepted, continue.
-- - TxRejected: The transaction was finalized and rejected. Some serious
--   problem might be happening here (serialization issues or sth similar).
-- - TxPending: The transaction is not yet finalized, keep watching it.
-- - TxError: Serious error in the chain as the transaction is finalized but in
--   no blocks or in more than one block.
data TxState = TxAbsent | TxPending | TxAccepted | TxRejected | TxError

queryTransactionState :: EnvData -> Types.TransactionHash -> IO TxState
queryTransactionState grpc th = do
  let f = getTransactionStatus (Text.pack $ show th)
      g :: TransactionStatusResult -> IO TxState
      g status = return $ case tsrState status of
        Absent -> TxAbsent
        Finalized -> case mapMaybe (\(k, v) -> (k,) <$> v) $ HM.toList (tsrResults status) of
          [(_, Execution.TransactionSummary {..})] ->
            case tsResult of
              Execution.TxSuccess _ -> TxAccepted
              _ -> TxRejected
          _ -> TxError
        _ -> TxPending
  jsonAnswer grpc f g

-- | This function will only be executed if the output from
-- `queryTransactionState` is @TxAccepted@.
compute2epochsSinceFinalization :: UTCTime -> NominalDiffTime -> EnvData -> Types.TransactionHash -> IO UTCTime
compute2epochsSinceFinalization genesisTime epochDuration grpc th = do
  -- get block in which the transaction is finalized
  let f = getTransactionStatus (Text.pack $ show th)
      g :: TransactionStatusResult -> IO Types.BlockHash
      g status = case mapMaybe (\(k, v) -> (k,) <$> v) $ HM.toList (tsrResults status) of
        [(hash, _)] -> return hash
        _ -> error "Should not happen, the tx was finalized!"
      -- compute the time based on the slot time of the block
      f' hash = getBlockInfo (Text.pack $ show hash)
      g' :: BlockInfoResult -> IO UTCTime
      g' blockInfo = return $ in2epochs genesisTime epochDuration $ birBlockSlotTime blockInfo
  hash <- jsonAnswer grpc f g
  jsonAnswer grpc (f' hash) g'

-- | Depending on whether the bakerId is present, will delegate or undelegate to it.
delegate :: EnvData -> DelegationAccounts -> Maybe Types.BakerId -> Int -> IO Types.TransactionHash
delegate grpc delegationAccounts bakerId idx =
  case bakerId of
    Just bid -> do
      let f = withBestBlockHash Nothing getBirkParameters
          g :: BirkParametersResult -> IO Bool
          g birkParams =
            let mBakerInfo = filter ((== bid) . bpbrId) $ bprBakers birkParams
             in case mBakerInfo of
                  [_] -> return True
                  _ -> return False
      bakerIdExists <- jsonAnswer grpc f g
      if bakerIdExists
        then doDelegation
        else error "Baker Id non existent!!"
    Nothing -> doDelegation
  where
    doDelegation = do
      -- get account instances for computing the cost of the delegating trnasaction
      let f' = getBestBlockHash >>= getAccountInfo (Text.pack . show . naAddr . acAddr $ delegationAccounts Vec.! idx)
          g' :: AccountInfoResult -> IO Types.Energy
          g' info = return $ Costs.accountDelegateEnergyCost (length . airInstances $ info) (HM.size $ acKeys $ delegationAccounts Vec.! idx)
      energy <- jsonAnswer grpc f' g'
      expiry <- (600 +) <$> getCurrentTimeUnix
      let txCfg =
            TransactionConfig
              { tcAccountCfg = delegationAccounts Vec.! idx,
                tcNonce = Nothing,
                tcEnergy = energy,
                tcExpiry = expiry
              }
          pl = case bakerId of
            Just bid -> Execution.DelegateStake {dsID = bid}
            Nothing -> Execution.UndelegateStake
      -- send the transaction
      either (throwError . userError . show) return =<< runClient grpc (getBlockItemHash <$> startTransaction txCfg pl)

in2epochs :: UTCTime -> NominalDiffTime -> UTCTime -> UTCTime
in2epochs genesisTime epochDuration finalizationTime =
  -- genesis + (((finalization - genesis)/epoch + 2) * epoch)
  addUTCTime (((diffUTCTime finalizationTime genesisTime / epochDuration) + 2) * epochDuration) genesisTime

microsecondsBetween :: UTCTime -> UTCTime -> Int
microsecondsBetween a b =
  truncate (10 ^ (6 :: Int) * diffUTCTime a b)

-- FIXME: Consider using MonadLogger $logTrace here instead of putStrLn.
logAndTakeTMVar :: String -> TMVar a -> IO a
logAndTakeTMVar context v = do
  putStrLn $ "[" ++ context ++ "]: taking local state tmvar"
  val <- atomically $ takeTMVar v
  putStrLn $ "[" ++ context ++ "]: took local state tmvar"
  return val

logAndReadTMVar :: String -> TMVar a -> IO a
logAndReadTMVar context v = do
  putStrLn $ "[" ++ context ++ "]: going to read local state tmvar"
  val <- atomically $ readTMVar v
  putStrLn $ "[" ++ context ++ "]: read local state tmvar"
  return val

logAndPutTMVar :: String -> TMVar a -> a -> IO ()
logAndPutTMVar context v val = do
  putStrLn $ "[" ++ context ++ "]: putting local state tmvar"
  atomically $ putTMVar v val
  putStrLn $ "[" ++ context ++ "]: put local state tmvar"

logAndDo :: String -> IO a -> IO a
logAndDo s f = do
  putStrLn s
  f
