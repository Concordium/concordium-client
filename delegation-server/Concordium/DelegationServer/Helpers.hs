{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Concordium.DelegationServer.Helpers where

import Concordium.Client.Cli
import Concordium.Client.Types.Account
import Concordium.Client.GRPC (getBirkParameters, getBlockInfo, getTransactionStatus, runClient, withBestBlockHash)
import Concordium.Client.Runner
import qualified Concordium.Client.Types.Transaction as Costs
import Concordium.Client.Types.TransactionStatus
import Concordium.Client.Utils
import qualified Concordium.ID.Types as ID
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Execution
import Control.Concurrent.MVar
import Control.Monad.Except (MonadError (throwError))
import Data.Aeson
import Data.HashMap.Strict as HM hiding (filter, mapMaybe)
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as Vec
import System.Log.FastLogger
import Concordium.Logger
import Control.Monad.IO.Class

-- Helpers

type Logger = LogLevel -> FastLogger

data DelegationAccount =
  DelegationAccount
  { daAddr :: !Types.AccountAddress
  , daKeys :: !AccountKeyMap
  , daThreshold :: !ID.SignatureThreshold
  }

type DelegationAccounts = Vec.Vector DelegationAccount

-- |Parse a response, raising an error in case of any failure.
-- Failure can occur in three ways.
--
-- - failure establishing connection to the GRPC server when running the supplied action
-- - failure of the GRPC call
-- - failure to parse the response with a provided JSON parser.
jsonAnswer ::
  (Show a, FromJSON t) =>
  EnvData ->
  ClientMonad IO (Either a Value) ->
  (t -> IO b) ->
  IO b
jsonAnswer grpc f g = do
  eeValue <- runClient grpc f
  case eeValue of
    Left e -> throwError . userError $ "GRPC connection error: " ++ show e
    Right (Left e) -> throwError . userError $ "GRPC call failed: " ++ show e
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
        Finalized -> case HM.toList (tsrResults status) of
          [(_, Execution.TransactionSummary {..})] ->
            case tsResult of
              Execution.TxSuccess _ -> TxAccepted
              _ -> TxRejected
          _ -> TxError
        _ -> TxPending
  jsonAnswer grpc f g

-- | This function will only be executed if the output from
-- `queryTransactionState` is @TxAccepted@.
compute2epochsSinceFinalization :: UTCTime -> NominalDiffTime -> EnvData -> MVar Types.TransactionHash -> IO UTCTime
compute2epochsSinceFinalization genesisTime epochDuration grpc thMVar = do
  th <- readMVar thMVar
  -- get block in which the transaction is finalized
  let f = getTransactionStatus (Text.pack $ show th)
      g :: TransactionStatusResult -> IO Types.BlockHash
      g status = case HM.toList (tsrResults status) of
        [(hash, _)] -> return hash
        _ -> error "Should not happen, the tx was finalized!"
      -- compute the time based on the slot time of the block
      f' hash = getBlockInfo (Text.pack $ show hash)
      g' :: BlockInfoResult -> IO UTCTime
      g' blockInfo = return $ in2epochs genesisTime epochDuration $ birBlockSlotTime blockInfo
  hash <- jsonAnswer grpc f g
  jsonAnswer grpc (f' hash) g'

-- | Checks if the given baker exists in the best block
checkBakerExists :: EnvData -> Types.BakerId -> IO Bool
checkBakerExists grpc bakerId = do
  let f = withBestBlockHash Nothing getBirkParameters
      g :: BirkParametersResult -> IO Bool
      g birkParams =
        let mBakerInfo = filter ((== bakerId) . bpbrId) $ bprBakers birkParams
          in case mBakerInfo of
              [_] -> return True
              _ -> return False
  jsonAnswer grpc f g

-- | Depending on whether the bakerId is present, will delegate or undelegate to it.
delegate :: Logger -> EnvData -> DelegationAccounts -> Maybe Types.BakerId -> Int -> IO Types.TransactionHash
delegate logger grpc delegationAccounts bakerId idx = do
      -- get account instances for computing the cost of the delegating transaction
      let f' = getBestBlockHash >>= getAccountInfo (Text.pack . show . daAddr $ delegationAccounts Vec.! idx)
          g' :: AccountInfoResult -> IO Types.Energy
          g' info = return $ Costs.accountDelegateEnergyCost (length . airInstances $ info) (HM.size $ daKeys $ delegationAccounts Vec.! idx)
      energy <- jsonAnswer grpc f' g'
      expiry <- (600 +) <$> getCurrentTimeUnix
      let DelegationAccount{..} = delegationAccounts Vec.! idx
      let
          pl = case bakerId of
            Just bid -> Execution.DelegateStake {dsID = bid}
            Nothing -> Execution.UndelegateStake
      -- action to send the transaction
      let sender = do
            currentNonce <- getBestBlockHash >>= getAccountNonce daAddr
            let tx = encodeAndSignTransaction pl daAddr energy currentNonce expiry daKeys daThreshold
            sendTransactionToBaker tx defaultNetId >>= \case
              Left err -> fail err
              Right False -> fail "Transaction not accepted by the baker."
              Right True -> do
                liftIO (logger LLInfo $ "Transaction with hash " <> toLogStr (show (getBlockItemHash tx)) <> ", delegating to baker " <> toLogStr (show bakerId) <> " accepted by the baker.")
                return (getBlockItemHash tx)
      -- try to send, and in case of failure throw an exception.
      runClient grpc sender `failWith` (userError . show)

in2epochs :: UTCTime -> NominalDiffTime -> UTCTime -> UTCTime
in2epochs genesisTime epochDuration finalizationTime =
  -- genesis + (((finalization - genesis)/epoch + 2) * epoch)
  addUTCTime (((fromInteger . truncate $ diffUTCTime finalizationTime genesisTime / epochDuration) + 2) * epochDuration) genesisTime

microsecondsBetween :: UTCTime -> UTCTime -> Int
microsecondsBetween a b =
  truncate (10 ^ (6 :: Int) * diffUTCTime a b)

logAndTakeMVar :: Logger -> LogStr -> MVar a -> IO a
logAndTakeMVar logger context v = do
  logger LLTrace $ "[" <> context <> "]: taking local state tmvar"
  val <- takeMVar v
  logger LLTrace $ "[" <> context <> "]: took local state tmvar"
  return val

logAndReadMVar :: Logger -> LogStr -> MVar a -> IO a
logAndReadMVar logger context v = do
  logger LLTrace $ "["  <> context <> "]: going to read local state tmvar"
  val <- readMVar v
  logger LLTrace $ "[" <> context <> "]: read local state tmvar"
  return val

logAndPutMVar :: Logger -> LogStr -> MVar a -> a -> IO ()
logAndPutMVar logger context v val = do
  logger LLTrace $ "[" <> context <> "]: putting local state tmvar"
  putMVar v val
  logger LLTrace $ "[" <> context <> "]: put local state tmvar"