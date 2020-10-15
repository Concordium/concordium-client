{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- The server logic that will cleanup the expired delegations issuing new ones
-- when possible.
--
-- Considering the different states a delegation can be, we can identify the
-- following flow:
--
-- @@
--
--  RequestDelegation                            Server interactions
--       |
-- +-----+-----+-----+-----+-----+-----+-----+
-- | ()  | p   | dtx | b   | utx | s   | r   |
-- +-----+-----+-----+-----+-----+-----+-----+
-- |  1  |  2  |  3  |  4  |  5  |  6  |  7  |   States
-- +-----+-----+-----+-----+-----+-----+-----+
--             |     | 2ep |     | 2ep |         Duration
-- ------------|-----------<--baking-->-------   Node
--             |     *     |     *
--       DelegateStake     UndelegateStake       On chain
-- @@
--
-- Where the state numbers correspond to:
--
-- 1. /Initial state/: Nothing requested yet or a previous delegation was
--    already finished. Querying the server returns an empty response saying
--    that no delegations have been done for this baker ID.
--
-- prop> To transition from this state, a request is issued and the user gets an
--    answer saying the expected waiting time.
--
-- 2. /Pending delegation/: The delegation is pending and the user can query the
--    server to get an update on the __estimated waiting time__ computed on
--    call.
--
-- prop> To transition from this state, the @DelegateStake@ transaction is
--    sent. The user knows this has happened by querying the server and getting
--    a state-3 answer.
--
-- 3. /Waiting finalization of delegating transaction/: Querying the server will
--    return the transaction hash of the delegate transaction.
--
-- prop> To transition from this state, the transaction gets finalized in the
--    chain. Once the transaction is finalized we can compute the current time +
--    2 epoch duration to give the user a certain timestamp when the delegation
--    will be active. The user knows this has happened by querying the server
--    and getting a state-4 answer.
--
-- 4. /Waiting for 2 epochs/: The user can query the server to get the timestamp
--    at which the 2 epochs will have passed and the delegation will be
--    effective (calculated when transitioning from 3 to 4).
--
-- prop> To transition from this, we need to wait 2 epochs. The user knows this
--    has happened by querying the server and getting a state-5 answer. As we
--    transition, we send an undelegate transaction.
--
-- 5. /Undelegate transaction sent/: The user can bake. Queries for the status
--    will return the undelegate transaction hash.
--
-- prop> To transition from this state, the transaction gets finalized in the
--    chain. Once the transaction is finalized we can compute the current time +
--    2 epoch duration to give the user a certain timestamp when the delgation
--    will be effectively revoked. The user knows this has happened by querying
--    the server and getting a state-6 answer.
--
-- 6. /Waiting for 2 epochs/: The user is baking. The user can query the server
--    to get the timestamp at which the 2 epochs will have passed and the
--    undelegation will be effective (calculated when transitioning from 5 to
--    6).
--
-- prop> To transition from this, we need to wait 2 epochs. The user knows this
--    has happened by querying the server and getting a state-7 answer.
--
-- 7. /Undelegate becomes effective/: The user can no longer use the delegated
--    stake for baking. This is equivalent to the 1st state. The user can query
--    for information about when was the undelegation effective.
module Concordium.DelegationServer.Logic where

import Concordium.Client.GRPC
import Concordium.DelegationServer.Helpers
import Concordium.Types
import Control.Concurrent
import Control.Exception (displayException, onException, ErrorCall, IOException, Handler(..), catches)
import Control.Monad.Except
import qualified Data.HashMap.Strict as HM
import qualified Data.HashPSQ as PSQ
import qualified Data.Set as Set
import Data.Time.Clock
import qualified Data.Vector as Vec
import System.Log.FastLogger
import Data.String
import Concordium.Logger

-- | Only will be 0-numDelegators. Represents the delegators.
type Delegator = Int


-- | State shared between requests.
data LocalState
  = LocalState
      { -- State 2

        -- | Priority queue with the pending requests.
        pendingDelegations :: !(PSQ.HashPSQ BakerId Int ()),
        -- State 3

        -- | Baker IDs that have been issued a delegating transaction and are
        -- waiting for it to finalize. INVARIANT: size < 5.
        awaitingDelegation :: !(HM.HashMap BakerId (Delegator, MVar TransactionHash)),
        -- State 4

        -- | Baker IDs that are in the 2 epoch time slice for
        -- delegation. INVARIANT: size < 5.
        waitingDelegationEpochs :: !(HM.HashMap BakerId (Delegator, MVar TransactionHash, UTCTime)),
        -- State 5

        -- | Baker IDs that have been issued an undelegating transaction and are
        -- waiting for it to finalize. INVARIANT: size < 5.
        awaitingUndelegation :: !(HM.HashMap BakerId (Delegator, MVar TransactionHash)),
        -- State 6

        -- | Baker IDs that are in the 2 epoch time slice for
        -- undelegation. INVARIANT: size < 5.
        waitingUndelegationEpochs :: !(HM.HashMap BakerId (Delegator, MVar TransactionHash, UTCTime)),
        -- State 7

        -- | Set of BakerIDs that have been recently delegated to with the
        -- timestamp at when it was undelegated adn the amount of times this
        -- baker was delegated to.
        recentDelegations :: !(HM.HashMap BakerId (Int, UTCTime)),
        -- Support structures

        -- | The baker IDs that are active right now. A delegation is said to
        -- be active if it is in states 3 or 4. INVARIANT: size < 5.
        activeDelegations :: !(Set.Set BakerId),
        -- | Vector with the current active delegations. A delegation is said to
        -- be active if it is in states 3 or 4. INVARIANT: size = 4.
        currentDelegations :: !(Vec.Vector (Maybe BakerId))
      }

-- the indentiation for the local prints. It will give us more readability.
lpi :: IsString a => a
lpi = fromString (replicate 20 ' ')

logHash :: TransactionHash -> LogStr
logHash = toLogStr . show

localStateDescription :: String -> LocalState -> LogStr
localStateDescription description LocalState {..} = toLogStr $
  unlines
    [ "",
      lpi ++ "==============================================",
      lpi ++ "Local State (" ++ description ++ ")",
      lpi ++ "----------------------------------------------",
      lpi ++ "Pending delegations: " ++ show pd,
      lpi ++ "Waiting for delegation transaction finalization: " ++ show ad,
      lpi ++ "Waiting 2 epochs since delegation transaction: " ++ show wd,
      lpi ++ "Waiting for undelegation transaction finalization: " ++ show au,
      lpi ++ "Waiting 2 epochs since undelegation transaction finalization: " ++ show wu,
      lpi ++ "Active delegations now: " ++ show currentDelegations,
      lpi ++ "=============================================="
    ]
  where
    pd = PSQ.keys pendingDelegations
    ad = HM.keys awaitingDelegation
    wd = HM.keys waitingDelegationEpochs
    au = HM.keys awaitingUndelegation
    wu = HM.keys waitingUndelegationEpochs

defaultLocalState :: Int -> LocalState
defaultLocalState numDelegators =
  LocalState
    { pendingDelegations = PSQ.empty,
      awaitingDelegation = HM.empty,
      waitingDelegationEpochs = HM.empty,
      awaitingUndelegation = HM.empty,
      waitingUndelegationEpochs = HM.empty,
      recentDelegations = HM.empty,
      activeDelegations = Set.empty,
      currentDelegations = Vec.replicate numDelegators Nothing
    }

data State
  = State
      { numDelegators :: Int,
        localState :: !(MVar LocalState),
        outerLocalState :: !(MVar LocalState),
        backend :: EnvData,
        configDir :: DelegationAccounts,
        epochDuration :: NominalDiffTime,
        genesisTime :: UTCTime,
        serviceLogger :: Logger
      }

-- | Creates the vector with the transitions that are executed in the state machine.
--
-- The different states are triggered by:
-- - 2 to 3 -> some position becomes available by a delegation that transitioned 4 to 5
-- - 3 to 4 -> finalization of the transaction
-- - 4 to 5 -> 2 epochs passed
-- - 5 to 6 -> finalization of the transction
-- - 6 to 7 -> 2 epochs passed
mkTransitions :: Logger -> Chan (BakerId, Int) -> Chan (Maybe TransactionHash) -> UTCTime -> NominalDiffTime -> EnvData -> DelegationAccounts -> Vec.Vector (BakerId -> (MVar LocalState, MVar LocalState) -> IO ())
mkTransitions logger transitionChan loopbackChan genesisTime epochDuration backend cfgDir =
  Vec.fromList
    [ transition2to3 logger transitionChan loopbackChan backend cfgDir False,
      transition3to4 logger transitionChan genesisTime epochDuration backend,
      transition4to5 logger transitionChan loopbackChan backend cfgDir,
      transition5to6 logger transitionChan genesisTime epochDuration backend,
      transition6to7 logger transitionChan
    ]

-- | Main entry point. It creates the state machine and the channels that will communicate the threads.
forkStateMachine :: State -> Chan (BakerId, Int) -> IO ()
forkStateMachine State {..} transitionChan = do
  loopbackChan <- newChan
  let transitions = mkTransitions serviceLogger transitionChan loopbackChan genesisTime epochDuration backend configDir
  _ <- forkIO $ stateMachine serviceLogger (localState, outerLocalState) transitions transitionChan
  pure ()

type Rollback = (BakerId, Delegator, (MVar LocalState, MVar LocalState))

-- | Runs the specified transition when the given transaction shows up as finalized
finalizationListener :: Logger -> EnvData -> Chan (BakerId, Int) -> TransactionHash -> (BakerId, Int) -> Either Rollback (IO TransactionHash) -> IO (MVar TransactionHash)
finalizationListener logger backend chan txHash next fallback = do
  txHashMVar <- newMVar txHash
  let loop th = do
        logger LLDebug $ "[finalizationListener]: Checking transaction status of " <> toLogStr (show txHash)
        txState <- queryTransactionState backend txHash
        case txState of
          TxAccepted -> do
            logger LLInfo $ "[finalizationListener]: The transaction " <> toLogStr (show txHash) <> " is finalized and accepted."
            writeChan chan next
          TxPending -> threadDelay 5000000 >> (loop th) -- 5 seconds
          _ -> do
            case txState of
              TxError ->
                logger LLWarning $ "[finalizationListener]: The transaction status returned TxError. Please check the transaction hash: " <> logHash txHash
              TxRejected -> logger LLWarning $ "[finalizationListener]: The transaction status shows the transaction was rejected. Please check the transaction hash: " <> logHash txHash
              TxAbsent -> logger LLWarning $ "[finalizationListener]: The transaction status shows the transaction is absent. Please check the transaction hash: " <> logHash txHash
              _ -> undefined -- impossible case
                -- as the state has already changed, it would be
                -- complicated to revert it keeping everything in sync, for now
                -- this same loop will try to resend the transaction with refreshed
                -- nonce and all.
            case fallback of
              Right newTransaction -> do
                th' <- newTransaction
                _ <- swapMVar txHashMVar th'
                logger LLInfo $ "[finalizationListener]: Sending a new transaction " <> logHash th <> " superseding " <> logHash txHash
                threadDelay 5000000
                -- watch the new transaction
                loop th'
              Left (baker, delegator, (localState, outerLocalState)) -> do
                -- we rollback the state update to the local state that was made pre-emptively
                currentState@LocalState{..} <- logAndTakeMVar logger "Finalization listener" localState
                -- We were trying to delegate, but the transaction failed. The most likely cause of this is the user
                -- removed themselves from the committee.
                let activeDelegations' = Set.delete baker activeDelegations
                    currentDelegations' = currentDelegations Vec.// [(delegator, Nothing)]
                    awaitingDelegation' = HM.delete baker awaitingDelegation
                let currentState' = currentState {
                      activeDelegations = activeDelegations',
                      currentDelegations = currentDelegations',
                      awaitingDelegation = awaitingDelegation'
                    }
                logAndPutMVar logger "Finalization listener" localState currentState'
                logger LLTrace $ localStateDescription "Rolled-back state" currentState'
                () <$ swapMVar outerLocalState currentState'
                -- inform the state machine that it can continue processing request, since a delegation spot is free now.
                writeChan chan (10, 0) -- the baker id is not used, so using dummy 10 as in many other places.
                
  _ <- forkIO (loop txHash)
  pure txHashMVar

-- | Runs the specified transition when the timer has expired
runTransitionIn :: Logger -> Bool -> Chan (BakerId, Int) -> Int -> (BakerId, Int) -> IO ()
runTransitionIn logger waitingEpochs chan timer next = do
  _ <- forkIO $ do
    if waitingEpochs
      then logger LLTrace $ "[runTransitionIn]: setting a timer for " <> toLogStr (show (timer `div` 1000000)) <> " seconds for a two epoch waiting time."
      else logger LLTrace $ "[runTransitionIn]: setting a timer for " <> toLogStr ((timer `div` 1000000)) <> " seconds because the delegation didn't expire yet (this is likely not going to happen)."
    threadDelay timer
    writeChan chan next
  pure ()

-- | Main loop of the state machine. Executes the transitions given through the chanToLoop channel.
stateMachine :: Logger -> (MVar LocalState, MVar LocalState) -> Vec.Vector (BakerId -> (MVar LocalState, MVar LocalState) -> IO ()) -> Chan (BakerId, Int) -> IO ()
stateMachine logger localState transitions chanToLoop = loop
  where loop = do
          (bid, trans) <- readChan chanToLoop
          if trans == 0
            then logger LLDebug $ "[stateMachine]: going to promote a request from the queue (transition 0)."
            else logger LLDebug $ "[stateMachine]: going to move the baker " <> toLogStr (show bid) <> " in transition " <> transitionNames trans
          (transitions Vec.! trans) bid localState `catches`
            [Handler (\(ex :: ErrorCall) -> logger LLError (toLogStr (displayException ex)))
            ,Handler (\(ex :: IOException) -> logger LLError (toLogStr (displayException ex)) )
            ]
          loop

transitionNames :: Int -> LogStr
transitionNames 1 = "(1) delegate transaction was finalized, setting 2 epoch timer."
transitionNames 2 = "(2) 2 epochs passed after finalized delegate transaction, sending undelegate/redelegate transaction."
transitionNames 3 = "(3) undelegate transaction was finalized, setting 2 epoch timer."
transitionNames 4 = "(4) 2 epochs passed after finalized undelegate transaction, moving to recent delegations"
transitionNames _ = undefined -- must not happen

transition6to7 :: Logger -> Chan (BakerId, Int) -> BakerId -> (MVar LocalState, MVar LocalState) -> IO ()
transition6to7 logger transitionChan baker (localState, outerLocalState) = do
  currentState@LocalState {..} <- logAndTakeMVar logger "transition 4" localState
  (flip onException)
    ( do
        logger LLError "IO exception raised in transition 4"
        -- put back the old state
        logAndPutMVar logger "transition 4 (exception)" localState currentState
        -- trigger this same transition at some point
        writeChan transitionChan (baker, 4)
    )
    $ do
      logger LLTrace $ localStateDescription "entering transition 4" currentState
      currentState' <- do
        now <- getCurrentTime
        -- Get expired item
        let mItem = HM.lookup baker waitingUndelegationEpochs
        case mItem of
          Just (_, _, expiry) ->
            if expiry < now
              then do
                -- remove item
                let waitingUndelegationEpochs' = HM.delete baker waitingUndelegationEpochs
                    recentDelegations' =
                      HM.alter
                        ( \case
                            Just (cnt, _) -> Just (cnt + 1, expiry)
                            Nothing -> Just (1, expiry)
                        )
                        baker
                        recentDelegations
                return $
                  currentState
                    { waitingUndelegationEpochs = waitingUndelegationEpochs',
                      recentDelegations = recentDelegations'
                    }
              else do
                -- if not expired yet, retry this in some time (this should not happen)
                runTransitionIn logger False transitionChan 5000000 (baker, 4)
                return currentState
          Nothing -> do
            logger LLError $ "Invariant violation, triggering transition 4 for baker " <> toLogStr (show baker) <> " but it doesn't seem to be in the correct state:"
            logger LLError $ localStateDescription "BAD STATE" currentState
            error "Delegation should exist!!"
      logger LLTrace $ localStateDescription "leaving transition 4" currentState'
      logAndPutMVar logger "transition 4" localState currentState'
      _ <- swapMVar outerLocalState currentState'
      pure ()

transition5to6 :: Logger -> Chan (BakerId, Int) -> UTCTime -> NominalDiffTime -> EnvData -> BakerId -> (MVar LocalState, MVar LocalState) -> IO ()
transition5to6 logger transitionChan genesisTime epochDuration backend baker (localState, outerLocalState) = do
  currentState@LocalState {..} <- logAndTakeMVar logger "transition 3" localState
  (flip onException)
    ( do
        logger LLError "IO exception raised in transition 3"
        -- put back the old state
        logAndPutMVar logger "transition 3 (exception)" localState currentState
        -- trigger this same transition at some point
        writeChan transitionChan (baker, 3)
    )
    $ do
      logger LLTrace $ localStateDescription "entering transition 3" currentState
      currentState' <- do
        -- Get finalized item
        let mItem = HM.lookup baker awaitingUndelegation
        case mItem of
          Just (delegator, txHash) -> do
            -- generate the new hashmap without the finalized item
            let awaitingUndelegation' = HM.delete baker awaitingUndelegation
            -- compute the 2 epoch time since finalization
            -- and store it in waitingUndelegationEpochs
            ex <- compute2epochsSinceFinalization genesisTime epochDuration backend txHash
            let waitingUndelegationEpochs' = HM.insert baker (delegator, txHash, ex) waitingUndelegationEpochs
            -- set a timer for next step
            waitingTime <- microsecondsBetween ex <$> getCurrentTime
            runTransitionIn logger True transitionChan waitingTime (baker, 4)
            return $
              currentState
                { waitingUndelegationEpochs = waitingUndelegationEpochs',
                  awaitingUndelegation = awaitingUndelegation'
                }
          Nothing -> do
            logger LLError $ "Invariant broken, triggering transition 3 for baker " <> toLogStr (show baker) <> " but it doesn't seem to be in the correct state:"
            logger LLError $ localStateDescription "BAD STATE" currentState
            error "Delegation should exist!!"
      logger LLTrace $ localStateDescription "leaving transition 3" currentState'
      logAndPutMVar logger "transition 3" localState currentState'
      _ <- swapMVar outerLocalState currentState'
      pure ()

transition4to5 :: Logger -> Chan (BakerId, Int) -> Chan (Maybe TransactionHash) -> EnvData -> DelegationAccounts -> BakerId -> (MVar LocalState, MVar LocalState) -> IO ()
transition4to5 logger transitionChan loopbackChan backend cfgDir baker s@(localState, outerLocalState) = do
  currentState <- logAndTakeMVar logger "transition 2" localState
  (flip onException)
    ( do
        logger LLError "IO exception raised in transition 2"
        -- put back the old state
        logAndPutMVar logger "transition 2 (exception)" localState currentState
        -- trigger this same transition at some point
        writeChan transitionChan (baker, 2)
    )
    $ do
      logger LLTrace $ localStateDescription "entering transition 2" currentState
      currentState' <- do
        now <- getCurrentTime
        -- Get expired item
        let mItem = HM.lookup baker (waitingDelegationEpochs currentState)
        case mItem of
          Just (delegator, _, expiry) ->
            if expiry < now
              then do
                -- remove the item
                let waitingDelegationEpochs' = HM.delete baker (waitingDelegationEpochs currentState)
                    -- a new slot is available
                    activeDelegations' = Set.delete baker (activeDelegations currentState)
                    currentDelegations' = currentDelegations currentState Vec.// [(delegator, Nothing)]
                    currentState' =
                      currentState
                        { waitingDelegationEpochs = waitingDelegationEpochs',
                          activeDelegations = activeDelegations',
                          currentDelegations = currentDelegations'
                        }
                logAndPutMVar logger "interleaving transition 2" localState currentState'
                -- signal that a new position is available
                transition2to3 logger transitionChan loopbackChan backend cfgDir True 10 s -- the baker is not needed so we will put 10 as a dummy value
                  -- if the position was re-delegated, use that transaction hash,
                  -- else undelegate
                mRedelegateTx <- readChan loopbackChan
                currentState'' <- logAndTakeMVar logger "interleaving transition 2" localState
                let sendThisTransaction = delegate logger backend cfgDir Nothing delegator
                th <- maybe sendThisTransaction return mRedelegateTx
                -- create listeners for finalization
                -- in case mRedelegateTx was Just, then the transition2to3 already set up a listener for the transaction it sent.
                -- If that transaction failed then the state will be rolled-back correctly for that transaction.
                -- Here we register a new listener which will, if the transaction fails, not touch the state, but
                -- instead keep trying to resend the undelegate transaction. This can only fail if our account keys are wrong,
                -- which will cause many other problems, so retrying in this case is acceptable.
                thMVar <- finalizationListener logger backend transitionChan th (baker, 3) (Right sendThisTransaction) -- undelegate transaction should not fail, so retry
                -- move to next collection
                let awaitingUndelegation' = HM.insert baker (delegator, thMVar) (awaitingUndelegation currentState'')
                return $
                  currentState''
                    { awaitingUndelegation = awaitingUndelegation'
                    }
              else do
                -- if not expired yet, retry this in some time (this should not happen)
                runTransitionIn logger False transitionChan 5000000 (baker, 2)
                return currentState
          Nothing -> do
            logger LLError $ "Invariant broken, triggering transition 2 for baker " <> toLogStr (show baker) <> " but it doesn't seem to be in the correct state:"
            logger LLError $ localStateDescription "BAD STATE" currentState
            error "Delegation should exist!!"
      logger LLTrace $ localStateDescription "leaving transition 2" currentState'
      logAndPutMVar logger "transition 2" localState currentState'
      _ <- swapMVar outerLocalState currentState'
      pure ()

transition3to4 :: Logger -> Chan (BakerId, Int) -> UTCTime -> NominalDiffTime -> EnvData -> BakerId -> (MVar LocalState, MVar LocalState) -> IO ()
transition3to4 logger transitionChan genesisTime epochDuration backend baker (localState, outerLocalState) = do
  currentState@LocalState {..} <- logAndTakeMVar logger "transition 1" localState
  (flip onException)
    ( do
        logger LLError "IO exception raised in transition 1"
        -- put back the old state
        logAndPutMVar logger "transition 1 (exception)" localState currentState
        -- trigger this same transition at some point
        writeChan transitionChan (baker, 1)
    )
    $ do
      logger LLTrace $ localStateDescription "entering transition 1" currentState
      currentState' <- do
        -- Get finalized item
        let mItem = HM.lookup baker awaitingDelegation
        case mItem of
          Just (delegator, txHash) -> do
            -- remove the item
            let awaitingDelegation' = HM.delete baker awaitingDelegation
            -- compute the 2 epoch time since finalization
            -- and store it in waitingDelegationEpochs
            ex <- compute2epochsSinceFinalization genesisTime epochDuration backend txHash
            let waitingDelegationEpochs' = HM.insert baker (delegator, txHash, ex) waitingDelegationEpochs
            -- set a timer for next step
            waitingTime <- microsecondsBetween ex <$> getCurrentTime
            runTransitionIn logger True transitionChan waitingTime (baker, 2)
            return $
              currentState
                { waitingDelegationEpochs = waitingDelegationEpochs',
                  awaitingDelegation = awaitingDelegation'
                }
          Nothing -> do
            logger LLError $ "Invariant broken, triggering transition 1 for baker " <> toLogStr (show baker) <> " but it doesn't seem to be in the correct state:"
            logger LLError $ localStateDescription "BAD STATE" currentState
            error "Delegation should exist!!"
      logger LLTrace $ localStateDescription "leaving transition 1" currentState'
      logAndPutMVar logger "transition 1" localState currentState'
      _ <- swapMVar outerLocalState currentState'
      pure ()

transition2to3 :: Logger -> Chan (BakerId, Int) -> Chan (Maybe TransactionHash) -> EnvData -> DelegationAccounts -> Bool -> BakerId -> (MVar LocalState, MVar LocalState) -> IO ()
transition2to3 logger transitionChan loopbackChan backend cfgDir calledFromSM _ state@(localState, outerLocalState) = do
  currentState@LocalState {..} <- logAndTakeMVar logger "transition 0" localState
  (flip onException)
    ( do
        logger LLError "IO exception raised in transition 0"
        -- put back the old state
        logAndPutMVar logger "transition 0 (exception)" localState currentState
        -- trigger this same transition at some point
        writeChan transitionChan (10, 0)
    )
    $ do
      logger LLTrace $ localStateDescription "entering transition 0" currentState
      currentState' <- do
        -- if we don't have pending requests, do nothing
        let mMinElem = PSQ.findMin pendingDelegations
        case mMinElem of
          Nothing -> do
            -- this case should always be called from the state machine but...
            when calledFromSM $ writeChan loopbackChan Nothing
            return currentState
          Just (baker, _, _) -> do
            -- find an empty delegator
            let mChosenDelegator = Vec.findIndex (== Nothing) currentDelegations
            case mChosenDelegator of
              Nothing ->
                -- no available delegators, this was not called from within the state machine and therefore
                -- doesn't need a value in the loopbackChan. This will be called again when a delegation transitions out of state 4
                return currentState
              Just delegator -> do
                -- remove the item
                let pendingDelegations' = PSQ.deleteMin pendingDelegations
                    sendThisTransaction = delegate logger backend cfgDir (Just baker) delegator
                -- send delegating transaction
                th <- sendThisTransaction
                -- if needed, give this hash to the step-4 transition
                when calledFromSM $ writeChan loopbackChan (Just th)
                -- set a timer for next step
                thMVar <- finalizationListener logger backend transitionChan th (baker, 1) (Left (baker, delegator, state))
                let activeDelegations' = Set.insert baker activeDelegations
                    currentDelegations' = currentDelegations Vec.// [(delegator, Just baker)]
                    awaitingDelegation' = HM.insert baker (delegator, thMVar) awaitingDelegation
                return $
                  currentState
                    { pendingDelegations = pendingDelegations',
                      activeDelegations = activeDelegations',
                      currentDelegations = currentDelegations',
                      awaitingDelegation = awaitingDelegation'
                    }
      logger LLTrace $ localStateDescription "leaving transition 0" currentState'
      logAndPutMVar logger "transition 0" localState currentState'
      _ <- swapMVar outerLocalState currentState'
      pure ()