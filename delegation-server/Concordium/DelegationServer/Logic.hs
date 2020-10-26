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
import Control.Exception (handle, SomeException(..), displayException)
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
data DelegationsState
  = DelegationsState
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

localStateDescription :: String -> DelegationsState -> LogStr
localStateDescription description DelegationsState {..} = toLogStr $
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

transitionNames :: Int -> LogStr
transitionNames 1 = "(1) delegate transaction was finalized, setting 2 epoch timer."
transitionNames 2 = "(2) 2 epochs passed after finalized delegate transaction, sending undelegate/redelegate transaction."
transitionNames 3 = "(3) undelegate transaction was finalized, setting 2 epoch timer."
transitionNames 4 = "(4) 2 epochs passed after finalized undelegate transaction, moving to recent delegations"
transitionNames _ = undefined -- must not happen

defaultLocalState :: Int -> DelegationsState
defaultLocalState numDelegators =
  DelegationsState
    { pendingDelegations = PSQ.empty,
      awaitingDelegation = HM.empty,
      waitingDelegationEpochs = HM.empty,
      awaitingUndelegation = HM.empty,
      waitingUndelegationEpochs = HM.empty,
      recentDelegations = HM.empty,
      activeDelegations = Set.empty,
      currentDelegations = Vec.replicate numDelegators Nothing
    }

data ServerState
  = ServerState
      { numDelegators :: !Int,
        localState :: !(MVar DelegationsState),
        outerLocalState :: !(MVar DelegationsState),
        backend :: !EnvData,
        delegatorAccounts :: !DelegationAccounts,
        epochDuration :: !NominalDiffTime,
        genesisTime :: !UTCTime,
        serviceLogger :: !Logger
      }

type Rollback = DelegationsState -> DelegationsState

-- | A pair of states, the first one is locked on transitions, the second one is just swapped for GET requests
type States = (MVar DelegationsState, MVar DelegationsState)

-- | Runs the specified transition when the given transaction shows up as finalized
finalizationListener :: Logger                     -- ^The logger service
                     -> EnvData                    -- ^The GRPC backend
                     -> Chan (BakerId, Int)        -- ^Channel used to notify for triggering a transition
                     -> MVar TransactionHash       -- ^The MVar to the hash of the current transaction that is being watched
                     -> (BakerId, Int)             -- ^The next transition that should be triggered
                     -> States                     -- ^MVars to the state
                     -> Maybe Rollback             -- ^How to restore the state
                     -> IO TransactionHash         -- ^An IO action to retry the transaction
                     -> IO ()
finalizationListener logger backend chan txHashMVar next (state, outerState) rollback retry = do
  let loop thMVar = do
        txHash <- readMVar thMVar

        let letsRetry = do
              handle ( \(e :: SomeException) -> do
                         logger LLWarning $ "[finalizationListener]: There was an exception (" <> toLogStr (show e) <> ") when resending a transaction. This probably is a temporal issue, so we will retry after some time."
                         threadDelay 5000000
                         letsRetry )
                ( do
                    -- send the new transaction
                    txHash' <- retry
                    -- swap the MVar to the new transaction
                    _ <- swapMVar thMVar txHash'
                    logger LLInfo $ "[finalizationListener]: Sent a new transaction " <> logHash txHash' <> " superseding " <> logHash txHash
                    threadDelay 5000000
                    -- watch the new transaction
                    loop thMVar )

        logger LLDebug $ "[finalizationListener]: Checking transaction status of " <> toLogStr (show txHash)
        txState <- handle ( \(e :: SomeException) -> return $ CaughtException (displayException e)) (queryTransactionState backend txHash)
        case txState of
          -- OK
          TxAccepted -> do
            logger LLInfo $ "[finalizationListener]: The transaction " <> toLogStr (show txHash) <> " is finalized and accepted."
            -- trigger the next transition
            writeChan chan next

          -- Loop
          TxPending -> do
            logger LLInfo $ "[finalizationListener]: The transaction " <> toLogStr (show txHash) <> " is still pending."
            threadDelay 5000000
            -- retry in 5 seconds
            loop thMVar

          -- Retry
          TxAbsent -> do
            logger LLWarning $ "[finalizationListener]: The transaction status shows the transaction is absent. Please check the transaction hash: " <> logHash txHash
            letsRetry

          -- Retry
          CaughtException e -> do
            logger LLWarning $ "[finalizationListener]: There was an exception (" <> toLogStr (show e) <> ") when checking the transaction status. This probably is a temporal issue, so we will retry after some time."
            letsRetry

          -- Critical error
          TxError -> logger LLWarning $ "[finalizationListener]: The transaction was finalized in several blocks. This is a critical error. Please check the transaction hash: " <> logHash txHash

          -- Maybe retry or abort rolling back
          TxRejected -> do
            logger LLWarning $ "[finalizationListener]: The transaction status shows the transaction was rejected. Please check the transaction hash: " <> logHash txHash
            case rollback of
              Nothing -> letsRetry
              Just rb -> do
                logger LLInfo "[finalizationListener]: Exception raised and caught. Restoring the state."
                rolledBackState <- rb <$> logAndTakeMVar logger "finalization listener (exception)" state
                logAndPutMVar logger "finalization listener (exception)" state rolledBackState
                _ <- swapMVar outerState rolledBackState
                writeChan chan (10, 0) -- this will be triggered when a baker is missing and the transaction was rejected. This means that a new delegation will be promoted.
  _ <- forkIO (loop txHashMVar)
  pure ()

-- | Runs the specified transition when the timer has expired
runTransitionIn :: Logger              -- ^The logger service
                -> Bool                -- ^Print on the log that we are waiting for two epochs or not
                -> Chan (BakerId, Int) -- ^Channel for triggering the next transition
                -> Int                 -- ^the number of Nanoseconds we want to wait
                -> (BakerId, Int)      -- ^The next transition to trigger
                -> IO ()
runTransitionIn logger waitingEpochs chan timer next = do
  _ <- forkIO $ do
    if waitingEpochs
      then logger LLTrace $ "[runTransitionIn]: setting a timer for " <> toLogStr (show (timer `div` 1000000)) <> " seconds for a two epoch waiting time."
      else logger LLTrace $ "[runTransitionIn]: setting a timer for " <> toLogStr (timer `div` 1000000) <> " seconds because the delegation didn't expire yet." -- (this is likely not going to happen)
    threadDelay timer
    writeChan chan next
  pure ()

-- | Creates the vector with the transitions that are executed in the state machine.
--
-- The different states are triggered by:
-- - doDelegate -> some position becomes available by a delegation that transitioned 4 to 5
-- - waitDelegate -> finalization of the transaction
-- - doUndelegate -> 2 epochs passed
-- - waitUndelegate -> finalization of the transction
-- - toRecentDelegations -> 2 epochs passed
mkTransitions :: Logger                              -- ^Logger to inject into the transitions.
              -> Chan (BakerId, Int)                 -- ^Transition channel, the server loop is listening on this one.
              -> Chan (Maybe (MVar TransactionHash)) -- ^Loopback channel, used to communicate transitions 3 and 1.
              -> UTCTime                             -- ^Genesis time
              -> NominalDiffTime                     -- ^Epoch duration
              -> EnvData                             -- ^GRPC Backend
              -> DelegationAccounts                  -- ^Vector with delegation accounts
              -> Vec.Vector (BakerId -> States -> IO ())
mkTransitions logger transitionChan loopbackChan genesisTime epochDuration backend delegatorAccounts =
  Vec.fromList
    [ doDelegate logger transitionChan loopbackChan backend delegatorAccounts False,
      waitDelegate logger transitionChan genesisTime epochDuration backend,
      doUndelegate logger transitionChan loopbackChan backend delegatorAccounts,
      waitUndelegate logger transitionChan genesisTime epochDuration backend,
      toRecentDelegations logger transitionChan
    ]

{-------------------------------- State machine and transitions ------------------------------------}

-- | Main entry point. It creates the state machine and the channels that will communicate the threads.
forkStateMachine :: ServerState -> Chan (BakerId, Int) -> IO ()
forkStateMachine ServerState {..} transitionChan = do
  loopbackChan <- newChan
  let transitions = mkTransitions serviceLogger transitionChan loopbackChan genesisTime epochDuration backend delegatorAccounts
  _ <- forkIO $ stateMachine serviceLogger (localState, outerLocalState) transitions transitionChan
  pure ()

-- | Main loop of the state machine. Executes the transitions given through the chanToLoop channel.
stateMachine :: Logger                                             -- ^Logger service
             -> States                                             -- ^MVars to the state
             -> Vec.Vector (BakerId -> States -> IO ())                -- ^Transitions
             -> Chan (BakerId, Int)                                -- ^Channel to trigger transitions
             -> IO ()
stateMachine logger localState transitions chanToLoop = loop
  where loop = do
          (bid, trans) <- readChan chanToLoop
          if trans == 0
            then logger LLDebug "[stateMachine]: going to promote a request from the queue (transition 0)."
            else logger LLDebug $ "[stateMachine]: going to move the baker " <> toLogStr (show bid) <> " in transition " <> transitionNames trans
          (transitions Vec.! trans) bid localState
          loop

toRecentDelegations :: Logger                -- ^Logger service
                    -> Chan (BakerId, Int)   -- ^Channel to trigger transitions
                    -> BakerId               -- ^Baker id to transition
                    -> States                -- ^MVars to the state
                    -> IO ()
toRecentDelegations logger transitionChan baker (mState, outerState) = do
  -- get current state
  currentState@DelegationsState {..} <- logAndTakeMVar logger "transition 4" mState
  handle -- can not be triggered in a reasonable universe (the only IO action that could fail is getCurrentTime)
    ( \(_ :: SomeException) -> do
        logger LLError "IO exception raised in transition 4"
        -- put back the old state
        logAndPutMVar logger "transition 4 (exception)" mState currentState
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
            -- the baker was delegated to
            -- the tx was finalized and 2 epochs have passed
            -- the undelegate tx was finalized and 2 epochs have passed
            -- but we cannot find the baker in the correct collection
            logger LLError $ "Invariant violation, triggering transition 4 for baker " <> toLogStr (show baker) <> " but it doesn't seem to be in the correct state:"
            logger LLError $ localStateDescription "BAD STATE" currentState
            -- we just return the current state without triggering an exception so that we just continue omitting this transition
            return currentState
      logger LLTrace $ localStateDescription "leaving transition 4" currentState'
      logAndPutMVar logger "transition 4" mState currentState'
      _ <- swapMVar outerState currentState'
      pure ()

waitUndelegate :: Logger                -- ^The logger service
               -> Chan (BakerId, Int)   -- ^The channel to trigger other transitions
               -> UTCTime               -- ^Genesis time
               -> NominalDiffTime       -- ^Epoch duration
               -> EnvData               -- ^The GRPC backend
               -> BakerId               -- ^The baker id we have to transition
               -> States                -- ^MVars to the state
               -> IO ()
waitUndelegate logger transitionChan genesisTime epochDuration backend baker (mState, outerState) = do
  -- get current state
  currentState@DelegationsState {..} <- logAndTakeMVar logger "transition 3" mState
  handle -- can only be reasonably triggered when checking the txhash for computing the waiting time
    ( \(_ :: SomeException) -> do
        logger LLError "IO exception raised in transition 3"
        -- put back the old state
        logAndPutMVar logger "transition 3 (exception)" mState currentState
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
            -- the baker was delegated to
            -- the delegate transaction was finalized and 2 epochs have passed
            -- the undelegate transaction was finalized
            -- but now we cannot find the baker in the correct collection
            logger LLError $ "Invariant broken, triggering transition 3 for baker " <> toLogStr (show baker) <> " but it doesn't seem to be in the correct state:"
            logger LLError $ localStateDescription "BAD STATE" currentState
            -- we just return the current state without triggering an exception so that we just continue omitting this transition
            return currentState
      logger LLTrace $ localStateDescription "leaving transition 3" currentState'
      logAndPutMVar logger "transition 3" mState currentState'
      _ <- swapMVar outerState currentState'
      pure ()

doUndelegate :: Logger                              -- ^The logger service
             -> Chan (BakerId, Int)                 -- ^Channel for triggering transitions
             -> Chan (Maybe (MVar TransactionHash)) -- ^Loopback channel to notify doUndelegate
             -> EnvData                             -- ^GRPC backend
             -> DelegationAccounts                  -- ^The delegation accounts
             -> BakerId                             -- ^Which baker we are going to do an undelegate for
             -> States                              -- ^MVars to the state
             -> IO ()
doUndelegate logger transitionChan loopbackChan backend delegatorAccounts baker states@(mState, outerState) = do
  -- get current state
  currentState <- logAndTakeMVar logger "transition 2" mState
  handle -- cannot only be triggered if the undelegate transaction is sent here and it's a grpc error (reasonably)
    ( \(_ :: SomeException) -> do
        logger LLError "IO exception raised in transition 2"
        -- put back the old state
        logAndPutMVar logger "transition 2 (exception)" mState currentState
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
                    -- prepare an intermediate state
                    currentState' =
                      currentState
                        { waitingDelegationEpochs = waitingDelegationEpochs',
                          activeDelegations = activeDelegations',
                          currentDelegations = currentDelegations'
                        }
                logAndPutMVar logger "interleaving transition 2" mState currentState'
                -- signal that a new position is available
                doDelegate logger transitionChan loopbackChan backend delegatorAccounts True 10 states -- the baker is not needed so we will put 10 as a dummy value
                -- if the position was re-delegated, use that transaction hash,
                -- else undelegate
                mRedelegateTx <- readChan loopbackChan
                currentState'' <- logAndTakeMVar logger "interleaving transition 2" mState
                let sendThisTransaction = delegate logger backend delegatorAccounts Nothing delegator
                thMVar <- maybe (sendThisTransaction >>= newMVar) return mRedelegateTx

                -- create listeners for finalization
                -- in case mRedelegateTx was Just, then the doDelegate already set up a listener for the transaction it sent.
                -- If that transaction failed then the state will be rolled-back correctly for that transaction.
                --
                -- Here we register a new listener which will, if that delegate transaction fails, not touch the state, but
                -- instead keep trying to resend the undelegate transaction. This can only fail if our account keys are wrong,
                -- which will cause many other problems, so retrying in this case is acceptable.
                --
                -- If mRedelegateTx was Nothing then we send an undelegate transaction and set up a listener for it.
                finalizationListener logger backend transitionChan thMVar (baker, 3) states Nothing sendThisTransaction -- undelegate transaction should not fail, so retry
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
            -- The baker was delegated to
            -- the transaction was finalized
            -- the 2 epochs have passed
            -- but now we cannot find the baker in the collection
            logger LLError $ "Invariant broken, triggering transition 2 for baker " <> toLogStr (show baker) <> " but it doesn't seem to be in the correct state:"
            logger LLError $ localStateDescription "BAD STATE" currentState
            -- we just return the current state removing the baker if needed without triggering an exception so that we just continue omitting this transition
            -- just in case it was missing from one of the collections only
            let activeDelegations' = Set.delete baker (activeDelegations currentState)
                currentDelegations' = case Vec.findIndex (== Just baker) (currentDelegations currentState) of
                                                            Nothing -> currentDelegations currentState
                                                            Just i -> currentDelegations currentState Vec.// [(i, Nothing)]
                currentState' =
                      currentState
                        { activeDelegations = activeDelegations',
                          currentDelegations = currentDelegations'
                        }
            writeChan transitionChan (10, 0) -- we are now missing one baker that was going to un/redelegate, so there is a spot free for another request
            return currentState'
      logger LLTrace $ localStateDescription "leaving transition 2" currentState'
      logAndPutMVar logger "transition 2" mState currentState'
      _ <- swapMVar outerState currentState'
      pure ()

waitDelegate :: Logger                -- ^The logger service
             -> Chan (BakerId, Int)   -- ^The transition channel to notify for transitions
             -> UTCTime               -- ^The genesis time
             -> NominalDiffTime       -- ^The epoch duration
             -> EnvData               -- ^The GRPC backend
             -> BakerId               -- ^The baker id that must transition
             -> States                -- ^MVars to the state
             -> IO ()
waitDelegate logger transitionChan genesisTime epochDuration backend baker (mState, outerState) = do
  -- get current state
  currentState@DelegationsState {..} <- logAndTakeMVar logger "transition 1" mState
  handle -- can only be reasonably triggered when checking the txhash for computing the waiting time
    ( \(_ :: SomeException) -> do
        logger LLError "IO exception raised in transition 1"
        -- put back the old state
        logAndPutMVar logger "transition 1 (exception)" mState currentState
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
            -- Run the transition when the time has passed
            runTransitionIn logger True transitionChan waitingTime (baker, 2)
            return $
              currentState
                { waitingDelegationEpochs = waitingDelegationEpochs',
                  awaitingDelegation = awaitingDelegation'
                }
          Nothing -> do
            -- The baker was delegated to, and the transaction was finalized (because this transition was triggered)
            -- but now we cannot find it in the bakers that are awaiting for the 2 epochs
            logger LLError $ "Invariant broken, triggering transition 1 for baker " <> toLogStr (show baker) <> " but it doesn't seem to be in the correct state:"
            logger LLError $ localStateDescription "BAD STATE" currentState
            -- return the same state
            return currentState
      logger LLTrace $ localStateDescription "leaving transition 1" currentState'
      logAndPutMVar logger "transition 1" mState currentState'
      _ <- swapMVar outerState currentState'
      pure ()

doDelegate :: Logger                              -- ^The logger service
           -> Chan (BakerId, Int)                 -- ^Channel for triggering transitions
           -> Chan (Maybe (MVar TransactionHash)) -- ^Loopback channel to notify doUndelegate
           -> EnvData                             -- ^GRPC backend
           -> DelegationAccounts                  -- ^The delegation accounts
           -> Bool                                -- ^Whether this was called from the state machine, i.e. from doUndelegate
           -> BakerId                             -- ^Just to normalize the interface with other transitions
           -> States                              -- ^MVars to the state
           -> IO ()
doDelegate logger transitionChan loopbackChan backend delegatorAccounts calledFromSM _ states@(mState, outerState) = do
  -- get the current state
  currentState <- logAndTakeMVar logger "transition 0" mState
  handle -- can only be reasonably called when there is a grpc error when sending the transaction
    ( \(_ :: SomeException) -> do
        logger LLError "IO exception raised in transition 0"
        -- put back the old state
        logAndPutMVar logger "transition 0 (exception)" mState currentState
        -- trigger this same transition at some point
        writeChan transitionChan (10, 0)
    )
    $ do
      logger LLTrace $ localStateDescription "entering transition 0" currentState
      currentState' <- do
        let mMinElem = PSQ.findMin (pendingDelegations currentState)
        case mMinElem of
          Nothing -> do
            -- if we don't have pending requests, do nothing.
            -- this case should always be called from the state machine but just in case
            when calledFromSM $ writeChan loopbackChan Nothing
            -- return the same state
            return currentState
          Just (baker, _, _) -> do
            -- find an empty delegator
            let mChosenDelegator = Vec.findIndex (== Nothing) (currentDelegations currentState)
            case mChosenDelegator of
              Nothing ->
                -- no available delegators, this was not called from within the state machine and therefore
                -- doesn't need a value in the loopbackChan. This will be called again when a delegation
                -- transitions on doUndelegate.
                --
                -- So we return the same state
                return currentState
              Just delegator -> do
                -- remove the item
                let pendingDelegations' = PSQ.deleteMin (pendingDelegations currentState)
                    -- prepare the transaction to be sent
                    sendThisTransaction = delegate logger backend delegatorAccounts (Just baker) delegator
                -- send delegating transaction
                thMVar <- sendThisTransaction >>= newMVar
                -- if we need to rollback it is because the transaction failed not due to an IO error,
                -- so we will remove this pending delegation request and continue normally
                let rollback d = d { pendingDelegations = PSQ.delete baker (pendingDelegations d),
                                     -- if it was registered as an active delegation then remove it
                                     activeDelegations = Set.delete baker (activeDelegations d),
                                     currentDelegations = case Vec.findIndex (== Just baker) (currentDelegations d) of
                                                            Nothing -> currentDelegations d
                                                            Just i -> currentDelegations d Vec.// [(i, Nothing)],
                                     awaitingDelegation = HM.delete baker (awaitingDelegation d)
                                   }
                -- set a timer for next step
                finalizationListener logger backend transitionChan thMVar (baker, 1) states (Just rollback) sendThisTransaction
                -- if needed, give this txHash to the doUndelegate transition
                when calledFromSM $ writeChan loopbackChan (Just thMVar)
                -- update the collections and return the updated state
                let activeDelegations' = Set.insert baker (activeDelegations currentState)
                    currentDelegations' = currentDelegations currentState Vec.// [(delegator, Just baker)]
                    awaitingDelegation' = HM.insert baker (delegator, thMVar) (awaitingDelegation currentState)
                return $
                  currentState
                    { pendingDelegations = pendingDelegations',
                      activeDelegations = activeDelegations',
                      currentDelegations = currentDelegations',
                      awaitingDelegation = awaitingDelegation'
                    }
      logger LLTrace $ localStateDescription "leaving transition 0" currentState'
      logAndPutMVar logger "transition 0" mState currentState'
      _ <- swapMVar outerState currentState'
      pure ()
