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
import Control.Concurrent.STM.TMVar
import Control.Monad.Except
import qualified Data.HashMap.Strict as HM
import qualified Data.HashPSQ as PSQ
import qualified Data.Set as Set
import Data.Time.Clock
import qualified Data.Vector as Vec

-- | Only will be 0-3. Represents the 4 delegators.
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
        awaitingDelegation :: !(HM.HashMap BakerId (Delegator, TransactionHash)),
        -- State 4

        -- | Baker IDs that are in the 2 epoch time slice for
        -- delegation. INVARIANT: size < 5.
        waitingDelegationEpochs :: !(HM.HashMap BakerId (Delegator, TransactionHash, UTCTime)),
        -- State 5

        -- | Baker IDs that have been issued an undelegating transaction and are
        -- waiting for it to finalize. INVARIANT: size < 5.
        awaitingUndelegation :: !(HM.HashMap BakerId (Delegator, TransactionHash)),
        -- State 6

        -- | Baker IDs that are in the 2 epoch time slice for
        -- undelegation. INVARIANT: size < 5.
        waitingUndelegationEpochs :: !(HM.HashMap BakerId (Delegator, TransactionHash, UTCTime)),
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
lpi :: String
lpi = replicate 20 ' '

localStateDescription :: String -> LocalState -> String
localStateDescription description LocalState {..} =
  unlines
    [ lpi ++ "==============================================",
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

defaultLocalState :: LocalState
defaultLocalState =
  LocalState
    { pendingDelegations = PSQ.empty,
      awaitingDelegation = HM.empty,
      waitingDelegationEpochs = HM.empty,
      awaitingUndelegation = HM.empty,
      waitingUndelegationEpochs = HM.empty,
      recentDelegations = HM.empty,
      activeDelegations = Set.empty,
      currentDelegations = Vec.replicate 4 Nothing
    }

data State
  = State
      { localState :: !(TMVar LocalState),
        backend :: EnvData,
        configDir :: DelegationAccounts,
        epochDuration :: NominalDiffTime,
        genesisTime :: UTCTime
      }

-- | Creates the vector with the transitions that are executed in the state machine.
--
-- The different states are triggered by:
-- - 2 to 3 -> some position becomes available by a delegation that transitioned 4 to 5
-- - 3 to 4 -> finalization of the transaction
-- - 4 to 5 -> 2 epochs passed
-- - 5 to 6 -> finalization of the transction
-- - 6 to 7 -> 2 epochs passed
mkTransitions :: Chan (BakerId, Int) -> Chan (Maybe TransactionHash) -> UTCTime -> NominalDiffTime -> EnvData -> DelegationAccounts -> Vec.Vector (BakerId -> TMVar LocalState -> IO ())
mkTransitions transitionChan loopbackChan genesisTime epochDuration backend cfgDir =
  Vec.fromList
    [ transition2to3 transitionChan loopbackChan backend cfgDir False,
      transition3to4 transitionChan genesisTime epochDuration backend,
      transition4to5 transitionChan loopbackChan backend cfgDir,
      transition5to6 transitionChan genesisTime epochDuration backend,
      transition6to7 transitionChan
    ]

-- | Main entry point. It creates the state machine and the channels that will communicate the threads.
forkStateMachine :: State -> Chan (BakerId, Int) -> IO ()
forkStateMachine State {..} transitionChan = do
  loopbackChan <- newChan
  let transitions = mkTransitions transitionChan loopbackChan genesisTime epochDuration backend configDir
  _ <- forkIO $ stateMachine localState transitions transitionChan
  pure ()

-- | Runs the specified transition when the given transaction shows up as finalized
finalizationListener :: EnvData -> Chan (BakerId, Int) -> TransactionHash -> (BakerId, Int) -> IO TransactionHash -> IO ()
finalizationListener backend chan txHash next fallback = do
  let loop = do
        putStrLn $ lpi ++ "[finalizationListener]: Checking transaction status of " ++ show txHash
        txState <- queryTransactionState backend txHash
        case txState of
          TxAccepted -> do
            putStrLn $ lpi ++ "[finalizationListener]: The transaction " ++ show txHash ++ " is finalized and accepted."
            writeChan chan next
          TxPending -> threadDelay 5000000 >> loop -- 5 seconds
          _ -> do
            case txState of
              TxError ->
                putStrLn $ lpi ++ "[finalizationListener]: The transaction status returned TxError. Please check the transaction hash: " ++ show txHash
              TxRejected -> putStrLn $ lpi ++ "[finalizationListener]: The transaction status shows the transaction was rejected. Please check the transaction hash: " ++ show txHash
              TxAbsent -> putStrLn $ lpi ++ "[finalizationListener]: The transaction status shows the transaction is absent. Please check the transaction hash: " ++ show txHash
              _ -> undefined -- impossible case
                -- as the state has already changed, it would be
                -- complicated to revert it keeping everything in sync, for now
                -- this same loop will try to resend the transaction with refreshed
                -- nonce and all.
            th <- fallback
            putStrLn $ lpi ++ "[finalizationListener]: Sending a new transaction " ++ show th ++ " superseding " ++ show txHash
            -- watch the new transaction
            finalizationListener backend chan th next fallback
  _ <- forkIO loop
  pure ()

-- | Runs the specified transition when the timer has expired
runTransitionIn :: Bool -> Chan (BakerId, Int) -> Int -> (BakerId, Int) -> IO ()
runTransitionIn waitingEpochs chan timer next = do
  _ <- forkIO $ do
    if waitingEpochs
      then putStrLn $ lpi ++ "[runTransitionIn]: setting a timer for " ++ show (timer `div` 1000000) ++ " seconds for a two epoch waiting time."
      else putStrLn $ lpi ++ "[runTransitionIn]: setting a timer for " ++ show (timer `div` 1000000) ++ " seconds because the delegation didn't expire yet (this is likely not going to happen)."
    threadDelay timer
    writeChan chan next
  pure ()

-- | Main loop of the state machine. Executes the transitions given through the chanToLoop channel.
stateMachine :: TMVar LocalState -> Vec.Vector (BakerId -> TMVar LocalState -> IO ()) -> Chan (BakerId, Int) -> IO ()
stateMachine localState transitions chanToLoop = do
  (bid, trans) <- readChan chanToLoop
  if trans == 0
    then putStrLn $ lpi ++ "[stateMachine]: going to promote a request from the queue (transition 0)."
    else putStrLn $ lpi ++ "[stateMachine]: going to move the baker " ++ show bid ++ " in transition " ++ transitionNames trans
  (transitions Vec.! trans) bid localState
  stateMachine localState transitions chanToLoop

transitionNames :: Int -> String
transitionNames 1 = "(1) delegate transaction was finalized, setting 2 epoch timer."
transitionNames 2 = "(2) 2 epochs passed after finalized delegate transaction, sending undelegate/redelegate transaction."
transitionNames 3 = "(3) undelegate transaction was finalized, setting 2 epoch timer."
transitionNames 4 = "(4) 2 epochs passed after finalized undelegate transaction, moving to recent delegations"
transitionNames _ = undefined -- must not happen

transition6to7 :: Chan (BakerId, Int) -> BakerId -> TMVar LocalState -> IO ()
transition6to7 transitionChan baker localState = do
  currentState@LocalState {..} <- logAndTakeTMVar "transition 4" localState
  putStrLn $ localStateDescription "entering transition 4" currentState
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
            runTransitionIn False transitionChan 5000000 (baker, 4)
            return currentState
      Nothing -> do
        putStrLn "================================================== (ERROR) =================================================="
        putStrLn $ "Invariant broken, triggering transition 4 for baker " ++ show baker ++ " but it doesn't seem to be in the correct state:"
        putStrLn $ localStateDescription "BAD STATE" currentState
        error "Delegation should exist!!"
  putStrLn $ localStateDescription "leaving transition 4" currentState'
  logAndPutTMVar "transition 4" localState currentState'

transition5to6 :: Chan (BakerId, Int) -> UTCTime -> NominalDiffTime -> EnvData -> BakerId -> TMVar LocalState -> IO ()
transition5to6 transitionChan genesisTime epochDuration backend baker localState = do
  currentState@LocalState {..} <- logAndTakeTMVar "transition 3" localState
  putStrLn $ localStateDescription "entering transition 3" currentState
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
        runTransitionIn True transitionChan waitingTime (baker, 4)
        return $
          currentState
            { waitingUndelegationEpochs = waitingUndelegationEpochs',
              awaitingUndelegation = awaitingUndelegation'
            }
      Nothing -> do
        putStrLn "================================================== (ERROR) =================================================="
        putStrLn $ "Invariant broken, triggering transition 3 for baker " ++ show baker ++ " but it doesn't seem to be in the correct state:"
        putStrLn $ localStateDescription "BAD STATE" currentState
        error "Delegation should exist!!"
  putStrLn $ localStateDescription "leaving transition 3" currentState'
  logAndPutTMVar "transition 3" localState currentState'

transition4to5 :: Chan (BakerId, Int) -> Chan (Maybe TransactionHash) -> EnvData -> DelegationAccounts -> BakerId -> TMVar LocalState -> IO ()
transition4to5 transitionChan loopbackChan backend cfgDir baker localState = do
  currentState <- logAndTakeTMVar "transition 2" localState
  putStrLn $ localStateDescription "entering transition 2" currentState
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
            logAndPutTMVar "interleaving transition 2" localState currentState'
            -- signal that a new position is available
            transition2to3 transitionChan loopbackChan backend cfgDir True 10 localState -- the baker is not needed so we will put 10 as a dummy value
              -- if the position was re-delegated, use that transaction hash,
              -- else undelegate
            mRedelegateTx <- readChan loopbackChan
            currentState'' <- logAndTakeTMVar "interleaving transition 2" localState
            let sendThisTransaction = delegate backend cfgDir Nothing delegator
            th <- maybe sendThisTransaction return mRedelegateTx
            -- create listeners for finalization
            finalizationListener backend transitionChan th (baker, 3) sendThisTransaction
            -- move to next collection
            let awaitingUndelegation' = HM.insert baker (delegator, th) (awaitingUndelegation currentState'')
            return $
              currentState''
                { awaitingUndelegation = awaitingUndelegation'
                }
          else do
            -- if not expired yet, retry this in some time (this should not happen)
            runTransitionIn False transitionChan 5000000 (baker, 2)
            return currentState
      Nothing -> do
        putStrLn "================================================== (ERROR) =================================================="
        putStrLn $ "Invariant broken, triggering transition 2 for baker " ++ show baker ++ " but it doesn't seem to be in the correct state:"
        putStrLn $ localStateDescription "BAD STATE" currentState
        error "Delegation should exist!!"
  putStrLn $ localStateDescription "leaving transition 2" currentState'
  logAndPutTMVar "transition 2" localState currentState'

transition3to4 :: Chan (BakerId, Int) -> UTCTime -> NominalDiffTime -> EnvData -> BakerId -> TMVar LocalState -> IO ()
transition3to4 transitionChan genesisTime epochDuration backend baker localState = do
  currentState@LocalState {..} <- logAndTakeTMVar "transition 1" localState
  putStrLn $ localStateDescription "entering transition 1" currentState
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
        runTransitionIn True transitionChan waitingTime (baker, 2)
        return $
          currentState
            { waitingDelegationEpochs = waitingDelegationEpochs',
              awaitingDelegation = awaitingDelegation'
            }
      Nothing -> do
        putStrLn "================================================== (ERROR) =================================================="
        putStrLn $ "Invariant broken, triggering transition 1 for baker " ++ show baker ++ " but it doesn't seem to be in the correct state:"
        putStrLn $ localStateDescription "BAD STATE" currentState
        error "Delegation should exist!!"
  putStrLn $ localStateDescription "leaving transition 1" currentState'
  logAndPutTMVar "transition 1" localState currentState'

transition2to3 :: Chan (BakerId, Int) -> Chan (Maybe TransactionHash) -> EnvData -> DelegationAccounts -> Bool -> BakerId -> TMVar LocalState -> IO ()
transition2to3 transitionChan loopbackChan backend cfgDir calledFromSM _ localState = do
  currentState@LocalState {..} <- logAndTakeTMVar "transition 0" localState
  putStrLn $ localStateDescription "entering transtion 0" currentState
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
                sendThisTransaction = delegate backend cfgDir (Just baker) delegator
            -- send delegating transaction
            th <- sendThisTransaction
            -- if needed, give this hash to the step-4 transition
            when calledFromSM $ writeChan loopbackChan (Just th)
            -- set a timer for next step
            finalizationListener backend transitionChan th (baker, 1) sendThisTransaction
            let activeDelegations' = Set.insert baker activeDelegations
                currentDelegations' = currentDelegations Vec.// [(delegator, Just baker)]
                awaitingDelegation' = HM.insert baker (delegator, th) awaitingDelegation
            return $
              currentState
                { pendingDelegations = pendingDelegations',
                  activeDelegations = activeDelegations',
                  currentDelegations = currentDelegations',
                  awaitingDelegation = awaitingDelegation'
                }
  putStrLn $ localStateDescription "leaving transition 0" currentState'
  logAndPutTMVar "transition 0" localState currentState'
