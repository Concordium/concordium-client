module Concordium.DelegationServer.Api.Implementation where

import Concordium.DelegationServer.Api.Definition
import Concordium.DelegationServer.Helpers
import Concordium.DelegationServer.Logic
import Concordium.Types as Types
import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.HashMap.Strict as HM hiding (filter)
import qualified Data.HashPSQ as PSQ
import Data.Hashable
import Data.Set as Set hiding (filter)
import qualified Data.Text as Text
import Data.Time
import qualified Data.Vector as Vec
import Servant

-- | Counts the elements that satisfy the predicate stopping at the first
-- element that doesn't satisfy it
psqCountWhile :: (Ord p, Hashable k, Ord k) => (k -> Bool) -> PSQ.HashPSQ k p v -> Int
psqCountWhile f pq = go pq 0
  where
    go p acc =
      let minV = PSQ.findMin p
       in case minV of
            Nothing -> acc
            Just (a, _, _) ->
              if f a
                then go (PSQ.deleteMin p) (acc + 1)
                else acc

-- | Returns a rough estimate on the waiting time calculated as 2 epochs if all
-- the delegators are active right now and 4 more epochs (2 for delegating and 2
-- for undelegating) for each set of 4 bakers that are waiting before the given
-- baker id.
calculateEstimatedWaitingEpochs :: Maybe Types.BakerId -> PSQ.HashPSQ Types.BakerId Int () -> Vec.Vector (Maybe BakerId) -> Int
calculateEstimatedWaitingEpochs (Just baker) pending current = 2 * (currentActive + 2 * currentWaiting)
  where
    currentActive = if Prelude.null $ Vec.find (== Nothing) current then 1 else 0
    currentWaiting = psqCountWhile (/= baker) pending `div` 4
calculateEstimatedWaitingEpochs Nothing pending current = 2 * (currentActive + 2 * currentWaiting)
  where
    currentActive = if Prelude.null $ Vec.find (== Nothing) current then 1 else 0
    currentWaiting = PSQ.size pending `div` 4

estimatedTimeAsUTC :: NominalDiffTime -> Maybe Types.BakerId -> PSQ.HashPSQ Types.BakerId Int () -> Vec.Vector (Maybe BakerId) -> IO UTCTime
estimatedTimeAsUTC epochDuration delegateTo pendingDelegations currentDelegations =
  addUTCTime (epochDuration * fromInteger (fromIntegral (calculateEstimatedWaitingEpochs delegateTo pendingDelegations currentDelegations))) <$> getCurrentTime

type AppM = ReaderT State Handler

getDelegations :: AppM CurrentDelegations
getDelegations = do
  state <- ask
  currentState <- liftIO . logAndReadTMVar "getDelegations" $ localState state
  return . CurrentDelegations . Vec.toList . currentDelegations $ currentState

requestDelegation :: Chan (BakerId, Int) -> RequestDelegationRequest -> AppM RequestDelegationResponse
requestDelegation transitionChan RequestDelegationRequest {..} = do
  state <- ask
  currentState@LocalState {..} <- liftIO . logAndTakeTMVar ("requestDelegation (" ++ show delegateTo ++ ")") $ localState state
  if Set.member delegateTo activeDelegations
    then do
      liftIO $ logAndPutTMVar ("requestDelegation (" ++ show delegateTo ++ ")") (localState state) currentState
      return RequestDelegationNotAccepted
    else do
      let cnt = case HM.lookup delegateTo recentDelegations of
            Just (c, _) -> c
            Nothing -> 0
          pendingDelegations' = PSQ.insert delegateTo cnt () pendingDelegations
          state' =
            currentState
              { pendingDelegations = pendingDelegations'
              }
      liftIO $ logAndPutTMVar ("requestDelegation (" ++ show delegateTo ++ ")") (localState state) state'
      -- notify the state machine that it can start with transition 0
      liftIO $ writeChan transitionChan (10, 0) -- the first item in the tuple is not needed so we will put 10 as a dummy value
      RequestDelegationAccepted <$> liftIO (tsMillis . utcTimeToTimestamp <$> estimatedTimeAsUTC (epochDuration state) (Just delegateTo) pendingDelegations' currentDelegations)

getDelegationStatus :: GetDelegationStatusRequest -> AppM GetDelegationStatusResponse
getDelegationStatus GetDelegationStatusRequest {..} = do
  state <- ask
  LocalState {..} <- liftIO . logAndReadTMVar ("getDelegationStatus (" ++ show delegateTo ++ ")") $ localState state
  if PSQ.member delegateTo pendingDelegations
    then-- State 2
      DelegationInQueue <$> liftIO (tsMillis . utcTimeToTimestamp <$> estimatedTimeAsUTC (epochDuration state) (Just delegateTo) pendingDelegations currentDelegations)
    else case HM.lookup delegateTo awaitingDelegation of
      -- State 3
      Just (_, th) -> return $ DelegationPending (Text.pack $ show th) Nothing
      Nothing -> case HM.lookup delegateTo waitingDelegationEpochs of
        -- State 4
        Just (_, th, expectedTime) -> return $ DelegationPending (Text.pack $ show th) (Just (tsMillis . utcTimeToTimestamp $ expectedTime))
        Nothing -> case HM.lookup delegateTo awaitingUndelegation of
          -- State 5
          Just (_, th) -> return $ DelegationAssigned (Text.pack $ show th) Nothing
          Nothing -> case HM.lookup delegateTo waitingUndelegationEpochs of
            -- State 6
            Just (_, th, expectedTime) -> return $ DelegationAssigned (Text.pack $ show th) (Just (tsMillis . utcTimeToTimestamp $ expectedTime))
            Nothing -> case HM.lookup delegateTo recentDelegations of
              -- State 7
              Just (_, expired) -> DelegationFinished (tsMillis . utcTimeToTimestamp $ expired) <$> liftIO (tsMillis . utcTimeToTimestamp <$> estimatedTimeAsUTC (epochDuration state) (Just delegateTo) pendingDelegations currentDelegations)
              -- State 0
              Nothing -> DelegationAbsent <$> liftIO (tsMillis . utcTimeToTimestamp <$> estimatedTimeAsUTC (epochDuration state) Nothing pendingDelegations currentDelegations)

server :: Chan (BakerId, Int) -> ServerT DelegationAPI AppM
server chan = getDelegations :<|> requestDelegation chan :<|> getDelegationStatus