module Concordium.Client.RWLock (
    RWLock,
    initializeLock,
    withReadLock,
    withWriteLock,
) where

import Control.Concurrent
import Control.Exception
import Data.Word

-- | A reader-writer lock that strongly prefer writers. More precisely this means the following
--  - readers and writers are mutually exclusive
--  - multiple readers may hold the lock at the same time if there is no writer
--  - at most one writer may hold the lock
--
--  If a writer tries to acquire a lock it will either
--  - succeed if there are no current readers or writers
--  - block after recording the intent to lock. While there are pending writers no new readers can acquire the lock.
--
--  If multiple writers are blocking on the lock they will be served in an
--  unspecified order and in principle it is possible that with heavy write
--  contention some writers would be starved. This is not the case for the
--  use-case we have.
--
--  Let ⊤ mean that the MVar is full and ⊥ that it is empty. The fields of the lock satisfy the following
--  properties.
--  - there are exactly waitingWriters threads blocking on acquireWrite
--  - rwlState == Free if and only if rwlReadLock == ⊤ and rwlWriteLock == ⊤
--  - rwlReadLock == ⊥ if and only if there is an active reader.
--  - rwlWriteLock == ⊥ if and only if there is an active writer.
--
--  Transitions between states are governed by the following transition system
--  where AW/RW and AR/RR mean acquire write, release write and acquire read,
--  release read, respectively. The WR and WW mean that the thread that
--  executed the transition is blocked waiting for rwlReadLock and rwlWriteLock MVar to be full.
--  (Free 0, ⊤, ⊤) -AR-> (ReadLocked 1 0, ⊥, ⊤)
--  (Free (n+1), ⊤, ⊤) -AR-> (Free (n+1), ⊤, ⊤)
--  (Free 0, ⊤, ⊤) -AW-> (WriteLocked 0, ⊤, ⊥)
--  (Free (n+1), ⊤, ⊤) -AW-> (WriteLocked n, ⊤, ⊥)
--
--  (ReadLocked n 0, ⊥, ⊤) -AR-> (ReadLocked (n+1) 0, ⊥, ⊤)
--  (ReadLocked n (m+1), ⊥, ⊤) -AR-> (ReadLocked n (m+1), ⊥, ⊤), WR
--  (ReadLocked n m, ⊥, ⊤) -AW-> (ReadLocked n (m+1), ⊥, ⊤), WW
--  (ReadLocked 1 m, ⊥, ⊤) -RR-> (Free m, ⊤, ⊤)
--  (ReadLocked (n+1) m, ⊥, ⊤) -RR-> (ReadLocked n m, ⊥, ⊤)
--
--  (WriteLocked n, ⊤, ⊥) -AR-> (WriteLocked n, ⊤, ⊥)
--  (WriteLocked n, ⊤, ⊥) -AW-> (WriteLocked (n+1), ⊤, ⊥), WR
--  (WriteLocked n, ⊤, ⊥) -RW-> (Free n, ⊤, ⊤), WW
--
--  No other state should be reachable.
--
--  Additionally, rwlReadLock and rwlWriteLock can only be modified while the
--  rwlState MVar is held.
data RWLock = RWLock
    { -- | The state the lock is currently in.
      rwlState :: !(MVar RWState),
      -- | An MVar used to signal threads that are waiting for all active readers to
      --  wake up. This is empty when there is at least one active reader and full
      --  otherwise.
      rwlReadLock :: !(MVar ()),
      -- | An MVar used to signal waiting readers and writers to wake up. This is
      --  empty when there is an active writer, and full otherwise. Readers wait on
      --  this MVar when there is an active writer.
      rwlWriteLock :: !(MVar ())
    }

-- | State of a reader-writer lock.
data RWState
    = -- | Nobody has acquired the lock.
      Free
        { -- | The lock is not acquired, but there might be pending writers that want to acquire it.
          waitingWriters :: !Word64
        }
    | -- | There is at least one active reader.
      ReadLocked
        { -- | The number of readers that are currently active.
          readers :: !Word64,
          -- | The number of pending writers.
          waitingWriters :: !Word64
        }
    | -- | The lock is acquired by a single writer.
      WriteLocked
        { -- | The number of writers that are pending (that is, currently blocked on this lock).
          waitingWriters :: !Word64
        }
    deriving (Show)

-- | Initialize a lock in the unlocked state.
initializeLock :: IO RWLock
initializeLock = do
    rwlState <- newMVar (Free 0)
    rwlReadLock <- newMVar ()
    rwlWriteLock <- newMVar ()
    return RWLock{..}

-- | Acquire a read lock. This will block until there are no pending writers
--  waiting to acquire the lock.
acquireRead :: RWLock -> IO ()
acquireRead RWLock{..} = mask_ go
  where
    go =
        takeMVar rwlState >>= \case
            st@(Free waitingWriters)
                | waitingWriters == 0 -> do
                    -- the lock is free and there are no waiting writers. Acquire a read lock.
                    takeMVar rwlReadLock
                    putMVar rwlState (ReadLocked 1 0)
                | otherwise -> do
                    -- the lock is free, but there are waiting writers. We do nothing and try again.
                    -- Due to fairness of MVars next time another thread will make progress
                    -- so we are going to end up after a finite number of iterations, in a WriteLocked state.
                    putMVar rwlState st
                    -- Since this branch seems to be compiled into a loop without
                    -- allocations by GHC with -O2 we need to explicitly yield to allow others
                    -- to make progress. Otherwise with sufficient contention this loop ends up
                    -- starving other threads since they are never scheduled. This then also means
                    -- the loop never terminates since no other thread transitions from the Free
                    -- to WriteLocked state.
                    yield
                    go
            st@(ReadLocked n waitingWriters)
                | waitingWriters == 0 ->
                    -- No waiting writers, add another reader.
                    putMVar rwlState $! ReadLocked (n + 1) 0
                | otherwise -> do
                    -- Some readers hold the lock, but there are waiting writers.
                    -- We do nothing and wait until there are no more readers and attempt again.
                    -- At that point we are likely to end up in WriteLocked state.
                    putMVar rwlState st
                    readMVar rwlReadLock
                    go
            lockState@(WriteLocked _) -> do
                -- There is an active writer. Do nothing and wait until the writer is done.
                putMVar rwlState lockState
                readMVar rwlWriteLock
                go

-- | Acquire a write lock. This will block when there are active readers or
--  writers. When this operation is blocked it also blocks new readers from
--  acquiring the lock.
acquireWrite :: RWLock -> IO ()
acquireWrite RWLock{..} = mask_ $ go False
  where
    -- the boolean flag indicates whether this is a first iteration of the loop (False) or not (True)
    go alreadyWaiting =
        takeMVar rwlState >>= \case
            (Free waitingWriters) -> do
                -- The lock is free, take it.
                takeMVar rwlWriteLock
                putMVar rwlState $! WriteLocked (waitingWriters - if alreadyWaiting then 1 else 0)
            (ReadLocked n waitingWriters) -> do
                -- There are active readers. Queue ourselves up and wait until all existing readers
                -- are done. This will block all subsequent readers from acquiring the lock.
                putMVar rwlState $! ReadLocked n (waitingWriters + if alreadyWaiting then 0 else 1)
                readMVar rwlReadLock
                go True
            (WriteLocked waitingWriters) -> do
                -- There is an active writer. Queue ourselves up so that readers are
                -- blocked from acquiring the lock and wait until the current writer is done.
                putMVar rwlState $! WriteLocked (waitingWriters + if alreadyWaiting then 0 else 1)
                readMVar rwlWriteLock
                go True

-- | Release the write lock. The lock is assumed to be in write state, otherwise
--  this function will raise an exception.
releaseWrite :: RWLock -> IO ()
releaseWrite RWLock{..} =
    mask_ $
        takeMVar rwlState >>= \case
            (WriteLocked waitingWriters) -> do
                putMVar rwlWriteLock ()
                putMVar rwlState (Free waitingWriters)
            lockState -> do
                putMVar rwlState lockState
                error $ "releaseWrite: attempting to release while in state: " ++ show lockState

-- | Release the read lock. The lock is assumed to be in read state, otherwise
--  this function will raise an exception. Note that since multiple readers may
--  acquire the read lock at the same time this either decrements the read count
--  and leaves the lock in read state, or unlocks it if called when there is only
--  a single active reader.
releaseRead :: RWLock -> IO ()
releaseRead RWLock{..} =
    mask_ $
        takeMVar rwlState >>= \case
            (ReadLocked 1 waitingWriters) -> do
                putMVar rwlReadLock ()
                putMVar rwlState (Free waitingWriters)
            (ReadLocked n waitingWriters) -> putMVar rwlState $! ReadLocked (n - 1) waitingWriters
            lockState -> do
                putMVar rwlState lockState
                error $ "releaseRead: attempting to release read when in state: " ++ show lockState

-- | Acquire the write lock and execute the action. The lock will be released
--  even if the action raises an exception. See 'acquireWrite' for more details.
withWriteLock :: RWLock -> IO a -> IO a
withWriteLock ls = bracket_ (acquireWrite ls) (releaseWrite ls)

-- | Acquire the read lock and execute the action. The lock will be released even
--  if the action raises an exception. See 'acquireRead' for more details.
withReadLock :: RWLock -> IO a -> IO a
withReadLock ls = bracket_ (acquireRead ls) (releaseRead ls)
