{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications, ScopedTypeVariables #-}

module Concordium.Client.GRPC where

import qualified Data.ByteString.Char8               as BS8
import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client
import           Network.GRPC.HTTP2.ProtoLens

import           Data.ProtoLens(defMessage)
import           Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Field                as Field
import           Proto.ConcordiumP2pRpc
import qualified Proto.ConcordiumP2pRpc_Fields       as CF

import           Concordium.Client.Runner.Helper

import           Concordium.Client.Cli
import           Concordium.Types.Transactions(BareBlockItem, putVersionedBareBlockItemV0)
import           Concordium.Types as Types
import           Concordium.ID.Types as IDTypes

import           Control.Exception
import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Monad.Fail
import           Control.Monad.State.Strict
import           Control.Monad.Reader                hiding (fail)
import qualified Data.Serialize                      as S
import           Lens.Micro.Platform

import           Data.IORef
import           Data.Aeson as AE
import           Data.Aeson.Types as AE
import qualified Data.HashSet as Set
import qualified Data.Map.Strict as Map
import           Data.Text
import           Data.String
import           Data.Word
import           Data.Maybe
import           Text.Printf
import qualified Web.Cookie as Cookie

import           Prelude                             hiding (fail, mod, null, unlines)
import Proto.Google.Protobuf.Wrappers

type LoggerMethod = Text -> IO ()

data GrpcConfig =
  GrpcConfig
    { host   :: !HostName
    , port   :: !PortNumber
    , grpcAuthenticationToken :: !String
    -- Target node, i.e. "node-0" for use with grpc-proxy.eu.test.concordium.com against testnet
    , target :: !(Maybe String)
    -- |Number of times to __retry__ to establish a connection. Thus a value of
    -- 0 means try only once.
    , retryNum :: !Int
    -- |Timeout of each RPC call (defaults to 5min if not given).
    , timeout :: !(Maybe Int)
    -- |Whether to use TLS or not.
    , useTls :: !Bool
    }

data EnvData =
  EnvData
    {
      -- How many times to retry to establish the connection.
      -- 0 means only try once.
      retryTimes :: !Int,
      config :: !GrpcClientConfig,
      rwlock :: !RWLock,
      -- |A shared reference to a connection together with a generation counter.
      -- All queries will reuse this single connection as much as possible. This
      -- is @Nothing@ if no connection is yet established. When we reconnect we
      -- increase the generation counter. The reason for the generation counter
      -- is so that if multiple queries are in-flight at the time the connection
      -- is reset, we only reconnect once, and then retry the queries.
      grpc :: !(IORef (Maybe (Word64, GrpcClient))),
      logger :: LoggerMethod,
      -- |A flag indicating that all the in-flight queries should be killed.
      -- |This is a workaround for the inadequate behaviour of the grpc library
      -- which does not handle disconnects from the server very well, and in
      -- particular it does not handle the server sending GoAway frames. Ideally
      -- in that scenario the library would either try to reconnect itself, or,
      -- alternatively, trigger a normal error that we could recover from and
      -- re-establish the connection. None of the two happen. So instead we
      -- install our own custom GoAway handler that kills all in-flight queries,
      -- and then re-establishes the connection.
      --
      -- This MVar will be empty when queries are progressing. When the queries
      -- need to be killed then we write to it. When we successfully
      -- re-establish the connection then the the MVar is again emptied.
      killConnection :: !(MVar ())
    }

-- |Monad in which the program would run
newtype ClientMonad m a =
  ClientMonad
    { _runClientMonad :: ReaderT EnvData (ExceptT ClientError (StateT CookieHeaders m)) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader EnvData
           , MonadFail
           , MonadIO
           )

instance (MonadIO m) => TransactionStatusQuery (ClientMonad m) where
  queryTransactionStatus hash = do
    r <- getTransactionStatus (pack $ show hash)
    tx <- case r of
            Left err -> error $ "RPC error: " ++ err
            Right tx -> return tx
    case fromJSON (grpcResponseVal tx) of
      Error err -> error $ printf "cannot parse '%s' as JSON: %s" (show tx) err
      Success v -> return v
  wait t = liftIO $ do
    putChar '.'
    threadDelay $ t*1000000

liftClientIO :: MonadIO m => ClientIO a -> ClientMonad m a
liftClientIO comp = ClientMonad {_runClientMonad = ReaderT (\_ -> do
                                                               r <- liftIO (runClientIO comp)
                                                               case r of
                                                                 Left err -> throwError err
                                                                 Right res -> return res
                                                           )}

-- |Execute the computation with the given environment (using the established connection).
runClient :: Monad m => EnvData -> ClientMonad m a -> m (Either ClientError a)
runClient config comp = evalStateT (runExceptT $ runReaderT (_runClientMonad comp) config) (Map.empty :: CookieHeaders)

-- |runClient but with additional cookies added to the GRPCRequest.
-- The updated set of cookies (set via set-cookie headers)  are returned.
runClientWithCookies :: CookieHeaders -> EnvData -> ClientMonad m a -> m (Either ClientError a, CookieHeaders)
runClientWithCookies hds cfg comp = runStateT (runExceptT $ runReaderT (_runClientMonad comp) cfg) hds

mkGrpcClient :: GrpcConfig -> Maybe LoggerMethod -> ClientIO EnvData
mkGrpcClient config mLogger =
  let auth = ("authentication", BS8.pack $ grpcAuthenticationToken config)
      header =
        case target config of
          Just t  -> [auth, ("target", BS8.pack t)]
          Nothing -> [auth]
      cfg = (grpcClientConfigSimple (host config) (port config) (useTls config))
                 { _grpcClientConfigCompression = uncompressed
                 , _grpcClientConfigHeaders = header
                 , _grpcClientConfigTimeout = Timeout (fromMaybe 300 (timeout config))
                 }
   in liftIO $ do
       lock <- initializeLock
       ioref <- newIORef Nothing -- don't start the connection just now
       killConnection <- newEmptyMVar
       let logger = fromMaybe (const (return ())) mLogger
       return $! EnvData (retryNum config) cfg lock ioref logger killConnection

getNodeInfo :: ClientMonad IO (GRPCResult NodeInfoResponse)
getNodeInfo = withUnaryNoMsg' (call @"nodeInfo")

getPeerTotalSent :: ClientMonad IO (GRPCResult Word64)
getPeerTotalSent = withUnaryNoMsg (call @"peerTotalSent") getValue

getPeerTotalReceived :: ClientMonad IO (GRPCResult Word64)
getPeerTotalReceived = withUnaryNoMsg (call @"peerTotalReceived") getValue

getPeerVersion :: ClientMonad IO (GRPCResult Text)
getPeerVersion = withUnaryNoMsg (call @"peerVersion") getValue

getPeerStats :: Bool -> ClientMonad IO (GRPCResult PeerStatsResponse)
getPeerStats bootstrapper = withUnary' (call @"peerStats") msg
  where msg = defMessage & CF.includeBootstrappers .~ bootstrapper

getPeerList :: Bool -> ClientMonad IO (GRPCResult PeerListResponse)
getPeerList bootstrapper = withUnary' (call @"peerList") msg
  where msg = defMessage & CF.includeBootstrappers .~ bootstrapper

-- |Return Right True if baker successfully started,
startBaker :: ClientMonad IO (GRPCResult Bool)
startBaker = withUnaryNoMsg (call @"startBaker") getValue

-- |Return Right True if baker successfully stopped.
stopBaker :: ClientMonad IO (GRPCResult Bool)
stopBaker = withUnaryNoMsg (call @"stopBaker") getValue

sendTransactionToBaker ::
     MonadIO m => BareBlockItem -> Int -> ClientMonad m (GRPCResult Bool)
sendTransactionToBaker t nid = do
  let msg = defMessage & CF.networkId .~ fromIntegral nid & CF.payload .~ S.runPut (putVersionedBareBlockItemV0 t)
  withUnary (call @"sendTransaction") msg getValue

getTransactionStatus :: MonadIO m => Text -> ClientMonad m (GRPCResult Value)
getTransactionStatus txh = withUnary (call @"getTransactionStatus") msg getJSON
  where msg = defMessage & CF.transactionHash .~ txh

getTransactionStatusInBlock :: MonadIO m => Text -> Text -> ClientMonad m (GRPCResult Value)
getTransactionStatusInBlock txh bh = withUnary (call @"getTransactionStatusInBlock") msg getJSON
  where msg = defMessage & CF.transactionHash .~ txh & CF.blockHash .~ bh

getAccountNonFinalizedTransactions :: MonadIO m => Text -> ClientMonad m (GRPCResult Value)
getAccountNonFinalizedTransactions addr = withUnary (call @"getAccountNonFinalizedTransactions") msg getJSON
  where msg = defMessage & CF.accountAddress .~ addr

getNextAccountNonce :: MonadIO m => Text -> ClientMonad m (GRPCResult Value)
getNextAccountNonce addr = withUnary (call @"getNextAccountNonce") msg getJSON
  where msg = defMessage & CF.accountAddress .~ addr

getBlockSummary :: MonadIO m => Text -> ClientMonad m (GRPCResult Value)
getBlockSummary hash = withUnaryBlock (call @"getBlockSummary") hash getJSON

getConsensusStatus :: MonadIO m => ClientMonad m (GRPCResult Value)
getConsensusStatus = withUnaryNoMsg (call @"getConsensusStatus") getJSON

getBlockInfo :: Text -> ClientMonad IO (GRPCResult Value)
getBlockInfo hash = withUnaryBlock (call @"getBlockInfo") hash getJSON

getAccountList :: Text -> ClientMonad IO (GRPCResult Value)
getAccountList hash = withUnaryBlock (call @"getAccountList") hash getJSON

getInstances :: Text -> ClientMonad IO (GRPCResult Value)
getInstances hash = withUnaryBlock (call @"getInstances") hash getJSON

-- |Retrieve the account information from the chain.
getAccountInfo :: MonadIO m
               => Text -- ^ Account identifier, address, index or credential registration id.
               -> Text -- ^ Block hash
               -> ClientMonad m (GRPCResult Value)
getAccountInfo account hash = withUnary (call @"getAccountInfo") msg getJSON
  where msg = defMessage & CF.blockHash .~ hash & CF.address .~ account

getInstanceInfo :: Text -> Text -> ClientMonad IO (GRPCResult Value)
getInstanceInfo account hash = withUnary (call @"getInstanceInfo") msg getJSON
  where msg = defMessage & CF.blockHash .~ hash & CF.address .~ account

invokeContract :: Text -> Text -> ClientMonad IO (GRPCResult Value)
invokeContract context block = withUnary (call @"invokeContract") msg getJSON
  where msg = defMessage & CF.blockHash .~ block & CF.context .~ context

getPoolStatus :: Types.BakerId -> Bool -> Text -> ClientMonad IO (GRPCResult Value)
getPoolStatus bid passiveDelegation hash = withUnary (call @"getPoolStatus") msg getJSON
  where msg = defMessage & CF.blockHash .~ hash & CF.passiveDelegation .~ passiveDelegation & CF.bakerId .~ fromIntegral bid

getBakerList :: Text -> ClientMonad IO (GRPCResult Value)
getBakerList hash = withUnary (call @"getBakerList") msg getJSON
  where msg = defMessage & CF.blockHash .~ hash

getRewardStatus :: Text -> ClientMonad IO (GRPCResult Value)
getRewardStatus hash = withUnaryBlock (call @"getRewardStatus") hash getJSON

getBirkParameters :: Text -> ClientMonad IO (GRPCResult Value)
getBirkParameters hash = withUnaryBlock (call @"getBirkParameters") hash getJSON

getModuleList :: Text -> ClientMonad IO (GRPCResult Value)
getModuleList hash = withUnaryBlock (call @"getModuleList") hash getJSON

getModuleSource :: Text -> Text -> ClientMonad IO (GRPCResult BS8.ByteString)
getModuleSource modRef hash = withUnary (call @"getModuleSource") msg getValue
  where msg = defMessage & CF.blockHash .~ hash & CF.moduleRef .~ modRef

peerConnect :: Text -> Int -> ClientMonad IO (GRPCResult Bool)
peerConnect ip peerPort = withUnary (call @"peerConnect") msg getValue
  where msg = defMessage &
              CF.ip .~ (defMessage & CF.value .~ ip) &
              CF.port .~ (defMessage & CF.value .~ fromIntegral peerPort)

peerDisconnect :: Text -> Int -> ClientMonad IO (GRPCResult Bool)
peerDisconnect ip peerPort = withUnary (call @"peerDisconnect") msg getValue
  where msg = defMessage &
              CF.ip .~ (defMessage & CF.value .~ ip) &
              CF.port .~ (defMessage & CF.value .~ fromIntegral peerPort)

getPeerUptime :: ClientMonad IO (GRPCResult Word64)
getPeerUptime = withUnaryNoMsg (call @"peerUptime") getValue

banNode :: Maybe Text -- ^ Node ID
          -> Maybe Text -- ^ IP address
          -> ClientMonad IO (GRPCResult Bool)
banNode identifier ip = withUnary (call @"banNode") msg getValue
  where msg = defMessage &
              CF.maybe'nodeId .~ (textToStringValue <$> identifier) &
              CF.maybe'ip .~ (textToStringValue <$> ip)

unbanNode :: Maybe Text -- ^ Node ID
            -> Maybe Text -- ^ IP address
            -> ClientMonad IO (GRPCResult Bool)
unbanNode identifier ip = withUnary (call @"unbanNode") msg getValue
  where msg = defMessage &
              CF.maybe'nodeId .~ (textToStringValue <$> identifier) &
              CF.maybe'ip .~ (textToStringValue <$> ip)

textToStringValue :: Text -> StringValue
textToStringValue x = defMessage & CF.value .~ x

joinNetwork :: Int -> ClientMonad IO (GRPCResult Bool)
joinNetwork net = withUnary (call @"joinNetwork") msg getValue
    where msg = defMessage &
                CF.networkId .~ (defMessage & CF.value .~ fromIntegral net)

leaveNetwork :: Int -> ClientMonad IO (GRPCResult Bool)
leaveNetwork net = withUnary (call @"leaveNetwork") msg getValue
  where msg = defMessage &
              CF.networkId .~ (defMessage & CF.value .~ fromIntegral net)

getAncestors :: Int -> Text -> ClientMonad IO (GRPCResult Value)
getAncestors amount blockHash = withUnary (call @"getAncestors") msg getJSON
  where msg = defMessage &
              CF.blockHash .~ blockHash &
              CF.amount .~ fromIntegral amount

getBranches :: ClientMonad IO (GRPCResult Value)
getBranches = withUnaryNoMsg (call @"getBranches") getJSON

-- |Query to get the blocks at the specified height.  Optionally, a genesis index may be provided.
-- The block height is considered as relative to the genesis block at this index, or the initial
-- genesis block (index 0) otherwise.  Optionally, a boolean may be specified determining whether
-- to restrict to only blocks in the specified genesis era.  If unspecified, this defaults to
-- 'False' (by the proto3 semantics).
getBlocksAtHeight :: Types.BlockHeight -> Maybe Types.GenesisIndex -> Maybe Bool -> ClientMonad IO (GRPCResult Value)
getBlocksAtHeight height mFromGen mRestrict = withUnary (call @"getBlocksAtHeight") msg getJSON
  where msg = defMessage
              & CF.blockHeight .~ fromIntegral height
              & optionally (\fromGen -> CF.fromGenesisIndex .~ fromIntegral fromGen) mFromGen
              & optionally (CF.restrictToGenesisIndex .~) mRestrict
        optionally f v = maybe id f v

getBannedPeers :: ClientMonad IO (GRPCResult PeerListResponse)
getBannedPeers = withUnaryNoMsg' (call @"getBannedPeers")

shutdown :: ClientMonad IO (GRPCResult Bool)
shutdown = withUnaryNoMsg (call @"shutdown") getValue

dumpStart :: ClientMonad IO (GRPCResult Bool)
dumpStart = withUnaryNoMsg (call @"dumpStart") getValue

dumpStop :: ClientMonad IO (GRPCResult Bool)
dumpStop = withUnaryNoMsg (call @"dumpStop") getValue

getIdentityProviders :: MonadIO m => Text -> ClientMonad m (GRPCResult Value)
getIdentityProviders hash = withUnary (call @"getIdentityProviders") msg getJSON
  where msg = defMessage & CF.blockHash .~ hash

getAnonymityRevokers :: MonadIO m => Text -> ClientMonad m (GRPCResult Value)
getAnonymityRevokers hash = withUnary (call @"getAnonymityRevokers") msg getJSON
  where msg = defMessage & CF.blockHash .~ hash

getCryptographicParameters :: MonadIO m => Text -> ClientMonad m (GRPCResult Value)
getCryptographicParameters hash = withUnary (call @"getCryptographicParameters") msg getJSON
  where msg = defMessage & CF.blockHash .~ hash

-- |Cookie headers that may be returned by the node in a query.
type CookieHeaders = Map.Map BS8.ByteString BS8.ByteString


-- |A reader-writer lock that strongly prefer writers. More precisely this means the following
-- - readers and writers are mutually exclusive
-- - multiple readers may hold the lock at the same time if there is no writer
-- - at most one writer may hold the lock
--
-- If a writer tries to acquire a lock it will either
-- - succeed if there are no current readers or writers
-- - block after recording the intent to lock. While there are pending writers no new readers can acquire the lock.
--
-- If multiple writers are blocking on the lock they will be served in an
-- unspecified order and in principle it is possible that with heavy write
-- contention some writers would be starved. This is not the case for the
-- use-case we have.
--
-- Let ⊤ mean that the MVar is full and ⊥ that it is empty. The fields of the lock satisfy the following
-- properties.
-- - there are exactly waitingWriters threads blocking on acquireWrite
-- - rwlState == Free if and only if rwlReadLock == ⊤ and rwlWriteLock == ⊤
-- - rwlReadLock == ⊥ if and only if there is an active reader.
-- - rwlWriteLock == ⊥ if and only if there is an active writer.
--
-- Transitions between states are governed by the following transition system
-- where AW/RW and AR/RR mean acquire write, release write and acquire read,
-- release read, respectively. The WR and WW mean that the thread that
-- executed the transition is blocked waiting for rwlReadLock and rwlWriteLock MVar to be full.
-- (Free 0, ⊤, ⊤) -AR-> (ReadLocked 1 0, ⊥, ⊤)
-- (Free (n+1), ⊤, ⊤) -AR-> (Free (n+1), ⊤, ⊤)
-- (Free 0, ⊤, ⊤) -AW-> (WriteLocked 0, ⊤, ⊥)
-- (Free (n+1), ⊤, ⊤) -AW-> (WriteLocked n, ⊤, ⊥)
--
-- (ReadLocked n 0, ⊥, ⊤) -AR-> (ReadLocked (n+1) 0, ⊥, ⊤)
-- (ReadLocked n (m+1), ⊥, ⊤) -AR-> (ReadLocked n (m+1), ⊥, ⊤), WR
-- (ReadLocked n m, ⊥, ⊤) -AW-> (ReadLocked n (m+1), ⊥, ⊤), WW
-- (ReadLocked 1 m, ⊥, ⊤) -RR-> (Free m, ⊤, ⊤)
-- (ReadLocked (n+1) m, ⊥, ⊤) -RR-> (ReadLocked n m, ⊥, ⊤)
--
-- (WriteLocked n, ⊤, ⊥) -AR-> (WriteLocked n, ⊤, ⊥)
-- (WriteLocked n, ⊤, ⊥) -AW-> (WriteLocked (n+1), ⊤, ⊥), WR
-- (WriteLocked n, ⊤, ⊥) -RW-> (Free n, ⊤, ⊤), WW
--
-- No other state should be reachable.
--
-- Additionally, rwlReadLock and rwlWriteLock can only be modified while the
-- rwlState MVar is held.
data RWLock = RWLock {
  -- |The state the lock is currently in.
  rwlState :: !(MVar RWState),
  -- |An MVar used to signal threads that are waiting for all active readers to
  -- wake up. This is empty when there is at least one active reader and full
  -- otherwise.
  rwlReadLock :: !(MVar ()),
  -- |An MVar used to signal waiting readers and writers to wake up. This is
  -- empty when there is an active writer, and full otherwise. Readers wait on
  -- this MVar when there is an active writer.
  rwlWriteLock :: !(MVar ())
  }


-- |State of a reader-writer lock.
data RWState =
   -- |Nobody has acquired the lock.
  Free {
      -- |The lock is not acquired, but there might be pending writers that want to acquire it.
      waitingWriters :: !Word64
      }
  -- |There is at least one active reader.
  | ReadLocked {
      -- |The number of readers that are currently active.
      readers :: !Word64,
      -- |The number of pending writers.
      waitingWriters :: !Word64
      }
  -- |The lock is acquired by a single writer.
  | WriteLocked {
      -- |The number of writers that are pending (that is, currently blocked on this lock).
      waitingWriters :: !Word64
      }
    deriving(Show)

-- |Initialize a lock in the unlocked state.
initializeLock :: IO RWLock
initializeLock = do
  rwlState <- newMVar (Free 0)
  rwlReadLock <- newMVar ()
  rwlWriteLock <- newMVar ()
  return RWLock{..}

-- |Acquire a read lock. This will block until there are no pending writers
-- waiting to acquire the lock.
acquireRead :: RWLock -> IO ()
acquireRead RWLock{..} = mask_ go
  where
    go = takeMVar rwlState >>= \case
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

-- |Acquire a write lock. This will block when there are active readers or
-- writers. When this is operation is blocked it also blocks new readers from
-- acquiring the lock.
acquireWrite :: RWLock -> IO ()
acquireWrite RWLock{..} = mask_ $ go False
  where
    -- the boolean flag indicates whether this is a first iteration of the loop (False) or not (True)
    go alreadyWaiting = takeMVar rwlState >>= \case
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

-- |Release the write lock. The lock is assumed to be in write state, otherwise
-- this function will raise an exception.
releaseWrite :: RWLock -> IO ()
releaseWrite RWLock{..} = mask_ $
   takeMVar rwlState >>= \case
    (WriteLocked waitingWriters) -> do
      putMVar rwlWriteLock ()
      putMVar rwlState (Free waitingWriters)
    lockState -> do
      putMVar rwlState lockState
      error $ "releaseWrite: attempting to release while in state: " ++ show lockState


-- |Release the read lock. The lock is assumed to be in read state, otherwise
-- this function will raise an exception. Note that since multiple readers may
-- acquire the read lock at the same time this either decrements the read count
-- and leaves the lock in read state, or unlocks it if called when there is only
-- a single active reader.
releaseRead :: RWLock -> IO ()
releaseRead RWLock{..} = mask_ $
  takeMVar rwlState >>= \case
    (ReadLocked 1 waitingWriters) -> do
      putMVar rwlReadLock ()
      putMVar rwlState (Free waitingWriters)
    (ReadLocked n waitingWriters) -> putMVar rwlState $! ReadLocked (n - 1) waitingWriters
    lockState -> do
      putMVar rwlState lockState
      error $ "releaseRead: attempting to release read when in state: " ++ show lockState

-- |Acquire the write lock and execute the action. The lock will be released
-- even if the action raises an exception. See 'acquireWrite' for more details.
withWriteLock :: RWLock -> IO a -> IO a
withWriteLock ls = bracket_ (acquireWrite ls) (releaseWrite ls)

-- |Acquire the read lock and execute the action. The lock will be released even
-- if the action raises an exception. See 'acquireRead' for more details.
withReadLock :: RWLock -> IO a -> IO a
withReadLock ls = bracket_ (acquireRead ls) (releaseRead ls)


-- | Setup the GRPC client and run a rawUnary call with the provided message to the provided method,
-- the output is interpreted using the function given in the third parameter.
withUnaryCore :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> MethodInput P2P m
          -> (GRPCResult (MethodOutput P2P m) -> b)
          -> ClientMonad n b
withUnaryCore method message k = do
  clientRef <- asks grpc
  cfg <- asks config
  lock <- asks rwlock
  logm <- asks logger
  cookies <- ClientMonad (lift get)
  mv <- asks killConnection
  let Timeout timeoutSeconds = _grpcClientConfigTimeout cfg

  -- try to establish a connection
  let tryEstablish :: Int -> IO (Maybe GrpcClient)
      tryEstablish n = do
        logm $ "Trying to establish connection, n = " <> pack (show n)
        let cfg' = cfg { _grpcClientConfigGoAwayHandler = \x -> do
                           liftIO (logm ("Receive GOAWAY message: " <> fromString (show x)))
                           -- use tryPutMVar instead of putMVar since multiple queries might do this at the same time.
                           -- That should not matter since once an MVar is set we will kill the existing client.
                           _ <- liftIO (tryPutMVar mv ())
                           return ()
                           }
        if n <= 0 then return Nothing
        else try @IOException (runExceptT (setupGrpcClient cfg')) >>= \case
               Right (Right client) -> return (Just client)
               _ -> do -- retry in case of error or exception, after waiting 1s
                 threadDelay 1000000
                 tryEstablish (n-1)

  let tryRun =
        withReadLock lock $ do
          logm "Running gRPC query."
          mclient <- readIORef clientRef
          case mclient of
            Nothing -> do
              logm "No network client."
              -- using 0 here means that the generation check below will always
              -- yield False, in case the connection is established by another
              -- query from this point until the retry. And thus that client
              -- will be used next time.
              return (0, Nothing)
            Just (gen, client) -> do
              -- if the MVar is not set then we are free to attempt a new query.
              -- If it is set then it means a GOAWAY frame is being handled. We
              -- fail here (the Just () branch) and will try the next time after
              -- a new client has been established.
              tryTakeMVar mv >>= \case
                Nothing -> do
                  logm "Network client exists, running query."
                  -- Overwrite the headers in the client with existing ones in the config.
                  -- This makes it possible to supply per-request headers.
                  let client' = client {_grpcClientHeaders = _grpcClientConfigHeaders cfg ++ fmap (\(x, y) -> ("Cookie", x <> "=" <> y)) (Map.toList cookies)}
                  let runRPC = runExceptT (rawUnary method client' message) >>=
                               \case Left err -> Nothing <$ logm ("Network error: " <> fromString (show err))  -- client error
                                     Right (Left err) -> Nothing <$ logm ("Too much concurrency: " <> fromString (show err))
                                     Right (Right x) -> return (Just x)
                  race (race (readMVar mv) (threadDelay (timeoutSeconds * 1000000))) runRPC >>=
                     \case Left (Left ()) -> (gen, Nothing) <$ logm "Terminating query because GOAWAY received."
                           Left (Right ()) -> (gen, Nothing) <$ logm "Terminating query because it timed out."
                           Right x -> return (gen, x)
                Just () -> return (gen, Nothing) -- fail this round, go again after the client is established.

  ret <- liftIO tryRun

  case ret of
    (usedGen, Nothing) -> do -- failed, need to establish connection
      liftIO (logm "gRPC call failed. Will try to reestablish connection.")
      retryNum <- asks retryTimes
      tryAgain <- liftIO $! withWriteLock lock $ do
        reEstablish <- readIORef clientRef >>= \case
          Nothing -> return (Just 1)
          Just (curGen, oldClient) | usedGen >= curGen -> do
              void (runExceptT (close oldClient)) -- FIXME: We ignore failure closing connection here.
              return (Just (curGen + 1))
          Just _ -> return Nothing
        case reEstablish of
          Nothing -> return True
          Just newGen -> tryEstablish (retryNum + 1) >>= \case
            Nothing -> do
              atomicWriteIORef clientRef Nothing
              return False
            Just newClient -> do
              logm "Established a connection."
              atomicWriteIORef clientRef (Just (newGen, newClient))
              return True
      if tryAgain then do
          liftIO (logm "Reestablished connection, trying again.")
          (_, ret') <- liftIO tryRun
          let response = outputGRPC ret'
          addHeaders response
          return (k response)
      else return (k (Left "Cannot establish connection to GRPC endpoint."))
    (_, Just v) ->
        let response = outputGRPC' v
        in do
            addHeaders response
            return (k response)

  where addHeaders response = case response of
                Right GRPCResponse{..} -> do
                    ClientMonad $ do
                        forM_ grpcHeaders $ \(hn, hv) ->
                            when (hn == "set-cookie") $
                                let c = Cookie.parseSetCookie hv
                                in modify' (Map.insert (Cookie.setCookieName c) (Cookie.setCookieValue c))
                Left _ -> return ()


-- | Setup the GRPC client and run a rawUnary call to the provided method.
withUnaryCoreNoMsg :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> (GRPCResult (MethodOutput P2P m) -> (CIHeaderList, b))
          -> ClientMonad n (CIHeaderList, b)
withUnaryCoreNoMsg method = withUnaryCore method defMessage

-- | Call a method with a given message and use a Getter on the output.
withUnary :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> MethodInput P2P m
          -> SimpleGetter (GRPCResponse (MethodOutput P2P m)) b
          -> ClientMonad n (Either String b)
withUnary method message k = withUnaryCore method message (\x -> (^. k) <$> x)

-- | Call a method with a given message using the `id` lens on the output.
withUnary' :: forall m n. (HasMethod P2P m, MonadIO n)
           => RPC P2P m
           -> MethodInput P2P m
           -> ClientMonad n (GRPCResult (MethodOutput P2P m))
withUnary' method message = withUnary method message (to id)

-- | Call a method without a message using the given lens
withUnaryNoMsg :: forall m n b. (HasMethod P2P m, MonadIO n)
               => RPC P2P m
               -> SimpleGetter (GRPCResponse (MethodOutput P2P m)) b
               -> ClientMonad n (Either String b)
withUnaryNoMsg method = withUnary method defMessage


-- | Call a method with a message that has `blockHash` as a field and use the given lens on the output
withUnaryBlock :: forall m n b. (HasMethod P2P m,
                                   MonadIO n,
                                   Field.HasField (MethodInput P2P m) "blockHash" Text)
               => RPC P2P m
               -> Text
               -> SimpleGetter (GRPCResponse (MethodOutput P2P m)) b
               -> ClientMonad n (Either String b)
withUnaryBlock method hash = withUnary method (defMessage & CF.blockHash .~ hash)

-- | Call a method with an empty message using the `id` lens on the output.
withUnaryNoMsg' :: forall m n. (HasMethod P2P m, MonadIO n)
                => RPC P2P m
                -> ClientMonad n (GRPCResult (MethodOutput P2P m))
withUnaryNoMsg' method = withUnary' method defMessage

call :: forall m . RPC P2P m
call = RPC @P2P @m

-- *Some helper wrappers around the raw commands

getBestBlockHash :: (MonadFail m, MonadIO m) => ClientMonad m Text
getBestBlockHash =
  getConsensusStatus >>= \case
    Left err -> fail err
    Right v ->
      case parse readBestBlock (grpcResponseVal v) of
        Success bh -> return bh
        Error err  -> fail err

getLastFinalBlockHash :: (MonadFail m, MonadIO m) => ClientMonad m Text
getLastFinalBlockHash =
  getConsensusStatus >>= \case
    Left err -> fail err
    Right v ->
      case parse readLastFinalBlock (grpcResponseVal v) of
        Success bh -> return bh
        Error err  -> fail err

getAccountNonce :: (MonadFail m, MonadIO m) => Types.AccountAddress -> Text -> ClientMonad m Types.Nonce
getAccountNonce addr blockhash =
  getAccountInfo (fromString $ show addr) blockhash >>= \case
    Left err -> fail err
    Right aval ->
      case parseNullable readAccountNonce (grpcResponseVal aval) of
        Error err     -> fail err
        Success Nothing -> fail $ printf "account '%s' not found" (show addr)
        Success (Just nonce) -> return nonce

getAccountNonceBestGuess :: (MonadFail m, MonadIO m) => Types.AccountAddress -> ClientMonad m (Types.Nonce, Bool)
getAccountNonceBestGuess addr =
  getNextAccountNonce (pack (show addr)) >>= \case
    Left err -> fail err
    Right nonceObj ->
      case parse nonceGuessParser (grpcResponseVal nonceObj) of
        AE.Success p -> return p
        AE.Error s -> fail s

      where nonceGuessParser = withObject "Nonce guess return." $ \obj -> do
                nonce <- obj .: "nonce"
                allFinal <- obj .: "allFinal"
                return (nonce, allFinal)

-- |Get all the credentials on the account as a list
getAccountCredentials :: (MonadFail m, MonadIO m) => Types.AccountAddress -> ClientMonad m [IDTypes.CredentialDeploymentValues]
getAccountCredentials addr =
  withBestBlockHash Nothing (getAccountInfo (pack (show addr))) >>= \case
    Left err -> fail err
    Right v ->
      case parseNullable accountCredParser (grpcResponseVal v) of
        AE.Success p -> return (fromMaybe [] p)
        AE.Error s -> fail s
      where accountCredParser = withObject "Account credential parser" $ \obj -> obj .: "accountCredentials"


getModuleSet :: Text -> ClientMonad IO (Set.HashSet Types.ModuleRef)
getModuleSet blockhash =
  getModuleList blockhash >>= \case
    Left err -> fail err
    Right v ->
      case fromJSON (grpcResponseVal v) of
        AE.Error s -> fail s
        AE.Success xs -> return $ Set.fromList (fmap Types.ModuleRef xs)

readBestBlock :: Value -> Parser Text
readBestBlock = withObject "Best block hash" $ \v -> v .: "bestBlock"

readLastFinalBlock :: Value -> Parser Text
readLastFinalBlock = withObject "Last final hash" $ \v -> v .: "lastFinalizedBlock"

readAccountNonce :: Value -> Parser Types.Nonce
readAccountNonce = withObject "Account nonce" $ \v -> v .: "accountNonce"

parseNullable :: (Value -> Parser a) -> Value -> Result (Maybe a)
parseNullable = parse . nullable

nullable :: (Value -> Parser a) -> Value -> Parser (Maybe a)
nullable p v = case v of
                Null -> return Nothing
                _ -> Just <$> p v

withBestBlockHash :: (MonadFail m, MonadIO m) => Maybe Text -> (Text -> ClientMonad m b) -> ClientMonad m b
withBestBlockHash v c =
  case v of
    Nothing -> getBestBlockHash >>= c
    Just x -> c x

withLastFinalBlockHash :: (MonadFail m, MonadIO m) => Maybe Text -> (Text -> ClientMonad m b) -> ClientMonad m b
withLastFinalBlockHash v c =
  case v of
    Nothing -> getLastFinalBlockHash >>= c
    Just x -> c x
