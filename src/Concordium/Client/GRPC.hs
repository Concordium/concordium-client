{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, DataKinds, GeneralizedNewtypeDeriving, TypeApplications, ScopedTypeVariables #-}

module Concordium.Client.GRPC where

import qualified Data.ByteString.Char8               as BS8
import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client
import           Network.GRPC.HTTP2.ProtoLens

import           Data.ProtoLens                      (defMessage)
import           Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Field                as Field
import           Proto.ConcordiumP2pRpc
import qualified Proto.ConcordiumP2pRpc_Fields       as CF

import qualified Acorn.Core                          as Core
import qualified Acorn.Parser.Runner                 as PR
import           Concordium.Client.Runner.Helper

import           Concordium.Client.Cli
import           Concordium.Types.Transactions(BareBlockItem, putVersionedBareBlockItemV0)
import           Concordium.Types as Types
import           Concordium.ID.Types as IDTypes

import           Control.Exception
import qualified Control.Concurrent.ReadWriteLock as RW
import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader                hiding (fail)
import qualified Data.Serialize                      as S
import           Lens.Micro.Platform

import           Data.IORef
import           Data.Aeson as AE
import           Data.Aeson.Types as AE
import qualified Data.HashSet as Set
import           Data.Text
import           Data.String
import           Data.Word
import           Data.Maybe
import           Text.Printf

import           Prelude                             hiding (fail, mod, null, unlines)

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
    }

data EnvData =
  EnvData
    {
      -- How many times to retry to establish the connection.
      -- 0 means only try once.
      retryTimes :: !Int,
      config :: !GrpcClientConfig,
      rwlock :: RW.RWLock,
      grpc :: !(IORef (Maybe GrpcClient)),
      logger :: LoggerMethod
    }

-- |Monad in which the program would run
newtype ClientMonad m a =
  ClientMonad
    { _runClientMonad :: ReaderT EnvData (ExceptT ClientError m) a
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
    case fromJSON tx of
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
runClient :: EnvData -> ClientMonad m a -> m (Either ClientError a)
runClient config comp = runExceptT $ runReaderT (_runClientMonad comp) config

mkGrpcClient :: GrpcConfig -> Maybe LoggerMethod -> ClientIO EnvData
mkGrpcClient config mLogger =
  let auth = ("authentication", BS8.pack $ grpcAuthenticationToken config)
      header =
        case target config of
          Just t  -> [auth, ("target", BS8.pack t)]
          Nothing -> [auth]
      cfg = (grpcClientConfigSimple (host config) (port config) False)
                 { _grpcClientConfigCompression = uncompressed
                 , _grpcClientConfigHeaders = header
                 , _grpcClientConfigTimeout = Timeout (fromMaybe 300 (timeout config))
                 }
   in liftIO $ do
       lock <- RW.new
       ioref <- newIORef Nothing -- don't start the connection just now
       let logger = fromMaybe (const (return ())) mLogger
       return $! EnvData (retryNum config) cfg lock ioref logger

getNodeInfo :: ClientMonad IO (Either String NodeInfoResponse)
getNodeInfo = withUnaryNoMsg' (call @"nodeInfo")

getPeerTotalSent :: ClientMonad IO (Either String Word64)
getPeerTotalSent = withUnaryNoMsg (call @"peerTotalSent") CF.value

getPeerTotalReceived :: ClientMonad IO (Either String Word64)
getPeerTotalReceived = withUnaryNoMsg (call @"peerTotalReceived") CF.value

getPeerVersion :: ClientMonad IO (Either String Text)
getPeerVersion = withUnaryNoMsg (call @"peerVersion") CF.value

getPeerStats :: Bool -> ClientMonad IO (Either String PeerStatsResponse)
getPeerStats bootstrapper = withUnary' (call @"peerStats") msg
  where msg = defMessage & CF.includeBootstrappers .~ bootstrapper

getPeerList :: Bool -> ClientMonad IO (Either String PeerListResponse)
getPeerList bootstrapper = withUnary' (call @"peerList") msg
  where msg = defMessage & CF.includeBootstrappers .~ bootstrapper

-- |Return Right True if baker successfully started,
startBaker :: ClientMonad IO (Either String Bool)
startBaker = withUnaryNoMsg (call @"startBaker") CF.value

-- |Return Right True if baker successfully stopped.
stopBaker :: ClientMonad IO (Either String Bool)
stopBaker = withUnaryNoMsg (call @"stopBaker") CF.value

sendTransactionToBaker ::
     (MonadIO m) => BareBlockItem -> Int -> ClientMonad m (Either String Bool)
sendTransactionToBaker t nid = do
  let msg = defMessage & CF.networkId .~ fromIntegral nid & CF.payload .~ S.runPut (putVersionedBareBlockItemV0 t)
  withUnary (call @"sendTransaction") msg CF.value

getTransactionStatus :: (MonadIO m) => Text -> ClientMonad m (Either String Value)
getTransactionStatus txh = withUnary (call @"getTransactionStatus") msg (to processJSON)
  where msg = defMessage & CF.transactionHash .~ txh

getTransactionStatusInBlock :: (MonadIO m) => Text -> Text -> ClientMonad m (Either String Value)
getTransactionStatusInBlock txh bh = withUnary (call @"getTransactionStatusInBlock") msg (to processJSON)
  where msg = defMessage & CF.transactionHash .~ txh & CF.blockHash .~ bh

getAccountNonFinalizedTransactions :: (MonadIO m) => Text -> ClientMonad m (Either String Value)
getAccountNonFinalizedTransactions addr = withUnary (call @"getAccountNonFinalizedTransactions") msg (to processJSON)
  where msg = defMessage & CF.accountAddress .~ addr

getNextAccountNonce :: (MonadIO m) => Text -> ClientMonad m (Either String Value)
getNextAccountNonce addr = withUnary (call @"getNextAccountNonce") msg (to processJSON)
  where msg = defMessage & CF.accountAddress .~ addr

getBlockSummary :: (MonadIO m) => Text -> ClientMonad m (Either String Value)
getBlockSummary hash = withUnaryBlock (call @"getBlockSummary") hash (to processJSON)

getConsensusStatus :: (MonadIO m) => ClientMonad m (Either String Value)
getConsensusStatus = withUnaryNoMsg (call @"getConsensusStatus") (to processJSON)

getBlockInfo :: Text -> ClientMonad IO (Either String Value)
getBlockInfo hash = withUnaryBlock (call @"getBlockInfo") hash (to processJSON)

getAccountList :: Text -> ClientMonad IO (Either String Value)
getAccountList hash = withUnaryBlock (call @"getAccountList") hash (to processJSON)

getInstances :: Text -> ClientMonad IO (Either String Value)
getInstances hash = withUnaryBlock (call @"getInstances") hash (to processJSON)

getAccountInfo :: (MonadIO m) => Text -> Text -> ClientMonad m (Either String Value)
getAccountInfo account hash = withUnary (call @"getAccountInfo") msg (to processJSON)
  where msg = defMessage & CF.blockHash .~ hash & CF.address .~ account

getInstanceInfo :: Text -> Text -> ClientMonad IO (Either String Value)
getInstanceInfo account hash = withUnary (call @"getInstanceInfo") msg (to processJSON)
  where msg = defMessage & CF.blockHash .~ hash & CF.address .~ account

getRewardStatus :: Text -> ClientMonad IO (Either String Value)
getRewardStatus hash = withUnaryBlock (call @"getRewardStatus") hash (to processJSON)

getBirkParameters :: Text -> ClientMonad IO (Either String Value)
getBirkParameters hash = withUnaryBlock (call @"getBirkParameters") hash (to processJSON)

getModuleList :: Text -> ClientMonad IO (Either String Value)
getModuleList hash = withUnaryBlock (call @"getModuleList") hash (to processJSON)

getModuleSource ::
     (MonadIO m)
  => Text
  -> Text
  -> ClientMonad (PR.Context Core.UA m) (Either String (Core.Module Core.UA))
getModuleSource moduleref hash = withUnaryCore (call @"getModuleSource") msg k
  where msg = defMessage
              & (CF.blockHash .~ hash)
              & CF.moduleRef .~ moduleref
        k ret = ret >>= S.decode . (^. CF.value)

peerConnect :: Text -> Int -> ClientMonad IO (Either String Bool)
peerConnect ip peerPort = withUnary (call @"peerConnect") msg CF.value
  where msg = defMessage &
              CF.ip .~ (defMessage & CF.value .~ ip) &
              CF.port .~ (defMessage & CF.value .~ fromIntegral peerPort)

getPeerUptime :: ClientMonad IO (Either String Word64)
getPeerUptime = withUnaryNoMsg (call @"peerUptime") CF.value

banNode :: Text -> Int -> Text -> ClientMonad IO (Either String Bool)
banNode identifier peerPort ip = withUnary (call @"banNode") msg CF.value
  where msg = defMessage &
              CF.nodeId .~ (defMessage & CF.value .~ identifier) &
              CF.ip .~ (defMessage & CF.value .~ ip) &
              CF.port .~ (defMessage & CF.value .~ fromIntegral peerPort)

unbanNode :: Text -> Int -> Text -> ClientMonad IO (Either String Bool)
unbanNode identifier peerPort ip = withUnary (call @"unbanNode") msg CF.value
  where msg = defMessage &
              CF.nodeId .~ (defMessage & CF.value .~ identifier) &
              CF.ip .~ (defMessage & CF.value .~ ip) &
              CF.port .~ (defMessage & CF.value .~ fromIntegral peerPort)

joinNetwork :: Int -> ClientMonad IO (Either String Bool)
joinNetwork net = withUnary (call @"joinNetwork") msg CF.value
    where msg = defMessage &
                CF.networkId .~ (defMessage & CF.value .~ fromIntegral net)

leaveNetwork :: Int -> ClientMonad IO (Either String Bool)
leaveNetwork net = withUnary (call @"leaveNetwork") msg CF.value
  where msg = defMessage &
              CF.networkId .~ (defMessage & CF.value .~ fromIntegral net)

getAncestors :: Int -> Text -> ClientMonad IO (Either String Value)
getAncestors amount blockHash = withUnary (call @"getAncestors") msg (to processJSON)
  where msg = defMessage &
            CF.blockHash .~ blockHash &
            CF.amount .~ fromIntegral amount

getBranches :: ClientMonad IO (Either String Value)
getBranches = withUnaryNoMsg (call @"getBranches") (to processJSON)

getBannedPeers :: ClientMonad IO (Either String PeerListResponse)
getBannedPeers = withUnaryNoMsg' (call @"getBannedPeers")

shutdown :: ClientMonad IO (Either String Bool)
shutdown = withUnaryNoMsg (call @"shutdown") CF.value

dumpStart :: ClientMonad IO (Either String Bool)
dumpStart = withUnaryNoMsg (call @"dumpStart") CF.value

dumpStop :: ClientMonad IO (Either String Bool)
dumpStop = withUnaryNoMsg (call @"dumpStop") CF.value

getIdentityProviders :: (MonadIO m) => Text -> ClientMonad m (Either String Value)
getIdentityProviders hash = withUnary (call @"getIdentityProviders") msg (to processJSON)
  where msg = defMessage & CF.blockHash .~ hash

getAnonymityRevokers :: (MonadIO m) => Text -> ClientMonad m (Either String Value)
getAnonymityRevokers hash = withUnary (call @"getAnonymityRevokers") msg (to processJSON)
  where msg = defMessage & CF.blockHash .~ hash

-- | Setup the GRPC client and run a rawUnary call with the provided message to the provided method,
-- the output is interpreted using the function given in the third parameter.
withUnaryCore :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> MethodInput P2P m
          -> (Either String (MethodOutput P2P m) -> b)
          -> ClientMonad n b
withUnaryCore method message k = do
  clientRef <- asks grpc
  cfg <- asks config
  lock <- asks rwlock
  logm <- asks logger
  let Timeout timeoutSeconds = _grpcClientConfigTimeout cfg

  -- try to establish a connection
  let tryEstablish :: Int -> IO (Maybe GrpcClient)
      tryEstablish n = do
        logm $ "Trying to establish connection, n = " <> pack (show n)
        if n <= 0 then return Nothing
        else try @ IOException (runExceptT (setupGrpcClient cfg)) >>= \case
               Right (Right client) -> return (Just client)
               _ -> do -- retry in case of error or exception, after waiting 1s
                 threadDelay 1000000
                 tryEstablish (n-1)

  let tryRun =
        RW.withRead lock $ do
          logm "Running gRPC query."
          mclient <- readIORef clientRef
          case mclient of
            Nothing -> do
              logm "No network client."
              return Nothing
            Just client -> do
              logm "Network client exists, running query."
              let runRPC = runExceptT (rawUnary method client message) >>=
                           \case Left _ -> return Nothing -- client error
                                 Right (Left _) -> return Nothing -- too much concurrency
                                 Right (Right x) -> return (Just x)
              race (threadDelay (timeoutSeconds * 1000000)) runRPC >>=
                 \case Left () -> Nothing <$ logm "Timeout out."
                       Right x -> return x

  ret <- liftIO tryRun

  case ret of
    Nothing -> do -- failed, need to establish connection
      liftIO (logm "gRPC call failed. Will try to reestablish connection.")
      retryNum <- asks retryTimes
      tryAgain <- liftIO $ RW.withWrite lock $ do
        readIORef clientRef >>= \case
          Nothing -> return ()
          Just oldClient -> void (runExceptT (close oldClient)) -- FIXME: We ignore failure closing connection here.
        tryEstablish (retryNum + 1) >>= \case
          Nothing -> do
            atomicWriteIORef clientRef Nothing
            return False
          Just newClient -> do
            logm "Established a connection."
            atomicWriteIORef clientRef (Just newClient)
            return True
      if tryAgain then do
          liftIO (logm "Reestablished connection, trying again.")
          ret' <- liftIO tryRun
          return (k (outputGRPC ret'))
      else return (k (Left "Cannot establish connection to GRPC endpoint."))
    Just v -> return (k (outputGRPC' v))

-- | Setup the GRPC client and run a rawUnary call to the provided method.
withUnaryCoreNoMsg :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> (Either String (MethodOutput P2P m) -> b)
          -> ClientMonad n b
withUnaryCoreNoMsg method = withUnaryCore method defMessage

-- | Call a method with a given message and use a Getter on the output.
withUnary :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> MethodInput P2P m
          -> SimpleGetter (MethodOutput P2P m) b
          -> ClientMonad n (Either String b)
withUnary method message k = withUnaryCore method message (\x -> (^. k) <$> x)

-- | Call a method with a given message using the `id` lens on the output.
withUnary' :: forall m n. (HasMethod P2P m, MonadIO n)
           => RPC P2P m
           -> MethodInput P2P m
           -> ClientMonad n (Either String (MethodOutput P2P m))
withUnary' method message = withUnary method message (to id)

-- | Call a method without a message using the given lens
withUnaryNoMsg :: forall m n b. (HasMethod P2P m, MonadIO n)
               => RPC P2P m
               -> SimpleGetter (MethodOutput P2P m) b
               -> ClientMonad n (Either String b)
withUnaryNoMsg method = withUnary method defMessage


-- | Call a method with a message that has `blockHash` as a field and use the given lens on the output
withUnaryBlock :: forall m n b. (HasMethod P2P m,
                                   MonadIO n,
                                   Field.HasField (MethodInput P2P m) "blockHash" Text)
               => RPC P2P m
               -> Text
               -> SimpleGetter (MethodOutput P2P m) b
               -> ClientMonad n (Either String b)
withUnaryBlock method hash = withUnary method (defMessage & CF.blockHash .~ hash)

-- | Call a method with an empty message using the `id` lens on the output.
withUnaryNoMsg' :: forall m n. (HasMethod P2P m, MonadIO n)
                => RPC P2P m
                -> ClientMonad n (Either String (MethodOutput P2P m))
withUnaryNoMsg' method = withUnary' method defMessage

call :: forall m . RPC P2P m
call = RPC @P2P @m

-- *Some helper wrappers around the raw commands

getBestBlockHash :: (MonadFail m, MonadIO m) => ClientMonad m Text
getBestBlockHash =
  getConsensusStatus >>= \case
    Left err -> fail err
    Right v ->
      case parse readBestBlock v of
        Success bh -> return bh
        Error err  -> fail err

getLastFinalBlockHash :: (MonadFail m, MonadIO m) => ClientMonad m Text
getLastFinalBlockHash =
  getConsensusStatus >>= \case
    Left err -> fail err
    Right v ->
      case parse readLastFinalBlock v of
        Success bh -> return bh
        Error err  -> fail err

getAccountNonce :: (MonadFail m, MonadIO m) => Types.AccountAddress -> Text -> ClientMonad m Types.Nonce
getAccountNonce addr blockhash =
  getAccountInfo (fromString $ show addr) blockhash >>= \case
    Left err -> fail err
    Right aval ->
      case parseNullable readAccountNonce aval of
        Error err     -> fail err
        Success Nothing -> fail $ printf "account '%s' not found" (show addr)
        Success (Just nonce) -> return nonce

getAccountNonceBestGuess :: (MonadFail m, MonadIO m) => Types.AccountAddress -> ClientMonad m (Types.Nonce, Bool)
getAccountNonceBestGuess addr =
  getNextAccountNonce (pack (show addr)) >>= \case
    Left err -> fail err
    Right nonceObj ->
      case parse nonceGuessParser nonceObj of
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
      case parseNullable accountCredParser v of
        AE.Success p -> return (fromMaybe [] p)
        AE.Error s -> fail s
      where accountCredParser = withObject "Account credential parser" $ \obj -> obj .: "accountCredentials"


getModuleSet :: Text -> ClientMonad IO (Set.HashSet Types.ModuleRef)
getModuleSet blockhash =
  getModuleList blockhash >>= \case
    Left err -> fail err
    Right v ->
      case fromJSON v of
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

withBestBlockHash :: (MonadIO m, MonadFail m) => Maybe Text -> (Text -> ClientMonad m b) -> ClientMonad m b
withBestBlockHash v c =
  case v of
    Nothing -> getBestBlockHash >>= c
    Just x -> c x

withLastFinalBlockHash :: (MonadIO m, MonadFail m) => Maybe Text -> (Text -> ClientMonad m b) -> ClientMonad m b
withLastFinalBlockHash v c =
  case v of
    Nothing -> getLastFinalBlockHash >>= c
    Just x -> c x
