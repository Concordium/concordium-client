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

import           Concordium.Client.GRPC2
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

getInstanceInfo :: (MonadIO m) => Text -> Text -> ClientMonad m (GRPCResult Value)
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

getModuleSource :: (MonadIO m) => Text -> Text -> ClientMonad m (GRPCResult BS8.ByteString)
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
              err <- try @SomeException (runExceptT (close oldClient))
              case err of
                 Left exc -> logm (fromString ("An exception occurred while closing the connection: " ++ show exc))
                 Right (Left cerr) -> logm (fromString ("An error occurred while closing the connection: " ++ show cerr))
                 Right (Right ()) -> return ()
              -- We close the connection here regardless of whether errors or
              -- exceptions occurred.
              --
              -- Clear the GOAWAY signal. Since this is happening under a write
              -- lock it will clear out the MVar since the MVar is only set
              -- under a read lock. The reason for using tryTakeMVar instead of
              -- takeMVar is that we could be in this state because a query
              -- timed out, not necessarily because we got a GOAWAY messages.
              _ <- tryTakeMVar mv
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
