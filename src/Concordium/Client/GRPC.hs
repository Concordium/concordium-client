{-# LANGUAGE OverloadedStrings, BangPatterns, DataKinds, GeneralizedNewtypeDeriving #-}

module Concordium.Client.GRPC where

import qualified Data.ByteString.Char8               as BS8
import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client

import           Data.ProtoLens                      (defMessage)
import           Proto.Concordium
import qualified Proto.Concordium_Fields             as CF

import qualified Acorn.Core                          as Core
import qualified Acorn.Parser.Runner                 as PR
import           Concordium.Client.Runner.Helper

import qualified Concordium.Scheduler.Types          as Types

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader                hiding (fail)
import qualified Data.Serialize                      as S
import           Lens.Simple

import           Data.Aeson                          as AE
import           Data.ByteString                     (ByteString)
import           Data.Text
import           Data.String

import           Data.Word

import           Prelude                             hiding (fail, mod, null,
                                                      unlines)

data GrpcConfig =
  GrpcConfig
    { host   :: HostName
    , port   :: PortNumber
    -- Target node, i.e. "node-0" for use with grpc-proxy.eu.test.concordium.com against testnet
    , target :: Maybe String
    }
  deriving (Show)

newtype EnvData =
  EnvData
    { grpc :: GrpcClient
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

liftClientIO :: MonadIO m => ClientIO a -> ClientMonad m a
liftClientIO comp = ClientMonad {_runClientMonad = ReaderT (\_ -> do
                                                               r <- liftIO (runClientIO comp)
                                                               case r of
                                                                 Left err -> throwError err
                                                                 Right res -> return res
                                                           )}

mkGrpcClient :: GrpcConfig -> ClientIO GrpcClient
mkGrpcClient config =
  let auth = ("authentication", "rpcadmin")
      header =
        case target config of
          Just t  -> [auth, ("target", BS8.pack t)]
          Nothing -> [auth]
      cfg = (grpcClientConfigSimple (host config) (port config) False)
                 { _grpcClientConfigCompression = gzip
                 , _grpcClientConfigHeaders = header
                 }
   in setupGrpcClient cfg

getNodeInfo :: ClientMonad IO (Either String NodeInfoResponse)
getNodeInfo = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "nodeInfo") client defMessage
    return $! outputGRPC ret

getPeerTotalSent :: ClientMonad IO (Either String Word64)
getPeerTotalSent = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "peerTotalSent") client defMessage
    return $ ((^. CF.value) <$> outputGRPC ret)

getPeerTotalReceived :: ClientMonad IO (Either String Word64)
getPeerTotalReceived = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "peerTotalReceived") client defMessage
    return $ ((^. CF.value) <$> outputGRPC ret)

getPeerVersion :: ClientMonad IO (Either String Text)
getPeerVersion = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "peerVersion") client defMessage
    return $ ((^. CF.value) <$> outputGRPC ret)

getPeerStats :: Bool -> ClientMonad IO (Either String PeerStatsResponse)
getPeerStats bootstrapper = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "peerStats") client (defMessage & CF.includeBootstrappers .~ bootstrapper)
    return $ (outputGRPC ret)

getPeerList :: Bool -> ClientMonad IO (Either String PeerListResponse)
getPeerList bootstrapper = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "peerList") client (defMessage & CF.includeBootstrappers .~ bootstrapper)
    return $ (outputGRPC ret)

-- |Return Right True if baker successfully started,
startBaker :: ClientMonad IO (Either String Bool)
startBaker = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "startBaker") client defMessage
    return $ ((^. CF.value) <$> outputGRPC ret)

-- |Return Right True if baker successfully stopped.
stopBaker :: ClientMonad IO (Either String Bool)
stopBaker = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "stopBaker") client defMessage
    return $ ((^. CF.value) <$> outputGRPC ret)

getBakerPrivateData :: ClientMonad IO (Either String [Value])
getBakerPrivateData = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "getBakerPrivateData") client defMessage
    return $ (processJSON ret)

sendTransactionToBaker ::
     (MonadIO m) => Types.BareTransaction -> Int -> ClientMonad m ()
sendTransactionToBaker t nid = do
  client <- asks grpc
  !_ <-
    liftClientIO $!
    rawUnary
      (RPC :: RPC P2P "sendTransaction")
      client
      (defMessage & CF.networkId .~ fromIntegral nid & CF.payload .~ S.encode t)
  return ()

hookTransaction :: (MonadIO m) => Text -> ClientMonad m (Either String [Value])
hookTransaction txh = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "hookTransaction")
        client
        (defMessage & CF.transactionHash .~ txh)
    return $ processJSON ret

getConsensusStatus :: (MonadFail m, MonadIO m) => ClientMonad m (Either String [Value])
getConsensusStatus = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "getConsensusStatus") client defMessage
    return $ processJSON ret

getBlockInfo :: Text -> ClientMonad IO (Either String [Value])
getBlockInfo hash = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getBlockInfo")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getAccountList :: Text -> ClientMonad IO (Either String [Value])
getAccountList hash = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getAccountList")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getInstances :: Text -> ClientMonad IO (Either String [Value])
getInstances hash = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getInstances")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getAccountInfo :: (MonadFail m, MonadIO m) => Text -> Text -> ClientMonad m (Either String [Value])
getAccountInfo hash account = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getAccountInfo")
        client
        (defMessage & CF.blockHash .~ hash & CF.address .~ account)
    return $ processJSON ret

getInstanceInfo :: Text -> Text -> ClientMonad IO (Either String [Value])
getInstanceInfo hash account = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getInstanceInfo")
        client
        (defMessage & CF.blockHash .~ hash & CF.address .~ account)
    return $ processJSON ret

getRewardStatus :: Text -> ClientMonad IO (Either String [Value])
getRewardStatus hash = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getRewardStatus")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getBirkParameters :: Text -> ClientMonad IO (Either String [Value])
getBirkParameters hash = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getBirkParameters")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getModuleList :: Text -> ClientMonad IO (Either String [Value])
getModuleList hash = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getModuleList")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getModuleSource ::
     (MonadIO m)
  => Text
  -> Text
  -> ClientMonad (PR.Context Core.UA m) (Either String (Core.Module Core.UA))
getModuleSource hash moduleref = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getModuleSource")
        client
        (defMessage & CF.blockHash .~ hash & CF.moduleRef .~ moduleref)
    return $ S.decode (ret ^. unaryOutput . CF.payload)

peerConnect :: Text -> Int -> ClientMonad IO (Either String Bool)
peerConnect ip port = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "peerConnect")
        client
        (defMessage &
         CF.ip .~ (defMessage & CF.value .~ ip) &
         CF.port .~ (defMessage & CF.value .~ fromIntegral port))
    return $ (^. CF.value) <$> outputGRPC ret

getPeerUptime :: ClientMonad IO (Either String Word64)
getPeerUptime = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "peerUptime")
        client
        defMessage
    return $ (^. CF.value) <$> outputGRPC ret

sendMessage :: Text -> Int -> ByteString -> Bool -> ClientMonad IO (Either String Bool)
sendMessage nodeId netId message broadcast = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "sendMessage")
        client
        (defMessage &
         CF.nodeId .~ (defMessage & CF.value .~ nodeId) &
         CF.networkId .~ (defMessage & CF.value .~ fromIntegral netId) &
         CF.message .~ (defMessage & CF.value .~ message) &
         CF.broadcast .~ (defMessage & CF.value .~ broadcast))
    return $ (^. CF.value) <$> outputGRPC ret

subscriptionStart :: ClientMonad IO (Either String Bool)
subscriptionStart = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "subscriptionStart")
        client
        defMessage
    return $ (^. CF.value) <$> outputGRPC ret

subscriptionStop :: ClientMonad IO (Either String Bool)
subscriptionStop = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "subscriptionStop")
        client
        defMessage
    return $ (^. CF.value) <$> outputGRPC ret

subscriptionPoll :: ClientMonad IO (Either String P2PNetworkMessage)
subscriptionPoll = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "subscriptionPoll")
        client
        defMessage
    return $ outputGRPC ret

banNode :: Text -> Int -> Text -> ClientMonad IO (Either String Bool)
banNode id port ip = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "banNode")
        client
        (defMessage &
         CF.nodeId .~ (defMessage & CF.value .~ id) &
         CF.ip .~ (defMessage & CF.value .~ ip) &
         CF.port .~ (defMessage & CF.value .~ fromIntegral port))
    return $ (^. CF.value) <$> outputGRPC ret

unbanNode :: Text -> Int -> Text -> ClientMonad IO (Either String Bool)
unbanNode id port ip = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "unbanNode")
        client
        (defMessage &
         CF.nodeId .~ (defMessage & CF.value .~ id) &
         CF.ip .~ (defMessage & CF.value .~ ip) &
         CF.port .~ (defMessage & CF.value .~ fromIntegral port))
    return $ (^. CF.value) <$> outputGRPC ret

joinNetwork :: Int -> ClientMonad IO (Either String Bool)
joinNetwork net = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "joinNetwork")
        client
        (defMessage &
         CF.networkId .~ (defMessage & CF.value .~ fromIntegral net))
    return $ (^. CF.value) <$> outputGRPC ret

leaveNetwork :: Int -> ClientMonad IO (Either String Bool)
leaveNetwork net = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
        (RPC :: RPC P2P "leaveNetwork")
        client
        (defMessage &
         CF.networkId .~ (defMessage & CF.value .~ fromIntegral net))
    return $ ((^. CF.value) <$> outputGRPC ret)

getAncestors :: Text -> Int -> ClientMonad IO (Either String [Value])
getAncestors blockHash amount = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "getAncestors")
      client
      (defMessage &
       CF.blockHash .~ blockHash &
       CF.amount .~ fromIntegral amount)
    return $ processJSON ret

getBranches :: ClientMonad IO (Either String [Value])
getBranches = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "getBranches")
      client
      defMessage
    return $ processJSON ret

getBannedPeers :: ClientMonad IO (Either String PeerListResponse)
getBannedPeers = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "getBannedPeers")
      client
      defMessage
    return $ outputGRPC ret


shutdown :: ClientMonad IO (Either String Bool)
shutdown = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "shutdown")
      client
      defMessage
    return $ (^. CF.value) <$> outputGRPC ret

tpsTest :: Int -> Text -> Text -> ClientMonad IO (Either String Bool)
tpsTest netId id dir = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "tpsTest")
      client
      (defMessage &
       CF.networkId .~ fromIntegral netId &
       CF.id .~ id &
       CF.directory .~ dir)
    return $ (^. CF.value) <$> outputGRPC ret

dumpStart :: ClientMonad IO (Either String Bool)
dumpStart = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "dumpStart")
      client
      defMessage
    return $ (^. CF.value) <$> outputGRPC ret

dumpStop :: ClientMonad IO (Either String Bool)
dumpStop = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "dumpStop")
      client
      defMessage
    return $ (^. CF.value) <$> outputGRPC ret

retransmitRequest :: Text -> Int -> Int -> Int -> ClientMonad IO (Either String Bool)
retransmitRequest identifier elementType since networkId = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "retransmitRequest")
      client
      (defMessage &
       CF.id .~ identifier &
       CF.elementType .~ fromIntegral elementType &
       CF.since .~ (defMessage & CF.value .~ fromIntegral since) &
       CF.networkId .~ fromIntegral networkId)
    return $ (^. CF.value) <$> outputGRPC ret

getSkovStats :: ClientMonad IO (Either String GRPCSkovStats)
getSkovStats = do
  client <- asks grpc
  liftClientIO $! do
    ret <-
      rawUnary
      (RPC :: RPC P2P "getSkovStats")
      client
      defMessage
    return $ (^. CF.gsStats) <$> outputGRPC ret
