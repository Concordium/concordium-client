{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, BangPatterns, DataKinds, GeneralizedNewtypeDeriving, TypeApplications, ScopedTypeVariables #-}

module Concordium.Client.GRPC where

import qualified Data.ByteString.Char8               as BS8
import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client

import           Data.ProtoLens                      (defMessage)
import           Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Field as Field
import           Proto.ConcordiumP2pRpc
import qualified Proto.ConcordiumP2pRpc_Fields             as CF

import qualified Acorn.Core                          as Core
import qualified Acorn.Parser.Runner                 as PR
import           Concordium.Client.Runner.Helper

import           Concordium.Types.Transactions(BareTransaction)

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader                hiding (fail)
import qualified Data.Serialize                      as S
import           Lens.Simple

import           Data.Aeson                          as AE
import qualified Data.ByteString                     as BS
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
                 { _grpcClientConfigCompression = uncompressed
                 , _grpcClientConfigHeaders = header
                 }
   in setupGrpcClient cfg

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

getBakerPrivateData :: ClientMonad IO (Either String Value)
getBakerPrivateData = withUnaryNoMsg (call @"getBakerPrivateData") (to processJSON)

sendTransactionToBaker ::
     (MonadIO m) => BareTransaction -> Int -> ClientMonad m (Either String Bool)
sendTransactionToBaker t nid = do
  let msg = defMessage & CF.networkId .~ fromIntegral nid & CF.payload .~ S.encode t
  withUnary (call @"sendTransaction") msg CF.value

hookTransaction :: (MonadIO m) => Text -> ClientMonad m (Either String Value)
hookTransaction txh = withUnary (call @"hookTransaction") msg (to processJSON)
  where msg = defMessage & CF.transactionHash .~ txh

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
        k ret = ret >>= S.decode . (^. CF.payload)

peerConnect :: Text -> Int -> ClientMonad IO (Either String Bool)
peerConnect ip peerPort = withUnary (call @"peerConnect") msg CF.value
  where msg = defMessage &
              CF.ip .~ (defMessage & CF.value .~ ip) &
              CF.port .~ (defMessage & CF.value .~ fromIntegral peerPort)

getPeerUptime :: ClientMonad IO (Either String Word64)
getPeerUptime = withUnaryNoMsg (call @"peerUptime") CF.value

sendMessage :: Text -> Int -> Bool -> ClientMonad IO (Either String Bool)
sendMessage nodeId netId broadcast = do
  message <- liftIO BS.getContents
  let msg = defMessage &
            CF.nodeId .~ (defMessage & CF.value .~ nodeId) &
            CF.networkId .~ (defMessage & CF.value .~ fromIntegral netId) &
            CF.message .~ (defMessage & CF.value .~ message) &
            CF.broadcast .~ (defMessage & CF.value .~ broadcast)
  withUnary (call @"sendMessage") msg CF.value

subscriptionStart :: ClientMonad IO (Either String Bool)
subscriptionStart = withUnaryNoMsg (call @"subscriptionStart") CF.value

subscriptionStop :: ClientMonad IO (Either String Bool)
subscriptionStop = withUnaryNoMsg (call @"subscriptionStop") CF.value

subscriptionPoll :: ClientMonad IO (Either String P2PNetworkMessage)
subscriptionPoll = withUnaryNoMsg' (call @"subscriptionPoll")

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

tpsTest :: Int -> Text -> Text -> ClientMonad IO (Either String Bool)
tpsTest netId identifier dir = withUnary (call @"tpsTest") msg CF.value
    where msg = defMessage &
              CF.networkId .~ fromIntegral netId &
              CF.id .~ identifier &
              CF.directory .~ dir

dumpStart :: ClientMonad IO (Either String Bool)
dumpStart = withUnaryNoMsg (call @"dumpStart") CF.value

dumpStop :: ClientMonad IO (Either String Bool)
dumpStop = withUnaryNoMsg (call @"dumpStop") CF.value

retransmitRequest :: Text -> Int -> Int -> Int -> ClientMonad IO (Either String Bool)
retransmitRequest identifier elementType since networkId =
  withUnary (call @"retransmitRequest") msg CF.value
  where msg = defMessage &
            CF.id .~ identifier &
            CF.elementType .~ fromIntegral elementType &
            CF.since .~ (defMessage & CF.value .~ fromIntegral since) &
            CF.networkId .~ fromIntegral networkId

getSkovStats :: ClientMonad IO (Either String GRPCSkovStats)
getSkovStats = withUnaryNoMsg (call @"getSkovStats") CF.gsStats

withUnaryCore :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> MethodInput P2P m
          -> (Either String (MethodOutput P2P m) -> b)
          -> ClientMonad n b
withUnaryCore method message k = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary method client message
    return $ k (outputGRPC ret)

withUnaryCoreNoMsg :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> (Either String (MethodOutput P2P m) -> b)
          -> ClientMonad n b
withUnaryCoreNoMsg method = withUnaryCore method defMessage 

withUnary :: forall m n b. (HasMethod P2P m, MonadIO n)
          => RPC P2P m
          -> MethodInput P2P m
          -> Getter' (MethodOutput P2P m) b
          -> ClientMonad n (Either String b)
withUnary method message k = withUnaryCore method message (\x -> (^. k) <$> x)

withUnary' :: forall m n. (HasMethod P2P m, MonadIO n)
           => RPC P2P m
           -> MethodInput P2P m
           -> ClientMonad n (Either String (MethodOutput P2P m))
withUnary' method message = withUnary method message (to id)

withUnaryNoMsg :: forall m n b. (HasMethod P2P m, MonadIO n)
               => RPC P2P m
               -> Getter' (MethodOutput P2P m) b
               -> ClientMonad n (Either String b)
withUnaryNoMsg method = withUnary method defMessage


withUnaryBlock :: forall m n b. (HasMethod P2P m,
                                   MonadIO n,
                                   Field.HasField (MethodInput P2P m) "blockHash" Text)
               => RPC P2P m
               -> Text
               -> Getter' (MethodOutput P2P m) b
               -> ClientMonad n (Either String b)
withUnaryBlock method hash = withUnary method (defMessage & CF.blockHash .~ hash)


withUnaryNoMsg' :: forall m n. (HasMethod P2P m, MonadIO n)
                => RPC P2P m
                -> ClientMonad n (Either String (MethodOutput P2P m))
withUnaryNoMsg' method = withUnary' method defMessage

call :: forall m . RPC P2P m
call = RPC @P2P @m
