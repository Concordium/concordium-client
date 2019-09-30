{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.Client.Runner
  ( process
  , getAccountNonce
  , getBestBlockHash
  , PeerData(..)
  , getPeerData
  , getNodeInfo
  , sendTransactionToBaker
  , sendHookToBaker
  , getConsensusStatus
  , getAccountInfo
  , ClientMonad(..)
  , runInClient
  , EnvData(..)
  , GrpcConfig
  ) where

import qualified Acorn.Core                          as Core
import qualified Acorn.Core.PrettyPrint              as PP
import qualified Acorn.Parser.Runner                 as PR
import           Concordium.Client.Commands          as COM hiding (networkId)
import           Concordium.Client.GRPC
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction as CT
import           Concordium.Crypto.SignatureScheme   (KeyPair (..))
import qualified Concordium.Crypto.SignatureScheme   as Sig
import qualified Concordium.ID.Account               as IDA

import           Data.ProtoLens                      (defMessage)
import           Proto.Concordium
import qualified Proto.Concordium_Fields             as CF

import qualified Concordium.Scheduler.Types          as Types

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader                hiding (fail)
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.Serialize                      as S
import qualified Data.Text.IO                        as TextIO hiding (putStrLn)
import           Lens.Simple

import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client.Exceptions

import           Data.Aeson                          as AE
import           Data.Aeson.Types                    as AE
import qualified Data.HashMap.Strict                 as Map
import           Data.Maybe
import           Data.Text
import           Data.String

import Data.Word

import           Prelude                             hiding (fail, mod, null,
                                                      unlines)
import           System.Exit                         (die)

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

liftContext :: Monad m => PR.Context Core.UA m a -> ClientMonad (PR.Context Core.UA m) a
liftContext comp = ClientMonad {_runClientMonad = ReaderT (lift . const comp)}

liftClientIO :: MonadIO m => ClientIO a -> ClientMonad m a
liftClientIO comp = ClientMonad {_runClientMonad = ReaderT (\_ -> do
                                                               r <- liftIO (runClientIO comp)
                                                               case r of
                                                                 Left err -> throwError err
                                                                 Right res -> return res
                                                           )}

liftClientIOToM :: MonadIO m => ClientIO a -> ExceptT ClientError m a
liftClientIOToM comp = do
  r <- liftIO (runClientIO comp)
  case r of
    Left err -> throwError err
    Right res -> return res

runInClient :: MonadIO m => Backend -> ClientMonad m a -> m a
runInClient bkend comp = do
  r <- runExceptT $! do
    client <- liftClientIOToM (mkGrpcClient $! GrpcConfig (COM.host bkend) (COM.port bkend) (COM.target bkend))
    ret <- ((runReaderT . _runClientMonad) comp $! EnvData client)
    liftClientIOToM (close client)
    return ret
  case r of
    Left err -> error (show err)
    Right x -> return x

-- |Execute the command given in the CLArguments
process :: Command -> IO ()
process command =
  case action command of
    LoadModule fname -> do
      mdata <- loadContextData
      cdata <-
        PR.execContext mdata $ do
          source <- liftIO $ TextIO.readFile fname
          PR.processModule source
      putStrLn
        "Module processed.\nThe following modules are currently in the local database and can be deployed.\n"
      showLocalModules cdata
      writeContextData cdata
    ListModules -> do
      mdata <- loadContextData
      putStrLn "The following modules are in the local database.\n"
      showLocalModules mdata
    -- The rest of the commands expect a backend to be provided
    act ->
      maybe (putStrLn "No Backend provided") (useBackend act) (backend command)

-- loop :: Either String [Value] -> ClientMonad IO ()
-- loop v =
--   case v of
--     Left err       -> liftIO $ putStrLn err
--     Right (AE.Object m:[]) ->
--       case Map.lookup "blockParent" m of
--         Just (String parent) -> do
--           printJSON v
--           case Map.lookup "blockSlot" m of
--             Just (AE.Number x) | x > 0 -> do
--               getBlockInfo parent >>= loop
--             _ -> liftIO $ putStrLn "Genesis block reached."
--         Nothing -> liftIO $ putStrLn "No parent."


useBackend :: Action -> Backend -> IO ()
useBackend act b =
  case act of
    SendTransaction fname nid hook -> do
      mdata <- loadContextData
      source <- BSL.readFile fname
      t <-
        PR.evalContext mdata $ runInClient b $ 
        processTransaction source nid hook
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (Types.transactionHash t)
    HookTransaction txh -> runInClient b $ hookTransaction txh >>= printJSON
    GetConsensusInfo -> runInClient b $ getConsensusStatus >>= printJSON
    GetBlockInfo block -> runInClient b $ getBlockInfo block >>= printJSON
    GetAccountList block -> runInClient b $ getAccountList block >>= printJSON
    GetInstances block -> runInClient b $ getInstances block >>= printJSON
    GetAccountInfo block account ->
      runInClient b $ getAccountInfo block account >>= printJSON
    GetInstanceInfo block account ->
      runInClient b $ getInstanceInfo block account >>= printJSON
    GetRewardStatus block -> runInClient b $ getRewardStatus block >>= printJSON
    GetBirkParameters block ->
      runInClient b $ getBirkParameters block >>= printJSON
    GetModuleList block -> runInClient b $ getModuleList block >>= printJSON
    GetModuleSource block moduleref -> do
      mdata <- loadContextData
      modl <-
        PR.evalContext mdata . runInClient b . getModuleSource block $ moduleref
      case modl of
        Left x ->
          print $ "Unable to get the Module from the gRPC server: " ++ show x
        Right v ->
          let s = show (PP.showModule v)
          in do
            putStrLn $ "Retrieved module " ++ show moduleref
            putStrLn s
    GetNodeInfo -> runInClient b $ getNodeInfo >>= printNodeInfo
    GetBakerPrivateData -> runInClient b $ getBakerPrivateData >>= printJSON
    GetPeerData bootstrapper -> runInClient b $ getPeerData bootstrapper >>= printPeerData
    _ -> undefined

printPeerData :: MonadIO m => Either String PeerData -> m ()
printPeerData epd =
  case epd of
    Left err -> liftIO $ putStrLn err
    Right PeerData{..} -> liftIO $ do
      putStrLn $ "Total packets sent: " ++ show totalSent
      putStrLn $ "Total packets received: " ++ show totalReceived
      putStrLn $ "Peer version: " ++ unpack version
      putStrLn $ "Peer stats:"
      forM_ (peerStats ^. CF.peerstats) $ \ps -> do
        putStrLn $ "  Peer: " ++ unpack (ps ^. CF.nodeId)
        putStrLn $ "    Packets sent: " ++ show (ps ^. CF.packetsSent)
        putStrLn $ "    Packets received: " ++ show (ps ^. CF.packetsReceived)
        putStrLn $ "    Measured latency: " ++ show (ps ^. CF.measuredLatency)
        putStrLn ""

      putStrLn $ "Peer type: " ++ unpack (peerList ^. CF.peerType)
      putStrLn $ "Peers:"
      forM_ (peerList ^. CF.peer) $ \pe -> do
        putStrLn $ "  Node id: " ++ unpack (pe ^. CF.nodeId ^. CF.value)
        putStrLn $ "    Port: " ++ show (pe ^. CF.port ^. CF.value)
        putStrLn $ "    IP: " ++ unpack (pe ^. CF.ip ^. CF.value)
        putStrLn ""

data PeerData = PeerData {
  totalSent :: Word64,
  totalReceived :: Word64,
  version :: Text,
  peerStats :: PeerStatsResponse,
  peerList :: PeerListResponse
  }

getPeerData :: Bool -> ClientMonad IO (Either String PeerData)
getPeerData bootstrapper = do
  totalSent' <- getPeerTotalSent
  totalReceived' <- getPeerTotalReceived
  version' <- getPeerVersion
  peerStats' <- getPeerStats bootstrapper
  peerList' <- getPeerList bootstrapper
  return $ do
    totalSent <- totalSent'
    totalReceived <- totalReceived'
    version <- version'
    peerStats <- peerStats'
    peerList <- peerList'
    return PeerData{..}


getNodeInfo :: ClientMonad IO (Either String NodeInfoResponse)
getNodeInfo = do
  client <- asks grpc
  liftClientIO $! do
    ret <- rawUnary (RPC :: RPC P2P "nodeInfo") client defMessage
    return $ (outputGRPC ret)

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

printNodeInfo :: MonadIO m => Either String NodeInfoResponse -> m ()
printNodeInfo mni =
  case mni of
    Left err -> liftIO (putStrLn err)
    Right ni -> liftIO $ do
      putStrLn $ "Node id: " ++ show (ni ^. CF.nodeId ^. CF.value)
      putStrLn $ "Current local time: " ++ show (ni ^. CF.currentLocaltime)
      putStrLn $ "Peer type: " ++ show (ni ^. CF.peerType)
      putStrLn $ "Baker running: " ++ show (ni ^. CF.consensusBakerRunning)
      putStrLn $ "Consensus running: " ++ show (ni ^. CF.consensusRunning)
      putStrLn $ "Consensus type: " ++ show (ni ^. CF.consensusType)
      putStrLn $ "Baker committee member: " ++ show (ni ^. CF.consensusBakerCommittee)
      putStrLn $ "Finalization committee member: " ++ show (ni ^. CF.consensusFinalizerCommittee)

processTransaction ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> Bool
  -> ClientMonad (PR.Context Core.UA m) Types.BareTransaction
processTransaction source networkId hookit =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t -> do
      transaction <-
        case t of
          Just transaction -> do
            nonce <-
              case thNonce . metadata $ transaction of
                Nothing ->
                  let senderAddress = IDA.accountAddress (thSenderKey (metadata transaction)) Sig.Ed25519
                  in getAccountNonce senderAddress =<< getBestBlockHash
                Just nonce -> return nonce
            encodeAndSignTransaction
              (payload transaction)
              (thGasAmount (metadata transaction))
              nonce
              (KeyPair (CT.signKey transaction) (thSenderKey (metadata transaction)))
          Nothing -> undefined
      when hookit $ do
        let trHash = Types.transactionHash transaction
        liftIO . putStrLn $ "Installing hook for transaction " ++ show trHash
        printJSON =<< sendHookToBaker trHash
      sendTransactionToBaker transaction networkId
      return transaction


getBestBlockHash :: (MonadFail m, MonadIO m) => ClientMonad m Text
getBestBlockHash = do
  getConsensusStatus >>= \case
    Left err -> fail err
    Right [] -> fail "Should not happen."
    Right (v:_) ->
      case parse readBestBlock v of
        Success bh -> return bh
        Error err -> fail err

getAccountNonce :: (MonadFail m, MonadIO m) => Types.AccountAddress -> Text -> ClientMonad m Types.Nonce
getAccountNonce addr blockhash =
  getAccountInfo blockhash (fromString (show addr)) >>= \case
    Left err -> fail err
    Right [] -> fail "Should not happen."
    Right (aval:_) ->
      case parse readAccountNonce aval of
        Success nonce -> return nonce
        Error err -> fail err

readBestBlock :: Value -> Parser Text
readBestBlock = withObject "Best block hash" $ \v -> v .: "bestBlock"

readAccountNonce :: Value -> Parser Types.Nonce
readAccountNonce = withObject "Account nonce" $ \v -> v .: "accountNonce"


readModule :: MonadIO m => FilePath -> ClientMonad m (Core.Module Core.UA)
readModule filePath = do
  source <- liftIO $ BSL.readFile filePath
  case S.decodeLazy source of
    Left err  -> liftIO (die err)
    Right mod -> return mod

encodeAndSignTransaction ::
     (MonadFail m, MonadIO m)
  => CT.TransactionJSONPayload
  -> Types.Energy
  -> Types.Nonce
  -> KeyPair
  -> ClientMonad (PR.Context Core.UA m) Types.BareTransaction
encodeAndSignTransaction pl energy nonce keys = do
  txPayload <- case pl of
    (CT.DeployModuleFromSource fileName) ->
      Types.DeployModule <$> readModule fileName -- deserializing is not necessary, but easiest for now.
    (CT.DeployModule mnameText) ->
      Types.DeployModule <$> liftContext (PR.getModule mnameText)
    (CT.InitContract initAmount mnameText cNameText paramExpr) -> do
      (mref, _, tys) <- liftContext $ PR.getModuleTmsTys mnameText
      case Map.lookup cNameText tys of
        Just contName -> do
          params <- liftContext $ PR.processTmInCtx mnameText paramExpr
          return $ Types.InitContract initAmount mref contName params
        Nothing -> error (show cNameText)
    (CT.Update mnameText updateAmount updateAddress msgText) -> do
      msg <- liftContext $ PR.processTmInCtx mnameText msgText
      return $ Types.Update updateAmount updateAddress msg
    (CT.Transfer transferTo transferAmount) ->
      return $ Types.Transfer transferTo transferAmount
    (CT.DeployCredential cred) -> return $ Types.DeployCredential cred
    (CT.DeployEncryptionKey encKey) -> return $ Types.DeployEncryptionKey encKey
    (CT.AddBaker evk svk ba p) -> return $ Types.AddBaker evk svk ba p
    (CT.RemoveBaker rbid rbp) -> return $ Types.RemoveBaker rbid rbp
    (CT.UpdateBakerAccount ubid uba ubp) ->
      return $ Types.UpdateBakerAccount ubid uba ubp
    (CT.UpdateBakerSignKey ubsid ubsk ubsp) ->
      return $ Types.UpdateBakerSignKey ubsid ubsk ubsp
    (CT.DelegateStake dsid) -> return $ Types.DelegateStake dsid

  let encPayload = Types.encodePayload txPayload
  let header = Types.makeTransactionHeader Sig.Ed25519 (verifyKey keys) (Types.payloadSize encPayload) nonce energy
  return $ Types.signTransaction keys header encPayload


sendHookToBaker ::
     (MonadIO m)
  => Types.TransactionHash
  -> ClientMonad m (Either String [Value])
sendHookToBaker txh = do
  client <- asks grpc
  ret <- liftClientIO $!
      rawUnary
        (RPC :: RPC P2P "hookTransaction")
        client
        (defMessage & CF.transactionHash .~ pack (show txh))
  return $ processJSON ret

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

hookTransaction :: Text -> ClientMonad IO (Either String [Value])
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
