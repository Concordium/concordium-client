{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Concordium.Client.Runner
  ( process
  , getAccountNonce
  , getBestBlockHash
  , PeerData(..)
  , getPeerData
  , getNodeInfo
  , sendTransactionToBaker
  , getConsensusStatus
  , getAccountInfo
  , ClientMonad(..)
  , runInClient
  , EnvData(..)
  , GrpcConfig
  , processTransaction_
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
import qualified Concordium.Crypto.Proofs            as Proofs
import qualified Concordium.Crypto.VRF               as VRF
import qualified Concordium.ID.Account               as IDA

import qualified Concordium.Scheduler.Types          as Types

import           Proto.Concordium
import qualified Proto.Concordium_Fields             as CF

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader                hiding (fail)
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.ByteString.Lazy.Char8          as BSL8
import qualified Data.Serialize                      as S
import qualified Data.Text.IO                        as TextIO hiding (putStrLn)
import           Lens.Simple

import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client.Exceptions

import           Data.Aeson                          as AE
import           Data.Aeson.Types                    as AE
import qualified Data.Aeson.Encode.Pretty            as AE
import qualified Data.HashMap.Strict                 as Map
import           Data.Maybe
import           Data.String
import           Data.Text

import           Data.Word

import           Prelude                             hiding (fail, mod, null,
                                                      unlines)
import           System.Exit                         (die)

liftContext :: Monad m => PR.Context Core.UA m a -> ClientMonad (PR.Context Core.UA m) a
liftContext comp = ClientMonad {_runClientMonad = ReaderT (lift . const comp)}

liftClientIOToM :: MonadIO m => ClientIO a -> ExceptT ClientError m a
liftClientIOToM comp = do
  r <- liftIO (runClientIO comp)
  case r of
    Left err  -> throwError err
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
    Right x  -> return x

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
    MakeBakerPayload bakerKeysFile accountKeysFile payloadFile -> handleMakeBaker bakerKeysFile accountKeysFile payloadFile
    act ->
      maybe (putStrLn "No Backend provided") (useBackend act) (backend command)

-- |Look up block infos all the way to genesis.
loop :: Either String Value -> ClientMonad IO ()
loop v =
  case v of
    Left err       -> liftIO $ putStrLn err
    Right (AE.Object m) ->
      case Map.lookup "blockParent" m of
        Just (String parent) -> do
          printJSON v
          case Map.lookup "blockSlot" m of
            Just (AE.Number x) | x > 0 -> do
              getBlockInfo parent >>= loop
            _ -> return () -- Genesis block reached.
        _ -> error "Unexpected return value for block parent."
    _ -> error "Unexptected return value for getBlockInfo."


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
    GetBlockInfo block every -> runInClient b $ getBlockInfo block >>= if every then loop else printJSON
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
    StartBaker -> runInClient b $ startBaker >>= printSuccess
    StopBaker -> runInClient b $ stopBaker >>= printSuccess
    PeerConnect ip port -> runInClient b $ peerConnect ip port >>= printSuccess
    GetPeerUptime -> runInClient b $ getPeerUptime >>= (liftIO . print)
    SendMessage nodeId netId broadcast -> runInClient b $ sendMessage nodeId netId broadcast >>= printSuccess
    SubscriptionStart -> runInClient b $ subscriptionStart >>= printSuccess
    SubscriptionStop -> runInClient b $ subscriptionStop >>= printSuccess
    SubscriptionPoll -> runInClient b $ subscriptionStart >>= (liftIO . print)
    BanNode nodeId nodePort nodeIp -> runInClient b $ banNode nodeId nodePort nodeIp >>= printSuccess
    UnbanNode nodeId nodePort nodeIp -> runInClient b $ unbanNode nodeId nodePort nodeIp >>= printSuccess
    JoinNetwork netId -> runInClient b $ joinNetwork netId >>= printSuccess
    LeaveNetwork netId -> runInClient b $ leaveNetwork netId >>= printSuccess
    GetAncestors blockHash amount -> runInClient b $ getAncestors blockHash amount >>= (liftIO . print)
    GetBranches -> runInClient b $ getBranches >>= printJSON
    GetBannedPeers -> runInClient b $ getBannedPeers >>= (liftIO . print)
    Shutdown -> runInClient b $ shutdown >>= printSuccess
    TpsTest networkId nodeId directory -> runInClient b $ tpsTest networkId nodeId directory >>= (liftIO . print)
    DumpStart -> runInClient b $ dumpStart >>= printSuccess
    DumpStop -> runInClient b $ dumpStop >>= printSuccess
    RetransmitRequest identifier elementType since networkId -> runInClient b $ retransmitRequest identifier elementType since networkId >>= printSuccess
    GetSkovStats -> runInClient b $ getSkovStats >>= (liftIO . print)
    SendTransactionPayload headerFile payloadFile nid -> runInClient b $ handleSendTransactionPayload nid headerFile payloadFile
    _ -> undefined

printSuccess :: Either String Bool -> ClientMonad IO ()
printSuccess (Left x)  = liftIO . putStrLn $ x
printSuccess (Right x) = liftIO $ if x then putStrLn "OK" else putStrLn "FAIL"

data PeerData = PeerData {
  totalSent     :: Word64,
  totalReceived :: Word64,
  version       :: Text,
  peerStats     :: PeerStatsResponse,
  peerList      :: PeerListResponse
  }
except :: IO (Maybe b) -> String -> IO b
except c err = c >>= \case
  Just x -> return x
  Nothing -> die err


handleSendTransactionPayload :: Int -> FilePath -> FilePath -> ClientMonad IO ()
handleSendTransactionPayload networkId headerFile payloadFile = do
  headerValue <- liftIO $ eitherDecodeFileStrict headerFile
  payload <- (S.decodeLazy <$> liftIO (BSL.readFile payloadFile)) >>= \case
    Left err -> liftIO $ die $ "Could not decode payload because: " ++ err
    Right p -> return p

  (scheme, signKey, verifyKey, nonce, energy) <-
    case headerValue >>= parseEither headerParser of
      Left err ->
        liftIO $ die $ "Could not decode header because: " ++ err
      Right hdr -> return hdr

  let encPayload = Types.encodePayload payload
  let header = Types.makeTransactionHeader scheme verifyKey (Types.payloadSize encPayload) nonce energy
  let tx = Types.signTransaction (Sig.KeyPair signKey verifyKey) header encPayload
  sendTransactionToBaker tx networkId >>= \case
    Left err -> liftIO $ putStrLn $ "Could not send transaction because: " ++ err
    Right False -> liftIO $ putStrLn $ "Could not send transaction."
    Right True -> liftIO $ putStrLn $ "Transaction sent."

  where headerParser = withObject "Transaction header" $ \v -> do
          verifyKey <- v .: "verifyKey"
          signKey <- v .: "signKey"
          signScheme <- v .:? "signatureScheme" .!= Sig.Ed25519
          nonce <- v .: "nonce"
          energy <- v .: "gasAmount"
          return (signScheme, signKey, verifyKey, nonce, energy)

handleMakeBaker :: FilePath -> FilePath -> Maybe FilePath -> IO ()
handleMakeBaker bakerKeysFile accountKeysFile payloadFile = do
  bakerKeysValue <- eitherDecodeFileStrict bakerKeysFile
  bakerAccountValue <- eitherDecodeFileStrict accountKeysFile
  (vrfPrivate, vrfVerify, signKey, verifyKey) <-
    case bakerKeysValue >>= parseEither bakerKeysParser of
      Left err ->
        die $ "Could not decode file with baker keys because: " ++ err
      Right keys -> return keys
  (scheme, accountSign, accountVerify) <-
    case bakerAccountValue >>= parseEither accountKeysParser of
      Left err ->
        die $ "Could not decode file with baker keys because: " ++ err
      Right keys -> return keys
  let abElectionVerifyKey = vrfVerify
  let abSignatureVerifyKey = verifyKey
  let abAccount = IDA.accountAddress accountVerify scheme
  let challenge = S.runPut (S.put abElectionVerifyKey <> S.put abSignatureVerifyKey <> S.put abAccount)
  abProofElection <- Proofs.proveDlog25519VRF challenge (VRF.KeyPair vrfPrivate vrfVerify) `except` "Could not produce VRF key proof."
  abProofSig <- Proofs.proveDlog25519KP challenge (Sig.KeyPair signKey verifyKey) `except` "Could not produce signature key proof."
  abProofAccount <- Proofs.proveDlog25519KP challenge (Sig.KeyPair accountSign accountVerify) `except` "Could not produce account keys proof."

  -- let addBaker = Types.AddBaker{..}
  let out = AE.encodePretty $
          object ["transactionType" AE..= String "AddBaker",
                  "electionVerifyKey" AE..= abElectionVerifyKey,
                  "signatureVerifyKey" AE..= abSignatureVerifyKey,
                  "bakerAccount" AE..= abAccount,
                  "proofSig" AE..= abProofSig,
                  "proofElection" AE..= abProofElection,
                  "proofAccount" AE..= abProofAccount]
  case payloadFile of
    Nothing -> BSL8.putStrLn out
    Just fileName -> BSL.writeFile fileName out

  where bakerKeysParser = withObject "Baker keys" $ \v -> do
          vrfPrivate <- v .: "electionPrivateKey"
          vrfVerifyKey <- v .: "electionVerifyKey"
          signKey <- v .: "signatureSignKey"
          verifyKey <- v .: "signatureVerifyKey"
          return (vrfPrivate, vrfVerifyKey, signKey, verifyKey)

        accountKeysParser = withObject "Account keys" $ \v -> do
          verifyKey <- v .: "verifyKey"
          signKey <- v .: "signKey"
          signScheme <- v .: "signatureScheme"
          return (signScheme, signKey, verifyKey)

printPeerData :: MonadIO m => Either String PeerData -> m ()
printPeerData epd =
  case epd of
    Left err -> liftIO $ putStrLn err
    Right PeerData{..} -> liftIO $ do
      putStrLn $ "Total packets sent: " ++ show totalSent
      putStrLn $ "Total packets received: " ++ show totalReceived
      putStrLn $ "Peer version: " ++ unpack version
      putStrLn "Peer stats:"
      forM_ (peerStats ^. CF.peerstats) $ \ps -> do
        putStrLn $ "  Peer: " ++ unpack (ps ^. CF.nodeId)
        putStrLn $ "    Packets sent: " ++ show (ps ^. CF.packetsSent)
        putStrLn $ "    Packets received: " ++ show (ps ^. CF.packetsReceived)
        putStrLn $ "    Measured latency: " ++ show (ps ^. CF.measuredLatency)
        putStrLn ""

      putStrLn $ "Peer type: " ++ unpack (peerList ^. CF.peerType)
      putStrLn "Peers:"
      forM_ (peerList ^. CF.peer) $ \pe -> do
        putStrLn $ "  Node id: " ++ unpack (pe ^. CF.nodeId . CF.value)
        putStrLn $ "    Port: " ++ show (pe ^. CF.port . CF.value)
        putStrLn $ "    IP: " ++ unpack (pe ^. CF.ip . CF.value)
        putStrLn ""

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

printNodeInfo :: MonadIO m => Either String NodeInfoResponse -> m ()
printNodeInfo mni =
  case mni of
    Left err -> liftIO (putStrLn err)
    Right ni -> liftIO $ do
      putStrLn $ "Node id: " ++ show (ni ^. CF.nodeId . CF.value)
      putStrLn $ "Current local time: " ++ show (ni ^. CF.currentLocaltime)
      putStrLn $ "Peer type: " ++ show (ni ^. CF.peerType)
      putStrLn $ "Baker running: " ++ show (ni ^. CF.consensusBakerRunning)
      putStrLn $ "Consensus running: " ++ show (ni ^. CF.consensusRunning)
      putStrLn $ "Consensus type: " ++ show (ni ^. CF.consensusType)
      putStrLn $ "Baker committee member: " ++ show (ni ^. CF.consensusBakerCommittee)
      putStrLn $ "Finalization committee member: " ++ show (ni ^. CF.consensusFinalizerCommittee)


getBestBlockHash :: (MonadFail m, MonadIO m) => ClientMonad m Text
getBestBlockHash =
  getConsensusStatus >>= \case
    Left err -> fail err
    Right v ->
      case parse readBestBlock v of
        Success bh -> return bh
        Error err  -> fail err

getAccountNonce :: (MonadFail m, MonadIO m) => Types.AccountAddress -> Text -> ClientMonad m Types.Nonce
getAccountNonce addr blockhash =
  getAccountInfo blockhash (fromString (show addr)) >>= \case
    Left err -> fail err
    Right aval ->
      case parse readAccountNonce aval of
        Success nonce -> return nonce
        Error err     -> fail err

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

processTransaction ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> Bool
  -> ClientMonad (PR.Context Core.UA m) Types.BareTransaction
processTransaction source networkId hookit =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t  -> processTransaction_ t networkId hookit

processTransaction_ ::
     (MonadFail m, MonadIO m)
  => TransactionJSON
  -> Int
  -> Bool
  -> ClientMonad (PR.Context Core.UA m) Types.BareTransaction
processTransaction_ transaction networkId hookit = do
  tx <- do
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

  when hookit $ do
    let trHash = Types.transactionHash tx
    liftIO . putStrLn $ "Installing hook for transaction " ++ show trHash
    printJSON =<< hookTransaction (pack . show $ trHash)
  sendTransactionToBaker tx networkId >>= \case
    Left err -> fail err
    Right False -> fail "Transaction not accepted by the baker."
    Right True -> return tx

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
    (CT.AddBaker evk svk ba p pe pa) -> return $ Types.AddBaker evk svk ba p pe pa
    (CT.RemoveBaker rbid rbp) -> return $ Types.RemoveBaker rbid rbp
    (CT.UpdateBakerAccount ubid uba ubp) ->
      return $ Types.UpdateBakerAccount ubid uba ubp
    (CT.UpdateBakerSignKey ubsid ubsk ubsp) ->
      return $ Types.UpdateBakerSignKey ubsid ubsk ubsp
    (CT.DelegateStake dsid) -> return $ Types.DelegateStake dsid

  let encPayload = Types.encodePayload txPayload
  let header = Types.makeTransactionHeader Sig.Ed25519 (verifyKey keys) (Types.payloadSize encPayload) nonce energy
  return $ Types.signTransaction keys header encPayload
