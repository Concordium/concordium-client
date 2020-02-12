{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Concordium.Client.Runner
  ( process
  , getAccountNonce
  , getBestBlockHash
  , getLastFinalBlockHash
  , PeerData(..)
  , getPeerData
  , getNodeInfo
  , sendTransactionToBaker
  , getConsensusStatus
  , getAccountInfo
  , getModuleSet
  , ClientMonad(..)
  , runInClient
  , EnvData(..)
  , GrpcConfig
  , processTransaction_
  ) where

import qualified Acorn.Core                          as Core
import qualified Acorn.Core.PrettyPrint              as PP
import qualified Acorn.Parser.Runner                 as PR

import           Concordium.Client.Commands          as COM
import           Concordium.Client.GRPC
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction as CT
import qualified Concordium.Crypto.BlockSignature    as BlockSig
import qualified Concordium.Crypto.BlsSignature      as Bls
import qualified Concordium.Crypto.Proofs            as Proofs
import qualified Concordium.Crypto.VRF               as VRF
import qualified Concordium.Types.Transactions       as Types
import qualified Concordium.Types.Execution          as Types
import qualified Concordium.Types                    as Types

import           Proto.ConcordiumP2pRpc
import qualified Proto.ConcordiumP2pRpc_Fields       as CF

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader                hiding (fail)
import           Data.Aeson                          as AE
import           Data.Aeson.Types                    as AE
import qualified Data.Aeson.Encode.Pretty            as AE
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.ByteString.Lazy.Char8          as BSL8
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict                 as Map
import           Data.Maybe
import qualified Data.Serialize                      as S
import           Data.String
import           Data.Text
import qualified Data.Text.IO                        as TextIO hiding (putStrLn)
import           Data.Word
import           Lens.Simple
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client.Exceptions
import           Prelude                             hiding (fail, mod, null, unlines)
import           System.Exit                         (die)
import           System.IO

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
process :: COM.Options -> IO ()
process (Options (LegacyCmd c) backend) = processLegacyCmd c backend
process (Options command backend) = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  -- Evaluate command.
  maybe printNoBackend p backend
  where p = case command of
              TransactionCmd c -> processTransactionCmd c
              AccountCmd c -> processAccountCmd c
              ModuleCmd c -> processModuleCmd c
              ContractCmd c -> processContractCmd c
              LegacyCmd _ -> error "Unreachable case: LegacyCmd."

processTransactionCmd :: TransactionCmd -> Backend -> IO ()
processTransactionCmd action backend = putStrLn $ "Not yet implemented: " ++ show action ++ ", " ++ show backend

processAccountCmd :: AccountCmd -> Backend -> IO ()
processAccountCmd action backend = putStrLn $ "Not yet implemented: " ++ show action ++ ", " ++ show backend

processModuleCmd :: ModuleCmd -> Backend -> IO ()
processModuleCmd action backend = putStrLn $ "Not yet implemented: " ++ show action ++ ", " ++ show backend

processContractCmd :: ContractCmd -> Backend -> IO ()
processContractCmd action backend = putStrLn $ "Not yet implemented: " ++ show action ++ ", " ++ show backend

processLegacyCmd :: LegacyCmd -> Maybe Backend -> IO ()
processLegacyCmd action backend =
  case action of
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
      maybe printNoBackend (useBackend act) backend

printNoBackend :: IO ()
printNoBackend = putStrLn "No Backend provided"

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

withBestBlockHash :: (MonadIO m, MonadFail m) => Maybe Text -> (Text -> ClientMonad m b) -> ClientMonad m b
withBestBlockHash v c =
  case v of
    Nothing -> getBestBlockHash >>= c
    Just x -> c x

useBackend :: LegacyCmd -> Backend -> IO ()
useBackend act b =
  case act of
    SendTransaction fname nid hook -> do
      mdata <- loadContextData
      source <- BSL.readFile fname
      t <- PR.evalContext mdata $ runInClient b $ processTransaction source nid hook
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (Types.transactionHash t)
    HookTransaction txh -> runInClient b $ hookTransaction txh >>= printJSON
    GetConsensusInfo -> runInClient b $ getConsensusStatus >>= printJSON
    GetBlockInfo every block -> runInClient b $ withBestBlockHash block getBlockInfo >>= if every then loop else printJSON
    GetAccountList block -> runInClient b $ withBestBlockHash block getAccountList >>= printJSON
    GetInstances block -> runInClient b $ withBestBlockHash block getInstances >>= printJSON
    GetAccountInfo account block ->
      runInClient b $ withBestBlockHash block (getAccountInfo account) >>= printJSON
    GetInstanceInfo account block ->
      runInClient b $ withBestBlockHash block (getInstanceInfo account) >>= printJSON
    GetRewardStatus block -> runInClient b $ withBestBlockHash block getRewardStatus >>= printJSON
    GetBirkParameters block ->
      runInClient b $ withBestBlockHash block getBirkParameters >>= printJSON
    GetModuleList block -> runInClient b $ withBestBlockHash block getModuleList >>= printJSON
    GetModuleSource moduleref block -> do
      mdata <- loadContextData
      modl <-
        PR.evalContext mdata . runInClient b $ withBestBlockHash block (getModuleSource moduleref)
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
    BanNode nodeId nodePort nodeIp -> runInClient b $ banNode nodeId nodePort nodeIp >>= printSuccess
    UnbanNode nodeId nodePort nodeIp -> runInClient b $ unbanNode nodeId nodePort nodeIp >>= printSuccess
    JoinNetwork netId -> runInClient b $ joinNetwork netId >>= printSuccess
    LeaveNetwork netId -> runInClient b $ leaveNetwork netId >>= printSuccess
    GetAncestors amount blockHash -> runInClient b $ withBestBlockHash blockHash (getAncestors amount) >>= printJSON
    GetBranches -> runInClient b $ getBranches >>= printJSON
    GetBannedPeers -> runInClient b $ getBannedPeers >>= (liftIO . print)
    Shutdown -> runInClient b $ shutdown >>= printSuccess
    TpsTest networkId nodeId directory -> runInClient b $ tpsTest networkId nodeId directory >>= (liftIO . print)
    DumpStart -> runInClient b $ dumpStart >>= printSuccess
    DumpStop -> runInClient b $ dumpStop >>= printSuccess
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

handleMakeBaker :: FilePath -> FilePath -> Maybe FilePath -> IO ()
handleMakeBaker bakerKeysFile accountKeysFile payloadFile = do
  bakerKeysValue <- eitherDecodeFileStrict bakerKeysFile
  bakerAccountValue <- eitherDecodeFileStrict accountKeysFile
  (vrfPrivate, vrfVerify, signKey, verifyKey, aggrVerifyKey :: Bls.PublicKey) <-
    case bakerKeysValue >>= parseEither bakerKeysParser of
      Left err ->
        die $ "Could not decode file with baker keys because: " ++ err
      Right keys -> return keys
  (accountAddr :: Types.AccountAddress, keyMap) <-
    case bakerAccountValue >>= parseEither accountKeysParser of
      Left err ->
        die $ "Could not decode file with account keys because: " ++ err
      Right addrKeys -> return addrKeys
  let abElectionVerifyKey = vrfVerify
  let abSignatureVerifyKey = verifyKey
  let challenge = S.runPut (S.put abElectionVerifyKey <> S.put abSignatureVerifyKey <> S.put accountAddr)
  abProofElection <- Proofs.proveDlog25519VRF challenge (VRF.KeyPair vrfPrivate vrfVerify) `except` "Could not produce VRF key proof."
  abProofSig <- Proofs.proveDlog25519Block challenge (BlockSig.KeyPair signKey verifyKey) `except` "Could not produce signature key proof."
  abProofAccounts <- forM keyMap (\key -> Proofs.proveDlog25519KP challenge key `except` "Could not produce account keys proof.")

  let out = AE.encodePretty $
          object ["transactionType" AE..= String "AddBaker",
                  "electionVerifyKey" AE..= abElectionVerifyKey,
                  "signatureVerifyKey" AE..= abSignatureVerifyKey,
                  "aggregationVerifyKey" AE..= aggrVerifyKey,
                  "bakerAccount" AE..= accountAddr,
                  "proofSig" AE..= abProofSig,
                  "proofElection" AE..= abProofElection,
                  "account" AE..= accountAddr,
                  "proofAccounts" AE..= Types.AccountOwnershipProof (Map.toList abProofAccounts)]
  case payloadFile of
    Nothing -> BSL8.putStrLn out
    Just fileName -> BSL.writeFile fileName out

  where accountKeysParser = withObject "Account keys" $ \v -> do
          accountAddr <- v .: "account"
          keyMap <- v .: "keys"
          return (accountAddr, keyMap)
        bakerKeysParser = withObject "Baker keys" $ \v -> do
          vrfPrivate <- v .: "electionPrivateKey"
          vrfVerifyKey <- v .: "electionVerifyKey"
          signKey <- v .: "signatureSignKey"
          verifyKey <- v .: "signatureVerifyKey"
          aggrVerifyKey <- v .: "aggregationVerifyKey"
          return (vrfPrivate, vrfVerifyKey, signKey, verifyKey, aggrVerifyKey)

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
  getAccountInfo (fromString (show addr)) blockhash >>= \case
    Left err -> fail err
    Right aval ->
      case parse readAccountNonce aval of
        Success nonce -> return nonce
        Error err     -> fail err

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
    Right t  -> processTransaction_ t networkId hookit True

processTransaction_ ::
     (MonadFail m, MonadIO m)
  => TransactionJSON
  -> Int
  -> Bool
  -> Bool
  -> ClientMonad (PR.Context Core.UA m) Types.BareTransaction
processTransaction_ transaction networkId hookit verbose = do
  tx <- do
    let header = metadata transaction
        sender = thSenderAddress header
    nonce <-
      case thNonce header of
        Nothing ->  getBestBlockHash >>= getAccountNonce sender
        Just nonce -> return nonce
    encodeAndSignTransaction
      (payload transaction)
      (thEnergyAmount header)
      nonce
      (thExpiry header)
      (sender, (CT.keys transaction))

  when hookit $ do
    let trHash = Types.transactionHash tx
    when verbose $
      liftIO . putStrLn $ "Installing hook for transaction " ++ show trHash

    h <- hookTransaction (pack . show $ trHash)
    when verbose $ printJSON h
  sendTransactionToBaker tx networkId >>= \case
    Left err -> fail err
    Right False -> fail "Transaction not accepted by the baker."
    Right True -> return tx

encodeAndSignTransaction ::
     (MonadFail m, MonadIO m)
  => CT.TransactionJSONPayload
  -> Types.Energy
  -> Types.Nonce
  -> Types.TransactionExpiryTime
  -> CT.SenderData
  -> ClientMonad (PR.Context Core.UA m) Types.BareTransaction
encodeAndSignTransaction pl energy nonce expiry (sender, keys) = do
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
    (CT.AddBaker evk svk avk ba p pe pa) -> return $ Types.AddBaker evk svk avk ba p pe pa
    (CT.RemoveBaker rbid rbp) -> return $ Types.RemoveBaker rbid rbp
    (CT.UpdateBakerAccount ubid uba ubp) ->
      return $ Types.UpdateBakerAccount ubid uba ubp
    (CT.UpdateBakerSignKey ubsid ubsk ubsp) ->
      return $ Types.UpdateBakerSignKey ubsid ubsk ubsp
    (CT.DelegateStake dsid) -> return $ Types.DelegateStake dsid

  let encPayload = Types.encodePayload txPayload
      header = Types.TransactionHeader{
        thSender = sender,
        thPayloadSize = Types.payloadSize encPayload,
        thNonce = nonce,
        thEnergyAmount = energy,
        thExpiry = expiry
  }
  return $ Types.signTransaction (Map.toList keys) header encPayload
