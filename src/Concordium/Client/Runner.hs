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
  , withClient
  , EnvData(..)
  , GrpcConfig
  , processTransaction_
  , awaitState
  ) where

import qualified Acorn.Core                          as Core
import qualified Acorn.Core.PrettyPrint              as PP
import qualified Acorn.Parser.Runner                 as PR

import           Concordium.Client.Cli
import           Concordium.Client.Config
import           Concordium.Client.Commands          as COM
import           Concordium.Client.GRPC
import           Concordium.Client.Output
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction as CT
import           Concordium.Client.Validate
import qualified Concordium.Crypto.BlockSignature    as BlockSig
import qualified Concordium.Crypto.BlsSignature      as Bls
import qualified Concordium.Crypto.Proofs            as Proofs
import qualified Concordium.Crypto.SignatureScheme   as Sig
import qualified Concordium.Crypto.VRF               as VRF
import qualified Concordium.Types.Transactions       as Types
import           Concordium.Types.HashableTo
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
import qualified Data.Char                           as C
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
import           Text.Printf

liftContext :: Monad m => PR.Context Core.UA m a -> ClientMonad (PR.Context Core.UA m) a
liftContext comp = ClientMonad {_runClientMonad = ReaderT (lift . const comp)}

liftClientIOToM :: MonadIO m => ClientIO a -> ExceptT ClientError m a
liftClientIOToM comp = do
  r <- liftIO (runClientIO comp)
  case r of
    Left err  -> throwError err
    Right res -> return res

withClient :: MonadIO m => Backend -> ClientMonad m a -> m a
withClient bkend comp = do
  r <- runExceptT $! do
    client <- liftClientIOToM (mkGrpcClient $! GrpcConfig (COM.grpcHost bkend) (COM.grpcPort bkend) (COM.grpcTarget bkend))
    ret <- ((runReaderT . _runClientMonad) comp $! client)
    liftClientIOToM (close (grpc client))
    return ret
  case r of
    Left err -> error (show err)
    Right x  -> return x

process :: COM.Options -> IO ()
process (Options (LegacyCmd c) _ backend _) = processLegacyCmd c backend
process (Options command cfgDir backend verbose) = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  -- Evaluate command.
  maybe printNoBackend p backend
  where p = case command of
              ConfigCmd c -> processConfigCmd c cfgDir
              TransactionCmd c -> processTransactionCmd c verbose cfgDir
              AccountCmd c -> processAccountCmd c verbose
              ModuleCmd c -> processModuleCmd c
              ContractCmd c -> processContractCmd c
              LegacyCmd _ -> error "Unreachable case: LegacyCmd."

processConfigCmd :: ConfigCmd -> Maybe FilePath -> Backend -> IO ()
processConfigCmd action baseCfgDir _ =
  case action of
    ConfigDump -> do
      baseCfg <- getBaseConfig baseCfgDir False
      runPrinter $ printBaseConfig baseCfg
      putStrLn ""
      accCfgs <- getAllAccountConfigs baseCfg
      runPrinter $ printAccountConfigList accCfgs

processTransactionCmd :: TransactionCmd -> Verbose -> Maybe FilePath -> Backend -> IO ()
processTransactionCmd action verbose baseCfgDir backend =
  case action of
    TransactionSubmit fname -> do
      -- TODO Ensure that the "nonce" field is optional in the payload.
      mdata <- loadContextData
      source <- BSL.readFile fname
      tx <- PR.evalContext mdata $ withClient backend $ processTransaction source defaultNetId
      printf "Transaction '%s' sent to the baker...\n" (show $ getBlockItemHash tx)
    TransactionStatus hash -> do
      validateTransactionHash hash
      status <- withClient backend $ queryTransactionStatus (read $ unpack hash)
      runPrinter $ printTransactionStatus status
    TransactionSendGtu receiver amount txCfg -> do
      baseCfg <- getBaseConfig baseCfgDir verbose

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      keysArg <- case decodeKeysArg $ COM.tcKeys txCfg of
        Nothing -> return Nothing
        Just (Left err) -> die err
        Just (Right ks) -> return $ Just ks
      accCfg <- getAccountConfig (tcSender txCfg) baseCfg Nothing keysArg

      when verbose $ do
        putStrLn ""
        runPrinter $ printAccountConfig accCfg
        putStrLn ""

      let keys = acKeys accCfg
          fromAddress = acAddr accCfg
      -- TODO Allow referencing address by name (similar to "sender")?
      toAddress <- getAddressArg "to address" $ Just receiver

      energy <- getArg "max energy amount" $ tcMaxEnergyAmount txCfg
      expiry <- getArg "expiry" $ tcExpiration txCfg
      printf "Sending %s GTU from %s to '%s'.\n" (show amount) (showNamedAddress accCfg) (show toAddress)
      printf "Allowing up to %s NRG to be spent as transaction fee.\n" (show energy)
      printf "Confirm [yN]: "
      input <- getChar
      when (C.toLower input /= 'y') $ die "Transaction cancelled."

      let t = TransactionJSON
                (TransactionJSONHeader fromAddress (tcNonce txCfg) energy expiry)
                (Transfer (Types.AddressAccount toAddress) amount)
                keys
      -- TODO Only needed because we're going through the generic
      --      processTransaction/encodeAndSignTransaction functions.
      --      Refactor to only include what's needed.
      tx <- PR.evalContext emptyContextData $ withClient backend $ processTransaction_ t defaultNetId False
      let hash = getBlockItemHash tx
      printf "Transaction sent to the baker. Waiting for transaction to be committed and finalized.\n"
      printf "You may skip this by interrupting this command (using Ctrl-C) - the transaction will still get processed\n"
      printf "and may be queried using 'transaction status %s'.\n" (show hash)

      t1 <- getFormattedLocalTimeOfDay
      printf "[%s] Waiting for the transaction to be committed..." t1
      committedStatus <- withClient backend $ awaitState 2 Committed hash
      putStrLn ""

      when (tsrState committedStatus == Absent) $ die "Transaction failed before it got committed. Most likely because it was invalid."

      runPrinter $ printTransactionStatus committedStatus

      -- If the transaction goes back to pending state after being committed to a branch which gets dropped later on,
      -- the command will currently keep presenting the transaction as committed and awaiting finalization.
      -- As the transaction is still expected to go back to being committed or (if for instance it expires) become absent,
      -- this seems acceptable from a UX perspective. Also, this is expected to be a very uncommon case.

      t2 <- getFormattedLocalTimeOfDay
      printf "[%s] Waiting for the transaction to be finalized..." t2
      finalizedStatus <- withClient backend $ awaitState 5 Finalized hash
      putStrLn ""

      when (tsrState finalizedStatus == Absent) $ die "Transaction failed after it was committed."

      -- Print out finalized status if the outcome differs from that of the committed status.
      when (tsrResults committedStatus /= tsrResults finalizedStatus) $ do
        runPrinter $ printTransactionStatus finalizedStatus

      t3 <- getFormattedLocalTimeOfDay
      printf "[%s] Transfer completed.\n" t3

-- Poll the transaction state continuously until it is "at least" the provided one.
-- Note that the "absent" state is considered the "highest" state,
-- so the loop will break if, for instance, the transaction state goes from "committed" to "absent".
awaitState :: (TransactionStatusQuery m) => Int -> TransactionState -> Types.TransactionHash -> m TransactionStatusResult
awaitState t s hash = do
  status <- queryTransactionStatus hash
  if tsrState status >= s then
    return status
  else do
    wait t
    awaitState t s hash

processAccountCmd :: AccountCmd -> Verbose -> Backend -> IO ()
processAccountCmd action verbose backend =
  case action of
    AccountShow address block -> do
      r <- withClient backend $ withBestBlockHash block (getAccountInfo address)
      v <- case r of
        Left err -> die $ "RPC error: " ++ err
        Right v -> return v
      account <- case fromJSON v of
        Error err -> die $ printf "cannot parse '%s' as JSON: %s" (show v) err
        Success a -> return a
      case account of
        Nothing -> putStrLn "Account not found."
        Just a -> runPrinter $ printAccountInfo address a verbose
    AccountList block -> do
      r <- withClient backend $ withBestBlockHash block getAccountList
      v <- case r of
             Left err -> die $ "RPC error: " ++ err
             Right v -> return v
      accountAddrs <- case fromJSON v of
                        Error err -> die $ printf "cannot parse '%s' as JSON: %s" (show v) err
                        Success as -> return as
      runPrinter $ printAccountList accountAddrs

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

-- |Helper function to specialize the type, avoiding the need for type
-- annotations in many places.
getBlockItemHash :: Types.BareBlockItem -> Types.TransactionHash
getBlockItemHash = getHash

useBackend :: LegacyCmd -> Backend -> IO ()
useBackend act b =
  case act of
    SendTransaction fname nid -> do
      mdata <- loadContextData
      source <- BSL.readFile fname
      t <- PR.evalContext mdata $ withClient b $ processTransaction source nid
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (getBlockItemHash t)
    GetConsensusInfo -> withClient b $ getConsensusStatus >>= printJSON
    GetBlockInfo every block -> withClient b $ withBestBlockHash block getBlockInfo >>= if every then loop else printJSON
    GetBlockSummary block -> withClient b $ withBestBlockHash block getBlockSummary >>= printJSON
    GetAccountList block -> withClient b $ withBestBlockHash block getAccountList >>= printJSON
    GetInstances block -> withClient b $ withBestBlockHash block getInstances >>= printJSON
    GetTransactionStatus txhash -> withClient b $ getTransactionStatus txhash >>= printJSON
    GetTransactionStatusInBlock txhash block -> withClient b $ getTransactionStatusInBlock txhash block >>= printJSON
    GetAccountInfo account block ->
      withClient b $ withBestBlockHash block (getAccountInfo account) >>= printJSON
    GetAccountNonFinalized account ->
      withClient b $ getAccountNonFinalizedTransactions account >>= printJSON
    GetNextAccountNonce account ->
      withClient b $ getNextAccountNonce account >>= printJSON
    GetInstanceInfo account block ->
      withClient b $ withBestBlockHash block (getInstanceInfo account) >>= printJSON
    GetRewardStatus block -> withClient b $ withBestBlockHash block getRewardStatus >>= printJSON
    GetBirkParameters block ->
      withClient b $ withBestBlockHash block getBirkParameters >>= printJSON
    GetModuleList block -> withClient b $ withBestBlockHash block getModuleList >>= printJSON
    GetModuleSource moduleref block -> do
      mdata <- loadContextData
      modl <-
        PR.evalContext mdata . withClient b $ withBestBlockHash block (getModuleSource moduleref)
      case modl of
        Left x ->
          print $ "Unable to get the Module from the gRPC server: " ++ show x
        Right v ->
          let s = show (PP.showModule v)
          in do
            putStrLn $ "Retrieved module " ++ show moduleref
            putStrLn s
    GetNodeInfo -> withClient b $ getNodeInfo >>= printNodeInfo
    GetBakerPrivateData -> withClient b $ getBakerPrivateData >>= printJSON
    GetPeerData bootstrapper -> withClient b $ getPeerData bootstrapper >>= printPeerData
    StartBaker -> withClient b $ startBaker >>= printSuccess
    StopBaker -> withClient b $ stopBaker >>= printSuccess
    PeerConnect ip port -> withClient b $ peerConnect ip port >>= printSuccess
    GetPeerUptime -> withClient b $ getPeerUptime >>= (liftIO . print)
    BanNode nodeId nodePort nodeIp -> withClient b $ banNode nodeId nodePort nodeIp >>= printSuccess
    UnbanNode nodeId nodePort nodeIp -> withClient b $ unbanNode nodeId nodePort nodeIp >>= printSuccess
    JoinNetwork netId -> withClient b $ joinNetwork netId >>= printSuccess
    LeaveNetwork netId -> withClient b $ leaveNetwork netId >>= printSuccess
    GetAncestors amount blockHash -> withClient b $ withBestBlockHash blockHash (getAncestors amount) >>= printJSON
    GetBranches -> withClient b $ getBranches >>= printJSON
    GetBannedPeers -> withClient b $ getBannedPeers >>= (liftIO . print)
    Shutdown -> withClient b $ shutdown >>= printSuccess
    DumpStart -> withClient b $ dumpStart >>= printSuccess
    DumpStop -> withClient b $ dumpStop >>= printSuccess
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
  (vrfPrivate, vrfVerify, signKey, verifyKey, aggrSignKey, aggrVerifyKey) <-
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
  let abAggregationVerifyKey :: Types.BakerAggregationVerifyKey = aggrVerifyKey
  let challenge = S.runPut (S.put abElectionVerifyKey <> S.put abSignatureVerifyKey <> S.put abAggregationVerifyKey <> S.put accountAddr)
  abProofElection <- Proofs.proveDlog25519VRF challenge (VRF.KeyPair vrfPrivate vrfVerify) `except` "Could not produce VRF key proof."
  abProofSig <- Proofs.proveDlog25519Block challenge (BlockSig.KeyPair signKey verifyKey) `except` "Could not produce signature key proof."
  abProofAccounts <- forM keyMap (\key -> Proofs.proveDlog25519KP challenge key `except` "Could not produce account keys proof.")
  let abProofAggregation = Bls.proveKnowledgeOfSK challenge aggrSignKey

  let out = AE.encodePretty $
          object ["transactionType" AE..= String "AddBaker",
                  "electionVerifyKey" AE..= abElectionVerifyKey,
                  "signatureVerifyKey" AE..= abSignatureVerifyKey,
                  "aggregationVerifyKey" AE..= abAggregationVerifyKey,
                  "bakerAccount" AE..= accountAddr,
                  "proofSig" AE..= abProofSig,
                  "proofElection" AE..= abProofElection,
                  "account" AE..= accountAddr,
                  "proofAccounts" AE..= Types.AccountOwnershipProof (Map.toList abProofAccounts),
                  "proofAggregation" AE..= abProofAggregation]
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
          aggrSignKey <- v .: "aggregationPrivateKey"
          aggrVerifyKey <- v .: "aggregationVerifyKey"
          return (vrfPrivate, vrfVerifyKey, signKey, verifyKey, aggrSignKey, aggrVerifyKey)

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
        putStrLn $ "    Latency: " ++ show (ps ^. CF.latency)
        putStrLn ""

      putStrLn $ "Peer type: " ++ unpack (peerList ^. CF.peerType)
      putStrLn "Peers:"
      forM_ (peerList ^. CF.peers) $ \pe -> do
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
  -> ClientMonad (PR.Context Core.UA m) Types.BareBlockItem
processTransaction source networkId =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t  -> processTransaction_ t networkId True

processTransaction_ ::
     (MonadFail m, MonadIO m)
  => TransactionJSON
  -> Int
  -> Verbose
  -> ClientMonad (PR.Context Core.UA m) Types.BareBlockItem
processTransaction_ transaction networkId _verbose = do
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
  -> ClientMonad (PR.Context Core.UA m) Types.BareBlockItem
encodeAndSignTransaction pl energy nonce expiry (sender, keys) = Types.NormalTransaction <$> do
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
    (CT.DeployEncryptionKey encKey) -> return $ Types.DeployEncryptionKey encKey
    (CT.AddBaker evk epk svk avk apk (BlockSig.KeyPair spk _) acc kp) ->
      let challenge = S.runPut (S.put evk <> S.put svk <> S.put avk <> S.put acc)
      in do
        Just proofElection <- liftIO $ Proofs.proveDlog25519VRF challenge epk
        Just proofSig <- liftIO $ Proofs.proveDlog25519KP challenge (Sig.KeyPairEd25519 spk svk)
        Just proofAccount' <- liftIO $ Proofs.proveDlog25519KP challenge kp
        let proofAccount = Types.singletonAOP proofAccount'
            proofAggregation = Bls.proveKnowledgeOfSK challenge apk
        return $ Types.AddBaker evk svk avk acc proofSig proofElection proofAccount proofAggregation
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
