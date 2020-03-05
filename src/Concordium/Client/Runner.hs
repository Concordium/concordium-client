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
  , awaitState
  ) where

import qualified Acorn.Core                          as Core
import qualified Acorn.Core.PrettyPrint              as PP
import qualified Acorn.Parser.Runner                 as PR

import           Concordium.Client.Cli
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
import           Text.Printf

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

process :: COM.Options -> IO ()
process (Options (LegacyCmd c) backend _) = processLegacyCmd c backend
process (Options command backend verbose) = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  -- Evaluate command.
  maybe printNoBackend p backend
  where p = case command of
              TransactionCmd c -> processTransactionCmd c
              AccountCmd c -> processAccountCmd c verbose
              ModuleCmd c -> processModuleCmd c
              ContractCmd c -> processContractCmd c
              LegacyCmd _ -> error "Unreachable case: LegacyCmd."

processTransactionCmd :: TransactionCmd -> Backend -> IO ()
processTransactionCmd action backend =
  case action of
    TransactionSubmit fname -> do
      mdata <- loadContextData
      source <- BSL.readFile fname
      tx <- PR.evalContext mdata $ runInClient backend $ processTransaction source defaultNetId
      printf "Transaction '%s' sent to the baker...\n" (show $ Types.transactionHash tx)
    TransactionStatus hash -> do
      validateTransactionHash hash
      status <- runInClient backend $ queryTransactionStatus (read $ unpack hash)
      runPrinter $ printTransactionStatus status
    TransactionSendGtu receiver amount cfg -> do
      toAddress <- getAddressArg "to address" $ Just receiver
      fromAddress <- getAddressArg "from address" $ sender cfg
      energy <- getArg "max energy amount" $ maxEnergyAmount cfg
      expiration <- getArg "expiration" $ expiration cfg
      keys <- getKeysArg $ COM.keys cfg
      printf "Sending %s GTU from '%s' to '%s'.\n" (show amount) (show fromAddress) (show toAddress)
      printf "Allowing up to %s NRG to be spent as transaction fee.\n" (show energy)
      printf "Confirm [yN]: "
      input <- getChar
      when (C.toLower input /= 'y') $ die "Transaction cancelled."

      let t = TransactionJSON
                (TransactionJSONHeader fromAddress (nonce cfg) energy expiration)
                (Transfer (Types.AddressAccount toAddress) amount)
                keys
      -- TODO Only needed because we're going through the generic
      --      processTransaction/encodeAndSignTransaction functions.
      --      Refactor to only include what's needed.
      tx <- PR.evalContext emptyContextData $ runInClient backend $ processTransaction_ t defaultNetId False
      let hash = Types.transactionHash tx
      printf "Transaction sent to the baker. Waiting for transaction to be committed and finalized.\n"
      printf "You may skip this by interrupting this command (using Ctrl-C) - the transaction will still get processed\n"
      printf "and may be queried using 'transaction status %s'.\n" (show hash)

      t1 <- getFormattedLocalTimeOfDay
      printf "[%s] Waiting for the transaction to be committed..." t1
      committedStatus <- runInClient backend $ awaitState 2 Committed hash
      putStrLn ""

      when (tsrState committedStatus == Absent) $ die "Transaction failed before it got committed. Most likely because it was invalid."

      runPrinter $ printTransactionStatus committedStatus

      -- If the transaction goes back to pending state after being committed to a branch which gets dropped later on,
      -- the command will currently keep presenting the transaction as committed and awaiting finalization.
      -- As the transaction is still expected to go back to being committed or (if for instance it expires) become absent,
      -- this seems acceptable from a UX perspective. Also, this is expected to be a very uncommon case.

      t2 <- getFormattedLocalTimeOfDay
      printf "[%s] Waiting for the transaction to be finalized..." t2
      finalizedStatus <- runInClient backend $ awaitState 5 Finalized hash
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
      r <- runInClient backend $ withBestBlockHash block (getAccountInfo address)
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
      r <- runInClient backend $ withBestBlockHash block getAccountList
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

withBestBlockHash :: (MonadIO m, MonadFail m) => Maybe Text -> (Text -> ClientMonad m b) -> ClientMonad m b
withBestBlockHash v c =
  case v of
    Nothing -> getBestBlockHash >>= c
    Just x -> c x

useBackend :: LegacyCmd -> Backend -> IO ()
useBackend act b =
  case act of
    SendTransaction fname nid -> do
      mdata <- loadContextData
      source <- BSL.readFile fname
      t <- PR.evalContext mdata $ runInClient b $ processTransaction source nid
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (Types.transactionHash t)
    GetConsensusInfo -> runInClient b $ getConsensusStatus >>= printJSON
    GetBlockInfo every block -> runInClient b $ withBestBlockHash block getBlockInfo >>= if every then loop else printJSON
    GetBlockSummary block -> runInClient b $ withBestBlockHash block getBlockSummary >>= printJSON
    GetAccountList block -> runInClient b $ withBestBlockHash block getAccountList >>= printJSON
    GetInstances block -> runInClient b $ withBestBlockHash block getInstances >>= printJSON
    GetTransactionStatus txhash -> runInClient b $ getTransactionStatus txhash >>= printJSON
    GetTransactionStatusInBlock txhash block -> runInClient b $ getTransactionStatusInBlock txhash block >>= printJSON
    GetAccountInfo account block ->
      runInClient b $ withBestBlockHash block (getAccountInfo account) >>= printJSON
    GetAccountNonFinalized account ->
      runInClient b $ getAccountNonFinalizedTransactions account >>= printJSON
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
  getAccountInfo (fromString $ show addr) blockhash >>= \case
    Left err -> fail err
    Right aval ->
      case parseNullable readAccountNonce aval of
        Error err     -> fail err
        Success Nothing -> fail $ printf "account '%s' not found" (show addr)
        Success (Just nonce) -> return nonce

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
parseNullable p v = parse (nullable p) v

nullable :: (Value -> Parser a) -> Value -> Parser (Maybe a)
nullable p v = case v of
                Null -> return Nothing
                _ -> Just <$> p v

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
  -> ClientMonad (PR.Context Core.UA m) Types.BareTransaction
processTransaction source networkId =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t  -> processTransaction_ t networkId True

processTransaction_ ::
     (MonadFail m, MonadIO m)
  => TransactionJSON
  -> Int
  -> Verbose
  -> ClientMonad (PR.Context Core.UA m) Types.BareTransaction
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
