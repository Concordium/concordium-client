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
import           Concordium.Client.Types.TransactionStatus
import           Concordium.Client.Validate
import qualified Concordium.Crypto.BlockSignature    as BlockSig
import qualified Concordium.Crypto.BlsSignature      as Bls
import qualified Concordium.Crypto.Proofs            as Proofs
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
import           Data.IORef
import           Data.Aeson                          as AE
import qualified Data.Aeson.Encode.Pretty            as AE
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.ByteString.Lazy.Char8          as BSL8
import qualified Data.Char                           as C
import qualified Data.HashMap.Strict                 as Map
import           Data.Maybe
import qualified Data.Serialize                      as S
import           Data.String
import           Data.Text
import           Data.Text.Encoding
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
    client <- liftClientIOToM (mkGrpcClient $! GrpcConfig (COM.grpcHost bkend) (COM.grpcPort bkend) (COM.grpcTarget bkend) (COM.grpcRetryNum bkend))
    ret <- ((runReaderT . _runClientMonad) comp $! client)
    liftClientIOToM (maybe (return ()) close =<< (liftIO . readIORef $ (grpc client)))
    return ret
  case r of
    Left err -> error (show err)
    Right x  -> return x

withClientJson :: (MonadIO m, FromJSON a) => Backend -> ClientMonad m (Either String Value) -> m a
withClientJson b comp = do
  r <- withClient b comp
  s <- case r of
         Left err -> logFatal ["RPC error: " ++ err]
         Right v -> return v
  case fromJSON s of
    Error err -> logFatal [printf "cannot parse '%s' as JSON: %s" (show s) err]
    Success v -> return v

process :: COM.Options -> IO ()
process Options{optsCmd = LegacyCmd c, optsBackend = backend} = processLegacyCmd c backend
process Options{optsCmd = command, optsBackend = backend, optsConfigDir = cfgDir, optsVerbose = verbose} = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering
  -- Evaluate command.
  maybe printNoBackend (p verbose) backend
  where p = case command of
              ConfigCmd c -> processConfigCmd c cfgDir
              TransactionCmd c -> processTransactionCmd c cfgDir
              AccountCmd c -> processAccountCmd c
              ModuleCmd c -> processModuleCmd c
              ContractCmd c -> processContractCmd c
              ConsensusCmd c -> processConsensusCmd c
              BlockCmd c -> processBlockCmd c
              BakerCmd c -> processBakerCmd c cfgDir
              LegacyCmd _ -> error "Unreachable case: LegacyCmd."

processConfigCmd :: ConfigCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processConfigCmd action baseCfgDir verbose _ =
  case action of
    ConfigShow -> do
      baseCfg <- getBaseConfig baseCfgDir verbose
      runPrinter $ printBaseConfig baseCfg
      putStrLn ""
      accCfgs <- getAllAccountConfigs baseCfg
      runPrinter $ printAccountConfigList accCfgs

processTransactionCmd :: TransactionCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processTransactionCmd action baseCfgDir verbose backend =
  case action of
    TransactionSubmit fname -> do
      -- TODO Ensure that the "nonce" field is optional in the payload.
      mdata <- loadContextData
      source <- BSL.readFile fname
      tx <- PR.evalContext mdata $ withClient backend $ processTransaction source defaultNetId
      logInfo [ printf "transaction sent to the baker"
              , printf "query status using 'transaction status %s'" (show $ getBlockItemHash tx) ]
    TransactionDeployCredential fname -> do
      source <- BSL.readFile fname
      tx <- withClient backend $ processCredential source defaultNetId
      logInfo [ printf "credential sent to the baker"
              , printf "query status using 'transaction status %s'" (show $ getBlockItemHash tx) ]
    TransactionStatus hash -> do
      validateTransactionHash hash
      status <- withClient backend $ queryTransactionStatus (read $ unpack hash)
      when verbose $ logInfo [printf "Response: %s" (show status)]
      runPrinter $ printTransactionStatus status
    TransactionSendGtu receiver amount txCfg -> do
      baseCfg <- getBaseConfig baseCfgDir verbose

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      keysArg <- case decodeKeysArg $ tcKeys txCfg of
                   Nothing -> return Nothing
                   Just act -> act >>= \case
                     Left err -> die err
                     Right ks -> return $ Just ks
      accCfg <- getAccountConfig (tcSender txCfg) baseCfg Nothing keysArg

      when verbose $ do
        runPrinter $ printAccountConfig accCfg
        putStrLn ""

      let keys = acKeys accCfg
          fromAddress = acAddr accCfg
      -- TODO Allow referencing address by name (similar to "sender")?
      toAddress <- getAddressArg "to address" $ Just receiver

      let transactionFee = simpleTransferEnergyCost (Prelude.length keys)
      energy <- case tcMaxEnergyAmount txCfg of
                  Nothing -> return transactionFee
                  Just maxEnergy -> promptEnergyUpdate maxEnergy transactionFee
      expiry <- getArg "expiry" $ tcExpiration txCfg
      logInfo [ printf "sending %s from %s to '%s'" (showGtu amount) (showNamedAddress accCfg) (show toAddress)
              , printf "allowing up to %s to be spent as transaction fee" (showNrg energy) ]
      putStr "Confirm [yN]: "
      input <- getChar
      when (C.toLower input /= 'y') $ logFatal ["transaction cancelled"]

      let t = TransactionJSON
                (TransactionJSONHeader fromAddress (tcNonce txCfg) energy expiry)
                (Transfer (Types.AddressAccount toAddress) amount)
                keys
      -- TODO Only needed because we're going through the generic
      --      processTransaction/encodeAndSignTransaction functions.
      --      Refactor to only include what's needed.
      tx <- PR.evalContext emptyContextData $ withClient backend $ processTransaction_ t defaultNetId False
      let hash = getBlockItemHash tx
      logInfo [ "transaction sent to the baker"
              , "waiting for transaction to be committed and finalized"
              , printf "you may skip this by interrupting this command (using Ctrl-C) - the transaction will still get procesed and may be queried using 'transaction status %s'" (show hash) ]

      t1 <- getFormattedLocalTimeOfDay
      printf "[%s] Waiting for the transaction to be committed..." t1
      committedStatus <- withClient backend $ awaitState 2 Committed hash
      putStrLn ""

      when (tsrState committedStatus == Absent) $ logFatal ["transaction failed before it got committed", "most likely because it was invalid"]

      runPrinter $ printTransactionStatus committedStatus

      -- If the transaction goes back to pending state after being committed to a branch which gets dropped later on,
      -- the command will currently keep presenting the transaction as committed and awaiting finalization.
      -- As the transaction is still expected to go back to being committed or (if for instance it expires) become absent,
      -- this seems acceptable from a UX perspective. Also, this is expected to be a very uncommon case.

      t2 <- getFormattedLocalTimeOfDay
      printf "[%s] Waiting for the transaction to be finalized..." t2
      finalizedStatus <- withClient backend $ awaitState 5 Finalized hash
      putStrLn ""

      when (tsrState finalizedStatus == Absent) $ logFatal ["transaction failed after it was committed"]

      -- Print out finalized status if the outcome differs from that of the committed status.
      when (tsrResults committedStatus /= tsrResults finalizedStatus) $ do
        runPrinter $ printTransactionStatus finalizedStatus

      t3 <- getFormattedLocalTimeOfDay
      printf "[%s] Transfer completed.\n" t3
  where
    promptEnergyUpdate energy actualFee
      | energy < actualFee = do
          logWarn ["insufficient energy allocated to the transaction!"
                  , printf "transaction fee will be %s, but only %s has been allocated" (showNrg actualFee) (showNrg energy) ]
          printf "Do you want to increase the energy allocation to %s? [yN]" (showNrg actualFee)
          input <- getChar
          if C.toLower input == 'y' then return actualFee else return energy
      | actualFee < energy = do
          logInfo [printf "%s allocated to the transaction, but only %s is needed" (showNrg energy) (showNrg actualFee)]
          return energy
      | otherwise = return energy

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
      v <- withClientJson backend $ withBestBlockHash block (getAccountInfo address)
      case v of
        Nothing -> putStrLn "Account not found."
        Just a -> runPrinter $ printAccountInfo address a verbose
    AccountList block -> do
      v <- withClientJson backend $ withBestBlockHash block getAccountList
      runPrinter $ printAccountList v

processModuleCmd :: ModuleCmd -> Verbose -> Backend -> IO ()
processModuleCmd action _ backend = putStrLn $ "Not yet implemented: " ++ show action ++ ", " ++ show backend

processContractCmd :: ContractCmd -> Verbose -> Backend -> IO ()
processContractCmd action _ backend = putStrLn $ "Not yet implemented: " ++ show action ++ ", " ++ show backend

processConsensusCmd :: ConsensusCmd -> Verbose -> Backend -> IO ()
processConsensusCmd action _ backend =
  case action of
    ConsensusStatus -> do
      v <- withClientJson backend getConsensusStatus
      runPrinter $ printConsensusStatus v
    ConsensusShowParameters b includeBakers -> do
      v <- withClientJson backend $ withBestBlockHash b getBirkParameters
      case v of
        Nothing -> putStrLn "Block not found."
        Just p -> runPrinter $ printBirkParameters includeBakers p

processBlockCmd :: BlockCmd -> Verbose -> Backend -> IO ()
processBlockCmd action _ backend =
  case action of
    BlockShow b -> do
      unless (maybe True isValidBlockHash b) $
          -- NB: The use of show on text is deliberate to produce quoted output.
          logFatal [show (fromJust b) ++ " is not a valid block hash."]

      v <- withClientJson backend $ withBestBlockHash b getBlockInfo
      runPrinter $ printBlockInfo v

processBakerCmd :: BakerCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processBakerCmd action baseCfgDir verbose backend =
  case action of
    BakerGenerateKeys outputFile -> do
      -- Aggr/bls keys
      aggrSk <- Bls.generateSecretKey
      let aggrPk = Bls.derivePublicKey aggrSk
      -- Election keys
      VRF.KeyPair {privateKey=elSk, publicKey=elPk} <- VRF.newKeyPair
      -- Signature keys
      BlockSig.KeyPair {signKey=sigSk, verifyKey=sigVk} <- BlockSig.newKeyPair
      let keys = BakerKeys { bkAggrSignKey = aggrSk
                           , bkAggrVerifyKey = aggrPk
                           , bkElectionSignKey = elSk
                           , bkElectionVerifyKey = elPk
                           , bkSigSignKey = sigSk
                           , bkSigVerifyKey = sigVk }
      let out = AE.encodePretty keys

      case outputFile of
        Nothing -> do
          -- TODO Store in config.
          BSL8.putStrLn out
          logInfo [ printf "to add a baker to the chain using these keys, store it in a file and use 'baker add FILE'" ]
        Just f -> do
          BSL.writeFile f out
          logInfo [ printf "keys written to file '%s'" f
                  , "DO NOT LOSE THIS FILE"
                  , printf "to add a baker to the chain using these keys, use 'baker add %s'" f ]
    BakerAdd f txCfg -> do
      src <- readFile f
      v <- case eitherDecodeStrict $ encodeUtf8 $ pack src of
             Left err -> logFatal [printf "cannot decode file '%s' as JSON: %s" f err]
             Right r -> return r
      bakerKeys <- case fromJSON v of
                     Error err -> logFatal [printf "cannot parse '%s' as baker keys: %s" src err]
                     Success r -> return r

      baseCfg <- getBaseConfig baseCfgDir verbose

      -- TODO Deduplicate with transaction send-gtu.
      --      And await finalization using the same logic.

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      keysArg <- case decodeKeysArg $ COM.tcKeys txCfg of
                   Nothing -> return Nothing
                   Just act -> act >>= \case
                     Left err -> die err
                     Right ks -> return $ Just ks
      accCfg <- getAccountConfig (tcSender txCfg) baseCfg Nothing keysArg

      when verbose $ do
        runPrinter $ printAccountConfig accCfg
        putStrLn ""

      energy <- getArg "max energy amount" $ tcMaxEnergyAmount txCfg
      expiry <- getArg "expiry" $ tcExpiration txCfg

      pl <- generateAddBakerPayload bakerKeys AccountKeys {akAddress = acAddr accCfg, akKeys = acKeys accCfg}

      logInfo [ printf "submitting transaction to add baker using keys from file '%s'" f
              , "DO NOT LOSE THIS FILE"]

      when verbose $ logInfo [printf "Payload: %s" (show pl)]

      -- Send transaction
      let sender = acAddr accCfg
      tx <- withClient backend $ do
        nonce <- case tcNonce txCfg of
                   Nothing -> getBestBlockHash >>= getAccountNonce sender
                   Just n -> return n
        let tx = encodeAndSignTransaction pl energy nonce expiry sender (acKeys accCfg)
        sendTransactionToBaker tx defaultNetId >>= \case
          Left err -> fail err
          Right False -> fail "Transaction not accepted by the baker."
          Right True -> return tx
      when verbose $ logInfo [printf "Transaction: %s" (show tx)]
      let hash = getBlockItemHash tx
      logInfo [ "transaction sent to the baker"
              , printf "the transaction may be queried using 'transaction status %s'" (show hash) ]

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
    act ->
      maybe printNoBackend (useBackend act) backend

printNoBackend :: IO ()
printNoBackend = putStrLn "No Backend provided."

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

generateChallenge :: Types.BakerElectionVerifyKey -> Types.BakerSignVerifyKey -> Types.BakerAggregationVerifyKey -> Types.AccountAddress -> BS.ByteString
generateChallenge electionVerifyKey sigVerifyKey aggrVerifyKey accountAddr =
  S.runPut (S.put electionVerifyKey <> S.put sigVerifyKey <> S.put aggrVerifyKey <> S.put accountAddr)

generateAddBakerPayload :: BakerKeys -> AccountKeys -> IO Types.Payload
generateAddBakerPayload bk ak = do
  let electionSignKey = bkElectionSignKey bk
      signatureSignKey = bkSigSignKey bk
      aggrSignKey = bkAggrSignKey bk

  let abElectionVerifyKey = bkElectionVerifyKey bk
      abSignatureVerifyKey = bkSigVerifyKey bk
      abAggregationVerifyKey = bkAggrVerifyKey bk

  let abAccount = akAddress ak
      keyMap = akKeys ak

  let challenge = generateChallenge abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey abAccount
  abProofElection <- Proofs.proveDlog25519VRF challenge (VRF.KeyPair electionSignKey abElectionVerifyKey) `except` "cannot produce VRF key proof"
  abProofSig <- Proofs.proveDlog25519Block challenge (BlockSig.KeyPair signatureSignKey abSignatureVerifyKey) `except` "cannot produce signature key proof"
  proofAccount <- forM keyMap (\key -> Proofs.proveDlog25519KP challenge key `except` "cannot produce account keys proof")
  let abProofAggregation = Bls.proveKnowledgeOfSK challenge aggrSignKey
      abProofAccount = Types.AccountOwnershipProof (Map.toList proofAccount)
  return Types.AddBaker {..}

  where except c err = c >>= \case
          Just x -> return x
          Nothing -> logFatal [err]

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
        Nothing -> getBestBlockHash >>= getAccountNonce sender
        Just nonce -> return nonce
    txPayload <- convertTransactionPayload $ payload transaction
    return $ encodeAndSignTransaction
      txPayload
      (thEnergyAmount header)
      nonce
      (thExpiry header)
      sender
      (CT.keys transaction)

  sendTransactionToBaker tx networkId >>= \case
    Left err -> fail $ show err
    Right False -> fail "Transaction not accepted by the baker."
    Right True -> return tx

processCredential ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> ClientMonad m Types.BareBlockItem
processCredential source networkId =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right cred ->
      let tx = Types.CredentialDeployment cred
      in sendTransactionToBaker tx networkId >>= \case
           Left err -> fail err
           Right False -> fail "Transaction not accepted by the baker."
           Right True -> return tx

convertTransactionPayload :: (MonadFail m, MonadIO m) => CT.TransactionJSONPayload -> ClientMonad (PR.Context Core.UA m) Types.Payload
convertTransactionPayload = \case
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
  (CT.RemoveBaker rbid) -> return $ Types.RemoveBaker rbid
  (CT.UpdateBakerAccount ubid uba ubp) ->
    return $ Types.UpdateBakerAccount ubid uba ubp
  (CT.UpdateBakerSignKey ubsid ubsk ubsp) ->
    return $ Types.UpdateBakerSignKey ubsid ubsk ubsp
  (CT.DelegateStake dsid) -> return $ Types.DelegateStake dsid
  (CT.UpdateElectionDifficulty d) -> return $ Types.UpdateElectionDifficulty d

encodeAndSignTransaction ::
     Types.Payload
  -> Types.Energy
  -> Types.Nonce
  -> Types.TransactionExpiryTime
  -> Types.AccountAddress
  -> KeyMap
  -> Types.BareBlockItem
encodeAndSignTransaction txPayload energy nonce expiry sender keys = Types.NormalTransaction $
  let encPayload = Types.encodePayload txPayload
      header = Types.TransactionHeader{
        thSender = sender,
        thPayloadSize = Types.payloadSize encPayload,
        thNonce = nonce,
        thEnergyAmount = energy,
        thExpiry = expiry
      }
  in Types.signTransaction (Map.toList keys) header encPayload
