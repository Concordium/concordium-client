{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , generateBakerKeys
  , generateBakerAddPayload
  , getAccountInfo
  , getModuleSet
  , getStatusOfPeers
  , StatusOfPeers(..)
  , ClientMonad(..)
  , withClient
  , EnvData(..)
  , GrpcConfig
  , processTransaction_
  , awaitState
  -- * For importing support
  , decodeGenesisFormattedAccountExport
  , accountCfgsFromWalletExportAccounts
  , decodeMobileFormattedAccountExport
  -- * For adding baker support
  , startTransaction
  , encodeAndSignTransaction
  , tailTransaction
  , getBlockItemHash
  -- * For delegation support
  , getAccountDelegateTransactionCfg
  , adtcTransactionCfg
  , accountDelegateTransactionPayload
  , withClientJson
  , getAccountUndelegateTransactionCfg
  , autcTransactionCfg
  , accountUndelegateTransactionPayload
  , TransactionConfig(..)
  , getFromJson
  ) where

import           Concordium.Client.Utils
import           Concordium.Client.Cli
import           Concordium.Client.Config
import           Concordium.Client.Commands          as COM
import           Concordium.Client.Export
import           Concordium.Client.GRPC
import           Concordium.Client.Output
import           Concordium.Client.Parse
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Account
import           Concordium.Client.Types.Transaction as CT
import           Concordium.Client.Types.TransactionStatus
import           Concordium.Common.Version
import qualified Concordium.Crypto.EncryptedTransfers as Enc
import qualified Concordium.Crypto.BlockSignature    as BlockSig
import qualified Concordium.Crypto.BlsSignature      as Bls
import qualified Concordium.Crypto.Proofs            as Proofs
import qualified Concordium.Crypto.SignatureScheme   as SigScheme
import qualified Concordium.Crypto.VRF               as VRF
import qualified Concordium.Types.Transactions       as Types
import           Concordium.Types.HashableTo
import qualified Concordium.Types.Execution          as Types
import qualified Concordium.Types                    as Types
import qualified Concordium.ID.Types                 as ID
import           Concordium.ID.Parameters
import           Proto.ConcordiumP2pRpc
import qualified Proto.ConcordiumP2pRpc_Fields       as CF

import           Control.Monad.Fail
import           Control.Monad.Except
import           Control.Monad.Reader                hiding (fail)
import           GHC.Generics
import           Data.IORef
import           Data.Foldable
import           Data.Aeson                          as AE
import           Data.Aeson.Types
import qualified Data.Aeson.Encode.Pretty            as AE
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.ByteString.Lazy.Char8          as BSL8
import qualified Data.HashMap.Strict                 as Map
import qualified Data.Map                            as OrdMap
import           Data.Maybe
import           Data.List                           as L
import qualified Data.Serialize                      as S
import qualified Data.Set                            as Set
import qualified Data.Sequence                       as Seq
import           Data.String
import           Data.Text(Text)
import qualified Data.Text                           as Text
import           Data.Text.Encoding
import           Data.Word
import           Lens.Micro.Platform
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client.Exceptions
import           Prelude                             hiding (fail, mod, unlines)
import           System.IO
import           System.Directory
import           Text.Printf

liftClientIOToM :: MonadIO m => ClientIO a -> ExceptT ClientError m a
liftClientIOToM comp = do
  r <- liftIO (runClientIO comp)
  case r of
    Left err  -> throwError err
    Right res -> return res

withClient :: MonadIO m => Backend -> ClientMonad m a -> m a
withClient bkend comp = do
  r <- runExceptT $! do
    let config = GrpcConfig (COM.grpcHost bkend) (COM.grpcPort bkend) (COM.grpcAuthenticationToken bkend) (COM.grpcTarget bkend) (COM.grpcRetryNum bkend) Nothing
    client <- liftClientIOToM (mkGrpcClient config Nothing)
    ret <- (runReaderT . _runClientMonad) comp $! client
    liftClientIOToM (maybe (return ()) close =<< (liftIO . readIORef $ grpc client))
    return ret
  case r of
    Left err -> error (show err)
    Right x  -> return x

withClientJson :: (MonadIO m, FromJSON a) => Backend -> ClientMonad m (Either String Value) -> m a
withClientJson b c = withClient b c >>= getFromJson

-- |Helper function for parsing JSON Value or fail if the value is missing or not valid JSON.
-- The parameter has the same type as the one returned by e.g. eitherDecode or processJSON,
-- which many of the GRPC commands use.
getFromJson :: (MonadIO m, FromJSON a) => Either String Value -> m a
getFromJson r = do
  s <- case r of
         Left err -> logFatal [printf "I/O error: %s" err]
         Right v -> return v
  case fromJSON s of
    Error err -> logFatal [printf "cannot parse '%s' as JSON: %s" (show s) err]
    Success v -> return v

-- |Look up account from the provided name or address. If 'Nothing' is given, use the defaultAcccountName.
-- Fail if the name or address cannot be found.
getAccountAddressArg :: AccountNameMap -> Maybe Text -> IO NamedAddress
getAccountAddressArg m input =
  case getAccountAddress m $ fromMaybe defaultAccountName input of
    Left err -> logFatal [err]
    Right v -> return v

-- |Process CLI command.
process :: COM.Options -> IO ()
process Options{optsCmd = command, optsBackend = backend, optsConfigDir = cfgDir, optsVerbose = verbose} = do
  -- Disable output buffering.
  hSetBuffering stdout NoBuffering

  case command of
    LegacyCmd c -> processLegacyCmd c backend
    ConfigCmd c -> processConfigCmd c cfgDir verbose
    TransactionCmd c -> processTransactionCmd c cfgDir verbose backend
    AccountCmd c -> processAccountCmd c cfgDir verbose backend
    ModuleCmd c -> processModuleCmd c verbose backend
    ContractCmd c -> processContractCmd c verbose backend
    ConsensusCmd c -> processConsensusCmd c cfgDir verbose backend
    BlockCmd c -> processBlockCmd c verbose backend
    BakerCmd c -> processBakerCmd c cfgDir verbose backend
    IdentityCmd c -> processIdentityCmd c backend

-- |Process a 'config ...' command.
processConfigCmd :: ConfigCmd -> Maybe FilePath -> Verbose ->  IO ()
processConfigCmd action baseCfgDir verbose =
  case action of
    ConfigInit -> void $ initBaseConfig baseCfgDir
    ConfigShow -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      runPrinter $ printBaseConfig baseCfg
      putStrLn ""
      accCfgs <- getAllAccountConfigs baseCfg
      runPrinter $ printAccountConfigList accCfgs
    ConfigAccountCmd c -> case c of
      ConfigAccountAdd addr naName -> do
        baseCfg <- getBaseConfig baseCfgDir verbose True
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        naAddr <-
          case ID.addressFromText addr of
            Left err -> logFatal [printf "cannot parse '%s' as an address: %s" addr err]
            Right a -> return a
        forM_ naName $ logFatalOnError . validateAccountName
        void $ initAccountConfig baseCfg NamedAddress{..}
      ConfigAccountImport file name importFormat -> do
        baseCfg <- getBaseConfig baseCfgDir verbose True
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        fileExists <- doesFileExist file
        unless fileExists $ logFatal [printf "the given file '%s' does not exist" file]

        -- NB: This also checks whether the name is valid if provided.
        accCfgs <- loadAccountImportFile importFormat file name
        void $ importAccountConfig baseCfg accCfgs
      ConfigAccountAddKeys addr keysFile -> do
        baseCfg <- getBaseConfig baseCfgDir verbose True
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        keyMapInput :: EncryptedAccountKeyMap <- AE.eitherDecodeFileStrict keysFile `withLogFatalIO` ("cannot decode keys: " ++)
        (baseCfg', accCfg) <- getAccountConfig (Just addr) baseCfg Nothing Nothing Nothing True

        let keyMapCurrent = acKeys accCfg
        let keyMapNew = Map.difference keyMapInput keyMapCurrent
        let accCfg' = accCfg { acKeys = Map.union keyMapCurrent keyMapNew}
        -- TODO warn about keys not added because index already exists

        writeAccountKeys baseCfg' accCfg'

-- |Read and parse a file exported from either genesis data or mobile wallet.
-- The format specifier tells which format to expect.
-- If the format is "mobile", the user is prompted for a password which is used to decrypt
-- the exported data. This may result in multiple named accounts. If a name is provided,
-- only the account with that name is being selected for import.
-- The "genesis" format is not encrypted and only contains a single account which is not named.
-- If a name is provided in this case, this will become the account name.
loadAccountImportFile :: AccountExportFormat -> FilePath -> Maybe Text -> IO [AccountConfig]
loadAccountImportFile format file name = do
  contents <- BS.readFile file
  case format of
    FormatMobile -> do
      pwd <- askPassword "Enter encryption password: "
      accCfgs <- decodeMobileFormattedAccountExport contents name pwd `withLogFatalIO` ("cannot import accounts: " ++)

      logInfo ["loaded account(s):"]
      forM_ accCfgs $ \AccountConfig{acAddr=NamedAddress{..}} -> logInfo [printf "- %s (%s)" (show naAddr) (maybe "-" show naName)]
      logInfo ["all signing keys have been encrypted with the password used for this import."]

      when (null accCfgs) $ logWarn ["no accounts selected for import"]
      return accCfgs
    FormatGenesis -> do
     pwd <- createPasswordInteractive (Just "encrypt all signing keys with") `withLogFatalIO` id
     accCfg <- decodeGenesisFormattedAccountExport contents name pwd `withLogFatalIO` ("cannot import account: " ++)
     return [accCfg]


-- |Process a 'transaction ...' command.
processTransactionCmd :: TransactionCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processTransactionCmd action baseCfgDir verbose backend =
  case action of
    TransactionSubmit fname intOpts -> do
      -- TODO Ensure that the "nonce" field is optional in the payload.
      source <- BSL.readFile fname

      -- TODO Print transaction details and ask for confirmation if (ioConfirm intOpts)

      withClient backend $ do
        tx <- processTransaction source defaultNetId
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "transaction successfully completed" ]
    TransactionDeployCredential fname intOpts -> do
      source <- BSL.readFile fname
      withClient backend $ do
        tx <- processCredential source defaultNetId
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "credential deployed successfully" ]
    TransactionStatus h -> do
      hash <- case parseTransactionHash h of
        Nothing -> logFatal [printf "invalid transaction hash '%s'" h]
        Just hash -> return hash
      status <- withClient backend $ queryTransactionStatus hash
      when verbose $ logInfo [printf "Response: %s" (show status)]
      runPrinter $ printTransactionStatus status
      -- TODO This works but output doesn't make sense if transaction is already committed/finalized.
      --      It should skip already completed steps.
      -- withClient backend $ tailTransaction hash
    TransactionSendGtu receiver amount txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""
      ttxCfg <- getTransferTransactionCfg baseCfg txOpts receiver amount
      let txCfg = ttcTransactionCfg ttxCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      let intOpts = toInteractionOpts txOpts
      pl <- transferTransactionPayload ttxCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    TransactionEncryptedTransfer txOpts receiver amount index -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      let nrgCost _ = return $ Just encryptedTransferEnergyCost
      txCfg <- liftIO (getTransactionCfg baseCfg txOpts nrgCost)

      encryptedSecretKey <- maybe (logFatal ["Missing account encryption secret key for account: " ++ show (acAddr . tcAccountCfg $ txCfg)]) return (acEncryptionKey . tcAccountCfg $ txCfg)
      secretKey <- either (\e -> logFatal ["Couldn't decrypt account encryption secret key: " ++ e]) return =<< decryptAccountEncryptionSecretKeyInteractive encryptedSecretKey

      withClient backend $ do
        ttxCfg <- getEncryptedTransferTransactionCfg baseCfg txCfg receiver amount index secretKey
        liftIO $ when verbose $ do
          runPrinter $ printAccountConfig $ tcAccountCfg txCfg
          putStrLn ""

        let intOpts = toInteractionOpts txOpts
        pl <- encryptedTransferTransactionPayload ttxCfg (ioConfirm intOpts)
        sendAndTailTransaction txCfg pl intOpts

-- |Poll the transaction state continuously until it is "at least" the provided one.
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

-- |Function type for computing the transaction energy cost for a given number of keys.
-- Returns Nothing if the cost cannot be computed.
type ComputeEnergyCost = Int -> Types.Energy

-- |Function for computing a cost function based on the resolved account config.
type GetComputeEnergyCost = AccountConfig -> IO (Maybe ComputeEnergyCost)

-- |Resolved configuration common to all transaction types.
data TransactionConfig =
  TransactionConfig
  { tcAccountCfg :: AccountConfig
  , tcNonce :: Maybe Types.Nonce
  , tcEnergy :: Types.Energy
  , tcExpiry :: Types.TransactionExpiryTime }

-- |Resolve transaction config based on persisted config and CLI flags.
-- If an energy cost function is provided and it returns a value which
-- is different from the specified energy allocation, a warning is logged.
-- If the energy allocation is too low, the user is prompted to increase it.
getTransactionCfg :: BaseConfig -> TransactionOpts -> GetComputeEnergyCost -> IO TransactionConfig
getTransactionCfg baseCfg txOpts getEnergyCostFunc = do
  keysArg <- case toKeys txOpts of
               Nothing -> return Nothing
               Just filePath -> AE.eitherDecodeFileStrict filePath `withLogFatalIO` ("cannot decode keys: " ++)
  (_, accCfg) <- getAccountConfig (toSender txOpts) baseCfg Nothing keysArg Nothing False

  energyCostFunc <- getEnergyCostFunc accCfg
  let computedCost = case energyCostFunc of
                       Nothing -> Nothing
                       Just ec -> Just $ ec (length $ acKeys accCfg)
  energy <- case (toMaxEnergyAmount txOpts, computedCost) of
              (Nothing, Nothing) -> logFatal ["energy amount not specified"]
              (Nothing, Just c) -> do
                logInfo [printf "using default energy amount of %s" (showNrg c)]
                return c
              (Just maxCost, Nothing) -> return maxCost
              (Just maxCost, Just c) -> promptEnergyUpdate maxCost c

  now <- getCurrentTimeUnix
  expiry <- getExpiryArg "expiry" now $ toExpiration txOpts
  warnSuspiciousExpiry expiry now

  return TransactionConfig
    { tcAccountCfg = accCfg
    , tcNonce = toNonce txOpts
    , tcEnergy = energy
    , tcExpiry = expiry }
  where
    promptEnergyUpdate energy actualFee
      | energy < actualFee = do
          logWarn [ "insufficient energy allocated to the transaction"
                  , printf "transaction fee will be %s, but only %s has been allocated" (showNrg actualFee) (showNrg energy) ]
          confirmed <- askConfirmation $ Just $ printf "Do you want to increase the energy allocation to %s?" (showNrg actualFee)
          return $ if confirmed then actualFee else energy
      | actualFee < energy = do
          logInfo [printf "%s allocated to the transaction, but only %s is needed" (showNrg energy) (showNrg actualFee)]
          return energy
      | otherwise = return energy
    -- Warn if expiry is in the past or very near or distant future.
    -- As the timestamps are unsigned, taking the simple difference might cause underflow.
    warnSuspiciousExpiry e now
      | e < now =
        logWarn [ "expiration time is in the past"
                , "the transaction will not be committed" ]
      | e < now + 30 =
        logWarn [ printf "expiration time is in just %s seconds" (show $ now - e)
                , "this may not be enough time for the transaction to be committed" ]
      | e > now + 3600 =
        logWarn [ "expiration time is in more than one hour" ]
      | otherwise = return ()

-- |Resolved configuration for a transfer transaction.
data TransferTransactionConfig =
  TransferTransactionConfig
  { ttcTransactionCfg :: TransactionConfig
  , ttcReceiver :: NamedAddress
  , ttcAmount :: Types.Amount }

-- |Resolve configuration of a transfer transaction based on persisted config and CLI flags.
-- See the docs for getTransactionCfg for the behavior when no or a wrong amount of energy is allocated.
getTransferTransactionCfg :: BaseConfig -> TransactionOpts -> Text -> Types.Amount -> IO TransferTransactionConfig
getTransferTransactionCfg baseCfg txOpts receiver amount = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost

  receiverAddress <- getAccountAddressArg (bcAccountNameMap baseCfg) $ Just receiver

  return TransferTransactionConfig
    { ttcTransactionCfg = txCfg
    , ttcReceiver = receiverAddress
    , ttcAmount = amount }
  where nrgCost _ = return $ Just simpleTransferEnergyCost

data EncryptedTransferTransactionConfig =
  EncryptedTransferTransactionConfig
  { ettTransactionCfg :: TransactionConfig
  , ettReceiver :: NamedAddress
  , ettAmount :: Types.Amount
  , ettTransferData :: !Enc.EncryptedAmountTransferData }

getEncryptedTransferTransactionCfg :: BaseConfig -> TransactionConfig -> Text -> Types.Amount -> Maybe Int -> ElgamalSecretKey -> ClientMonad IO EncryptedTransferTransactionConfig
getEncryptedTransferTransactionCfg baseCfg ettTransactionCfg receiver ettAmount idx secretKey = do
  let senderAddr = acAddress . tcAccountCfg $ ettTransactionCfg

  -- get encrypted amounts for the sender
  infoValue <- logFatalOnError =<< (withBestBlockHash Nothing $ getAccountInfo (Text.pack . show $ senderAddr))
  case AE.fromJSON infoValue of
    AE.Error err -> logFatal ["Cannot decode account info response from the node: " ++ err]
    AE.Success Nothing -> logFatal [printf "Account %s does not exist on the chain." $ show senderAddr]
    AE.Success (Just AccountInfoResult{airEncryptedAmount=Types.AccountEncryptedAmount{..}}) -> do
      -- get receiver's public encryption key
      infoValueReceiver <- logFatalOnError =<< (withBestBlockHash Nothing $ getAccountInfo receiver)
      case AE.fromJSON infoValueReceiver of
        AE.Error err -> logFatal ["Cannot decode account info response from the node: " ++ err]
        AE.Success Nothing -> logFatal [printf "Account %s does not exist on the chain." $ show senderAddr]
        AE.Success (Just air) -> do
          let receiverPk = ID._elgamalPublicKey $ airEncryptionKey air
          -- precomputed table for speeding up decryption
          let table = Enc.computeTable globalContext (2^(16::Int))
              decoder = Enc.decryptAmount table secretKey
              selfDecrypted = decoder _selfAmount
          -- aggregation of idx encrypted amounts
              taker = case idx of
                Nothing -> id
                Just v -> Seq.take v
              inputEncAmounts = taker _incomingEncryptedAmounts
              aggAmounts = foldl' (<>) _selfAmount inputEncAmounts
              totalEncryptedAmount = foldl' (+) selfDecrypted $ fmap decoder inputEncAmounts
          unless (totalEncryptedAmount >= ettAmount) $
            logFatal [printf "The requested transfer (%s) is more than the total encrypted balance (%s)." (show ettAmount) (show totalEncryptedAmount)]
          -- index indicating which encrypted amounts we used as input
          let aggIndex = Enc.EncryptedAmountAggIndex (Enc.theAggIndex _startIndex + fromIntegral (fromMaybe (Seq.length _incomingEncryptedAmounts) idx))
          let aggAmount = Enc.makeAggregatedDecryptedAmount aggAmounts totalEncryptedAmount aggIndex
          ettReceiver <- liftIO $ getAccountAddressArg (bcAccountNameMap baseCfg) $ Just receiver
          liftIO $ Enc.makeEncryptedAmountTransferData globalContext receiverPk secretKey aggAmount ettAmount >>= \case
            Nothing -> logFatal ["Could not create transfer. Likely the provided secret key is incorrect."]
            Just ettTransferData -> return EncryptedTransferTransactionConfig{..}

-- |Resolved configuration for a 'baker add' transaction.
data BakerAddTransactionConfig =
  BakerAddTransactionConfig
  { batcTransactionCfg :: TransactionConfig
  , batcBakerKeys :: BakerKeys }

-- |Resolved configuration for a 'baker remove' transaction.
data BakerRemoveTransactionConfig =
  BakerRemoveTransactionConfig
  { brtcTransactionCfg :: TransactionConfig
  , brtcBakerId :: Types.BakerId }

-- |Resolved configuration for a 'baker set-account' transaction.
data BakerSetAccountTransactionConfig =
  BakerSetAccountTransactionConfig
  { bsatcTransactionCfg :: TransactionConfig
  , bsatcBakerId :: Types.BakerId
  , bsatcAccountSigningData :: AccountSigningData }

-- |Resolved configuration for a 'baker set-key' transaction.
data BakerSetKeyTransactionConfig =
  BakerSetKeyTransactionConfig
  { bsktcTransactionCfg :: TransactionConfig
  , bsktcBakerId :: Types.BakerId
  , bsktcKeyPair :: BlockSig.KeyPair }

data BakerSetAggregationKeyTransactionConfig =
  BakerSetAggregationKeyTransactionConfig
  { bsakTransactionCfg :: TransactionConfig
  , bsakBakerId :: Types.BakerId
  , bsakSecretKey :: Types.BakerAggregationPrivateKey
  , bsakPublicKey :: Types.BakerAggregationVerifyKey }

-- |Resolved configuration for an account delegation transaction.
data AccountDelegateTransactionConfig =
  AccountDelegateTransactionConfig
  { adtcTransactionCfg :: TransactionConfig
  , adtcBakerId :: Types.BakerId }

-- |Resolved configuration for an account undelegation transaction.
data AccountUndelegateTransactionConfig =
  AccountUndelegateTransactionConfig
  { autcTransactionCfg :: TransactionConfig }

data AccountUpdateKeysTransactionCfg =
  AccountUpdateKeysTransactionCfg
  { auktcTransactionCfg :: TransactionConfig
  , auktcKeys :: OrdMap.Map ID.KeyIndex Types.AccountVerificationKey }

data AccountAddKeysTransactionCfg =
  AccountAddKeysTransactionCfg
  { aaktcTransactionCfg :: TransactionConfig
  , aaktcKeys :: OrdMap.Map ID.KeyIndex Types.AccountVerificationKey
  , aaktcThreshold :: Maybe ID.SignatureThreshold }

data AccountRemoveKeysTransactionCfg =
  AccountRemoveKeysTransactionCfg
  { arktcTransactionCfg :: TransactionConfig
  , arktcIndices :: Set.Set ID.KeyIndex
  , arktcThreshold :: Maybe ID.SignatureThreshold }

-- |Resolved configuration for setting election difficuntly transaction.
data SetElectionDifficultyTransactionConfig =
  SetElectionDifficultyTransactionConfig
  { sdtcTransactionCfg :: TransactionConfig
  , sdtcDifficulty :: Types.ElectionDifficulty }

-- |Resolved configuration for a baker set-election-key transaction
data BakerSetElectionKeyTransactionConfig =
  BakerSetElectionKeyTransactionConfig
  { bsekTransactionCfg :: TransactionConfig
  , bsekBakerId :: Types.BakerId
  , bsekKeyPair :: VRF.KeyPair }

-- |Resolved configuration for transferring from public to encrypted balance.
data AccountEncryptTransactionConfig =
  AccountEncryptTransactionConfig
  { aeTransactionCfg :: TransactionConfig,
    aeAmount :: Types.Amount
  }

-- |Resolved configuration for transferring from encrypted to public balance.
data AccountDecryptTransactionConfig =
  AccountDecryptTransactionConfig
  { adTransactionCfg :: TransactionConfig,
    adTransferData :: Enc.SecToPubAmountTransferData
  }

-- |Resolve configuration of a 'baker add' transaction based on persisted config and CLI flags.
-- See the docs for getTransactionCfg for the behavior when no or a wrong amount of energy is allocated.
getBakerAddTransactionCfg :: BaseConfig -> TransactionOpts -> FilePath -> IO BakerAddTransactionConfig
getBakerAddTransactionCfg baseCfg txOpts f = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  bakerKeys <- eitherDecodeFileStrict f >>= getFromJson
  return BakerAddTransactionConfig
    { batcTransactionCfg = txCfg
    , batcBakerKeys = bakerKeys }
  where nrgCost _ = return $ Just bakerAddEnergyCost

-- |Resolve configuration of a 'baker add' transaction based on persisted config and CLI flags.
-- See the docs for getTransactionCfg for the behavior when no or a wrong amount of energy is allocated.
getBakerRemoveTransactionCfg :: BaseConfig -> TransactionOpts -> Types.BakerId -> IO BakerRemoveTransactionConfig
getBakerRemoveTransactionCfg baseCfg txOpts bid = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  return BakerRemoveTransactionConfig
    { brtcTransactionCfg = txCfg
    , brtcBakerId = bid }
  where nrgCost _ = return $ Just bakerRemoveEnergyCost

-- |Resolve configuration for an 'account delegate' transaction.
getAccountDelegateTransactionCfg :: BaseConfig -> TransactionOpts -> Types.BakerId -> GetComputeEnergyCost -> IO AccountDelegateTransactionConfig
getAccountDelegateTransactionCfg baseCfg txOpts bakerId nrgCost = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  return AccountDelegateTransactionConfig
    { adtcTransactionCfg = txCfg
    , adtcBakerId = bakerId }

-- |Resolve configuration for an 'account undelegate' transaction.
getAccountUndelegateTransactionCfg :: BaseConfig -> TransactionOpts -> GetComputeEnergyCost -> IO AccountUndelegateTransactionConfig
getAccountUndelegateTransactionCfg baseCfg txOpts nrgCost = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  return AccountUndelegateTransactionConfig
    { autcTransactionCfg = txCfg }

-- |Resolve configuration for an 'update keys' transaction
getAccountUpdateKeysTransactionCfg :: BaseConfig -> TransactionOpts -> FilePath -> IO AccountUpdateKeysTransactionCfg
getAccountUpdateKeysTransactionCfg baseCfg txOpts f = do
  keys <- getFromJson =<< eitherDecodeFileStrict f
  txCfg <- getTransactionCfg baseCfg txOpts (nrgCost $ length keys)
  return $ AccountUpdateKeysTransactionCfg txCfg keys
  where nrgCost numKeys _ = return $ Just $ accountUpdateKeysEnergyCost numKeys

getAccountAddKeysTransactionCfg :: BaseConfig -> TransactionOpts -> FilePath -> Maybe ID.SignatureThreshold -> IO AccountAddKeysTransactionCfg
getAccountAddKeysTransactionCfg baseCfg txOps f threshold = do
  keys <- getFromJson =<< eitherDecodeFileStrict f
  txCfg <- getTransactionCfg baseCfg txOps (nrgCost $ length keys)
  return $ AccountAddKeysTransactionCfg txCfg keys threshold
  where nrgCost numKeys _ = return $ Just $ accountAddKeysEnergyCost numKeys

getAccountRemoveKeysTransactionCfg :: BaseConfig -> TransactionOpts -> [ID.KeyIndex] -> Maybe ID.SignatureThreshold -> IO AccountRemoveKeysTransactionCfg
getAccountRemoveKeysTransactionCfg baseCfg txOpts idxs threshold = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  indexSet <- Types.safeSetFromAscList $ L.sort idxs
  return $ AccountRemoveKeysTransactionCfg txCfg indexSet threshold
  where
      nrgCost _ = return $ Just accountRemoveKeysEnergyCost

-- |Resolve configuration for transferring an amount from public to encrypted
-- balance of an account.
getAccountEncryptTransactionCfg :: BaseConfig -> TransactionOpts -> Types.Amount -> IO AccountEncryptTransactionConfig
getAccountEncryptTransactionCfg baseCfg txOpts aeAmount = do
  aeTransactionCfg <- getTransactionCfg baseCfg txOpts nrgCost
  return AccountEncryptTransactionConfig{..}
  where nrgCost _ = return $ Just accountEncryptEnergyCost

-- |Resolve configuration for transferring an amount from encrypted to public
-- balance of an account.
getAccountDecryptTransactionCfg :: TransactionConfig -> Types.Amount -> ElgamalSecretKey -> ClientMonad IO AccountDecryptTransactionConfig
getAccountDecryptTransactionCfg adTransactionCfg adAmount secretKey = do
  let senderAddr = acAddress . tcAccountCfg $ adTransactionCfg
  infoValue <- logFatalOnError =<< (withBestBlockHash Nothing $ getAccountInfo (Text.pack . show $ senderAddr))
  case AE.fromJSON infoValue of
    AE.Error err -> logFatal ["Cannot decode account info response from the node: " ++ err]
    AE.Success Nothing -> logFatal [printf "Account %s does not exist on the chain." $ show senderAddr]
    AE.Success (Just AccountInfoResult{airEncryptedAmount=Types.AccountEncryptedAmount{..},..}) -> do
      -- precomputed table for speeding up decryption
      let table = Enc.computeTable globalContext (2^(16::Int))
      let decoder = Enc.decryptAmount table secretKey
      let selfDecrypted = decoder _selfAmount
      -- aggregation of all encrypted amounts
      let aggAmounts = foldl' (<>) _selfAmount _incomingEncryptedAmounts
      let totalEncryptedAmount = foldl' (+) selfDecrypted $ fmap decoder _incomingEncryptedAmounts
      unless (totalEncryptedAmount >= adAmount) $
        logFatal [printf "The requested transfer (%s) is more than the total encrypted balance (%s)." (show adAmount) (show totalEncryptedAmount)]
      -- index indicating which encrypted amounts we used as input
      let aggIndex = Enc.EncryptedAmountAggIndex (Enc.theAggIndex _startIndex + fromIntegral (Seq.length _incomingEncryptedAmounts))
      let aggAmount = Enc.makeAggregatedDecryptedAmount aggAmounts totalEncryptedAmount aggIndex
      liftIO $ Enc.makeSecToPubAmountTransferData globalContext secretKey aggAmount adAmount >>= \case
        Nothing -> logFatal ["Could not create transfer. Likely the provided secret key is incorrect."]
        Just adTransferData -> return AccountDecryptTransactionConfig{..}


-- |Convert transfer transaction config into a valid payload,
-- optionally asking the user for confirmation.
transferTransactionPayload :: TransferTransactionConfig -> Bool -> IO Types.Payload
transferTransactionPayload ttxCfg confirm = do
  let TransferTransactionConfig
        { ttcTransactionCfg = TransactionConfig
                              { tcEnergy = energy
                              , tcExpiry = expiryTs
                              , tcAccountCfg = AccountConfig { acAddr = fromAddress } }
        , ttcAmount = amount
        , ttcReceiver = toAddress }
        = ttxCfg

  logSuccess [ printf "sending %s from %s to %s" (showGtu amount) (showNamedAddress fromAddress) (showNamedAddress toAddress)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiryTs) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return Types.Transfer { tToAddress = naAddr toAddress, tAmount = amount }

encryptedTransferTransactionPayload :: MonadIO m => EncryptedTransferTransactionConfig -> Bool -> m Types.Payload
encryptedTransferTransactionPayload EncryptedTransferTransactionConfig{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcAccountCfg = AccountConfig { acAddr = addr } }
        = ettTransactionCfg

  logInfo $
    [ printf "transferring %s GTU from encrypted balance of account %s to %s" (Types.amountToString ettAmount) (showNamedAddress addr) (showNamedAddress ettReceiver)
    , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return $ Types.EncryptedAmountTransfer (naAddr ettReceiver) ettTransferData

-- |Convert 'baker add' transaction config into a valid payload,
-- optionally asking the user for confirmation.
bakerAddTransactionPayload :: BakerAddTransactionConfig -> FilePath -> Bool -> AccountKeyMap -> IO Types.Payload
bakerAddTransactionPayload batxCfg accountKeysFile confirm accKeys = do
  let BakerAddTransactionConfig
        { batcTransactionCfg = TransactionConfig{..}
        , batcBakerKeys = bakerKeys }
        = batxCfg

  logSuccess [ printf "submitting transaction to add baker using keys from file '%s'" accountKeysFile
             , printf "allowing up to %s to be spent as transaction fee" (showNrg tcEnergy) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  generateBakerAddPayload bakerKeys $ accountSigningDataFromConfig tcAccountCfg accKeys

-- |Convert 'baker remove' transaction config into a valid payload,
-- optionally asking the user for confirmation.
bakerRemoveTransactionPayload :: BakerRemoveTransactionConfig -> Bool -> IO Types.Payload
bakerRemoveTransactionPayload brtxCfg confirm = do
  let BakerRemoveTransactionConfig
        { brtcTransactionCfg = TransactionConfig
                               { tcEnergy = energy }
        , brtcBakerId = bid }
        = brtxCfg

  logSuccess [ printf "submitting transaction to remove baker with ID '%s'" (show bid)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return Types.RemoveBaker { rbId = bid }

-- |Convert 'consensus set-election-difficulty' transaction config into a valid payload,
-- optionally asking the user for confirmation.
setElectionDifficultyTransactionPayload :: SetElectionDifficultyTransactionConfig -> Bool -> IO Types.Payload
setElectionDifficultyTransactionPayload sdtxCfg confirm = do
  let SetElectionDifficultyTransactionConfig
        { sdtcTransactionCfg = TransactionConfig
                               { tcEnergy = energy }
        , sdtcDifficulty = d }
        = sdtxCfg

  logSuccess [ printf "setting election difficulty to '%f'" d
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , "note that only special control accounts are allowed to perform this operation"
             , "non-authorized transactions will be rejected" ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return Types.UpdateElectionDifficulty {uedDifficulty = d}

-- |Convert 'account delegate' transaction config into a valid payload,
-- optionally asking the user for confirmation.
accountDelegateTransactionPayload :: AccountDelegateTransactionConfig -> Bool -> IO Types.Payload
accountDelegateTransactionPayload adtxCfg confirm = do
  let AccountDelegateTransactionConfig
        { adtcTransactionCfg = TransactionConfig
                              { tcEnergy = energy
                              , tcExpiry = expiry
                              , tcAccountCfg = AccountConfig { acAddr = addr } }
        , adtcBakerId = bakerId }
        = adtxCfg

  logInfo [ printf "delegating stake from account %s to baker %s" (showNamedAddress addr) (show bakerId)
          , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
          , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return Types.DelegateStake { dsID = bakerId }

-- |Convert 'account undelegate' transaction config into a valid payload,
-- optionally asking the user for confirmation.
accountUndelegateTransactionPayload :: AccountUndelegateTransactionConfig -> Bool -> IO Types.Payload
accountUndelegateTransactionPayload autxCfg confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcAccountCfg = AccountConfig { acAddr = addr } }
        = autcTransactionCfg autxCfg

  logInfo [ printf "undelegating stake from account %s" (showNamedAddress addr)
          , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
          , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return Types.UndelegateStake

accountUpdateKeysTransactionPayload :: AccountUpdateKeysTransactionCfg -> Bool -> IO Types.Payload
accountUpdateKeysTransactionPayload AccountUpdateKeysTransactionCfg{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcAccountCfg = AccountConfig { acAddr = addr } }
        = auktcTransactionCfg

  let logKeyChanges = OrdMap.foldrWithKey (\idx (SigScheme.VerifyKeyEd25519 key) l -> (printf "\t%s: %s" (show idx) (show key)) : l) [] auktcKeys

  logInfo $
    [ printf "updating the following keys for account %s:" (showNamedAddress addr) ] ++
    logKeyChanges ++
    [ printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return $ Types.UpdateAccountKeys auktcKeys


accountAddKeysTransactionPayload :: AccountAddKeysTransactionCfg -> Bool -> IO Types.Payload
accountAddKeysTransactionPayload AccountAddKeysTransactionCfg{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcAccountCfg = AccountConfig { acAddr = addr } }
        = aaktcTransactionCfg

  let logKeyChanges = OrdMap.foldrWithKey (\idx (SigScheme.VerifyKeyEd25519 key) l -> (printf "\t%s: %s" (show idx) (show key)) : l) [] aaktcKeys
      logThresholdChange = case aaktcThreshold of
          Nothing -> []
          Just v -> [ printf "updating signature threshold for account to %d" (toInteger v) ]

  logInfo $
    [ printf "adding the following keys to account %s:" (showNamedAddress addr) ] ++
    logKeyChanges ++
    logThresholdChange ++
    [ printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return $ Types.AddAccountKeys aaktcKeys aaktcThreshold

accountRemoveKeysTransactionPayload :: AccountRemoveKeysTransactionCfg -> Bool -> IO Types.Payload
accountRemoveKeysTransactionPayload AccountRemoveKeysTransactionCfg{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcAccountCfg = AccountConfig { acAddr = addr } }
        = arktcTransactionCfg

  let logThresholdChange = case arktcThreshold of
        Nothing -> []
        Just v -> [ printf "updating signature threshold for account to %d" (toInteger v) ]

  logInfo $
    [ printf "removing keys at indices %s from account %s" (show $ Set.toList arktcIndices) (showNamedAddress addr) ] ++
    logThresholdChange ++
    [ printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return $ Types.RemoveAccountKeys arktcIndices arktcThreshold

accountEncryptTransactionPayload :: AccountEncryptTransactionConfig -> Bool -> IO Types.Payload
accountEncryptTransactionPayload AccountEncryptTransactionConfig{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcAccountCfg = AccountConfig { acAddr = addr } }
        = aeTransactionCfg


  logInfo $
    [ printf "transferring %s GTU from public to encrypted balance of account %s" (Types.amountToString aeAmount) (showNamedAddress addr)
    , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return $ Types.TransferToEncrypted aeAmount

accountDecryptTransactionPayload :: MonadIO m => AccountDecryptTransactionConfig -> Bool -> m Types.Payload
accountDecryptTransactionPayload AccountDecryptTransactionConfig{..} confirm = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcAccountCfg = AccountConfig { acAddr = addr } }
        = adTransactionCfg

  logInfo $
    [ printf "transferring %s GTU from encrypted to public balance of account %s" (Types.amountToString (Enc.stpatdTransferAmount adTransferData)) (showNamedAddress addr)
    , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
    , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]

  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  return $ Types.TransferToPublic adTransferData


-- |Encode, sign, and send transaction off to the baker.
-- If confirmNonce is set, the user is asked to confirm using the next nonce
-- if there are pending transactions.
startTransaction :: (MonadFail m, MonadIO m)
  => TransactionConfig
  -> Types.Payload
  -> Bool
  -> Maybe AccountKeyMap -- ^ The decrypted account signing keys. If not provided, the encrypted keys
                         -- from the 'TransactionConfig' will be used, and for each the password will be queried.
  -> ClientMonad m Types.BareBlockItem
startTransaction txCfg pl confirmNonce maybeAccKeys = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcNonce = n
        , tcAccountCfg = AccountConfig { acAddr = NamedAddress { .. }, .. }
        , ..
        } = txCfg
  nonce <- getNonce naAddr n confirmNonce
  accountKeyMap <- case maybeAccKeys of
                     Just acKeys' -> return acKeys'
                     Nothing -> liftIO $ failOnError $ decryptAccountKeyMapInteractive acKeys Nothing
  let tx = encodeAndSignTransaction pl naAddr energy nonce expiry accountKeyMap acThreshold

  sendTransactionToBaker tx defaultNetId >>= \case
    Left err -> fail err
    Right False -> fail "transaction not accepted by the baker"
    Right True -> return tx

-- |Fetch next nonces relative to the account's most recently committed and
-- pending transactions, respectively.
-- If they match, the nonce is returned.
-- If they don't match, optionally ask the user to confirm proceeding with the latter nonce.
-- If rejected, the process is cancelled (exit with code 0).
getNonce :: (MonadFail m, MonadIO m) => Types.AccountAddress -> Maybe Types.Nonce -> Bool -> ClientMonad m Types.Nonce
getNonce sender nonce confirm =
  case nonce of
    Nothing -> do
      currentNonce <- getBestBlockHash >>= getAccountNonce sender
      nextNonce <- nanNonce <$> (getNextAccountNonce (Text.pack $ show sender) >>= getFromJson)
      liftIO $ when (currentNonce /= nextNonce) $ do
        logWarn [ printf "there is a pending transaction with nonce %s, but last committed one has %s" (show $ nextNonce-1) (show $ currentNonce-1)
                , printf "this transaction will have nonce %s and might hang if the pending transaction fails" (show nextNonce) ]
        when confirm $ do
          putStrLn "Proceed if you're confident that all currently pending transactions are valid."
          confirmed <- askConfirmation $ Just "proceed"
          unless confirmed exitTransactionCancelled
      return nextNonce
    Just v -> return v

-- |Send a transaction and optionally tail it (see 'tailTransaction' below).
sendAndTailTransaction :: (MonadIO m, MonadFail m)
    => TransactionConfig -- ^ Information about the sender, and the context of transaction
    -> Types.Payload -- ^ Payload of the transaction to send
    -> InteractionOpts -- ^ How interactive should sending and tailing be
    -> ClientMonad m ()
sendAndTailTransaction txCfg pl intOpts = do
  tx <- startTransaction txCfg pl (ioConfirm intOpts) Nothing
  let hash = getBlockItemHash tx
  logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
  when (ioTail intOpts) $
    tailTransaction hash

-- |Continuously query and display transaction status until the transaction is finalized.
tailTransaction :: (MonadIO m) => Types.TransactionHash -> ClientMonad m ()
tailTransaction hash = do
  logInfo [ "waiting for the transaction to be committed and finalized"
          , "you may skip this step by interrupting the command using Ctrl-C (pass flag '--no-wait' to do this by default)"
          , printf "the transaction will still get processed and may be queried using\n  'transaction status %s'" (show hash) ]

  liftIO $ printf "[%s] Waiting for the transaction to be committed..." =<< getLocalTimeOfDayFormatted
  committedStatus <- awaitState 2 Committed hash
  liftIO $ putStrLn ""

  when (tsrState committedStatus == Absent) $
    logFatal [ "transaction failed before it got committed"
             , "most likely because it was invalid" ]

  runPrinter $ printTransactionStatus committedStatus

  -- If the transaction goes back to pending state after being committed
  -- to a branch which gets dropped later on, the command will currently
  -- keep presenting the transaction as committed and awaiting finalization.
  -- As the transaction is still expected to go back to being committed or
  -- (if for instance it expires) become absent, this seems acceptable
  -- from a UX perspective. Also, this is expected to be a very uncommon case.

  liftIO $ printf "[%s] Waiting for the transaction to be finalized..." =<< getLocalTimeOfDayFormatted
  finalizedStatus <- awaitState 5 Finalized hash
  liftIO $ putStrLn ""

  when (tsrState finalizedStatus == Absent) $
    logFatal [ "transaction failed after it was committed"
             , "response:\n" ++ showResponse committedStatus ]

  -- Print out finalized status if the outcome differs from that of the committed status.
  when (tsrResults committedStatus /= tsrResults finalizedStatus) $
    runPrinter $ printTransactionStatus finalizedStatus

  liftIO $ printf "[%s] Transaction finalized.\n" =<< getLocalTimeOfDayFormatted
  where
    getLocalTimeOfDayFormatted = showTimeOfDay <$> getLocalTimeOfDay
    showResponse = Text.unpack . decodeUtf8 . BSL8.toStrict . AE.encodePretty

-- |Process an 'account ...' command.
processAccountCmd :: AccountCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processAccountCmd action baseCfgDir verbose backend =
  case action of
    AccountShow address block -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      na <- getAccountAddressArg (bcAccountNameMap baseCfg) address
      v <- withClientJson backend $ withBestBlockHash block $ getAccountInfo (Text.pack $ show $ naAddr na)
      case v of
        Nothing -> putStrLn "Account not found."
        Just a -> runPrinter $ printAccountInfo na a verbose
    AccountList block -> do
      v <- withClientJson backend $ withBestBlockHash block getAccountList
      runPrinter $ printAccountList v
    AccountDelegate bakerId txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      adtxCfg <- getAccountDelegateTransactionCfg baseCfg txOpts bakerId getDelegateCostFunc
      let txCfg = adtcTransactionCfg adtxCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      let intOpts = toInteractionOpts txOpts
      pl <- accountDelegateTransactionPayload adtxCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    AccountUndelegate txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      autxCfg <- getAccountUndelegateTransactionCfg baseCfg txOpts getDelegateCostFunc
      let txCfg = autcTransactionCfg autxCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      let intOpts = toInteractionOpts txOpts
      pl <- accountUndelegateTransactionPayload autxCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    AccountUpdateKeys f txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      aukCfg <- getAccountUpdateKeysTransactionCfg baseCfg txOpts f
      let txCfg = auktcTransactionCfg aukCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      let intOpts = toInteractionOpts txOpts
      pl <- accountUpdateKeysTransactionPayload aukCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    AccountAddKeys f thresh txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      aakCfg <- getAccountAddKeysTransactionCfg baseCfg txOpts f thresh
      let txCfg = aaktcTransactionCfg aakCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      let intOpts = toInteractionOpts txOpts
      pl <- accountAddKeysTransactionPayload aakCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    AccountRemoveKeys idxs thresh txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True

      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      arkCfg <- getAccountRemoveKeysTransactionCfg baseCfg txOpts idxs thresh
      let txCfg = arktcTransactionCfg arkCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      let intOpts = toInteractionOpts txOpts
      pl <- accountRemoveKeysTransactionPayload arkCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    AccountEncrypt{..} -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      aetxCfg <- getAccountEncryptTransactionCfg baseCfg aeTransactionOpts aeAmount
      let txCfg = aeTransactionCfg aetxCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      let intOpts = toInteractionOpts aeTransactionOpts
      pl <- accountEncryptTransactionPayload aetxCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    AccountDecrypt{..} -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      let nrgCost _ = return $ Just accountDecryptEnergyCost
      txCfg <- liftIO (getTransactionCfg baseCfg adTransactionOpts nrgCost)

      encryptedSecretKey <- maybe (logFatal ["Missing account encryption secret key for account: " ++ show (acAddr . tcAccountCfg $ txCfg)]) return (acEncryptionKey . tcAccountCfg $ txCfg)
      secretKey <- either (\e -> logFatal ["Couldn't decrypt account encryption secret key: " ++ e]) return =<< decryptAccountEncryptionSecretKeyInteractive encryptedSecretKey

      withClient backend $ do
        adtxCfg <- getAccountDecryptTransactionCfg txCfg adAmount secretKey
        when verbose $ do
          runPrinter $ printAccountConfig $ tcAccountCfg txCfg
          liftIO $ putStrLn ""

        let intOpts = toInteractionOpts adTransactionOpts
        pl <- accountDecryptTransactionPayload adtxCfg (ioConfirm intOpts)
        sendAndTailTransaction txCfg pl intOpts


  where getDelegateCostFunc acc = withClient backend $ do
          when verbose $ logInfo ["retrieving instances"]
          info <- getBestBlockHash >>=
                  getAccountInfo (Text.pack $ show $ naAddr $ acAddr acc) >>=
                  getFromJson
          let is = length $ airInstances info
          when verbose $ logInfo [printf "retrieved %d instances" is]
          return $ Just $ accountDelegateEnergyCost is

-- |Process a 'module ...' command.
processModuleCmd :: ModuleCmd -> Verbose -> Backend -> IO ()
processModuleCmd action _ backend =
  case action of
    ModuleList block -> do
      v <- withClient backend $ withBestBlockHash block getModuleList >>= getFromJson
      runPrinter $ printModuleList v

-- |Process a 'contract ...' command.
processContractCmd :: ContractCmd -> Verbose -> Backend -> IO ()
processContractCmd action _ backend = putStrLn $ "Not yet implemented: " ++ show action ++ ", " ++ show backend

-- |Process a 'consensus ...' command.
processConsensusCmd :: ConsensusCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processConsensusCmd action baseCfgDir verbose backend =
  case action of
    ConsensusStatus -> do
      v <- withClientJson backend getConsensusStatus
      runPrinter $ printConsensusStatus v
    ConsensusShowParameters b includeBakers -> do
      v <- withClientJson backend $ withBestBlockHash b getBirkParameters
      case v of
        Nothing -> putStrLn "Block not found."
        Just p -> runPrinter $ printBirkParameters includeBakers p
    ConsensusSetElectionDifficulty d txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      sdtxCfg <- getSetElectionDifficultyTransactionCfg baseCfg txOpts d
      let txCfg = sdtcTransactionCfg sdtxCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      let intOpts = toInteractionOpts txOpts
      pl <- setElectionDifficultyTransactionPayload sdtxCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

-- |Process a 'block ...' command.
processBlockCmd :: BlockCmd -> Verbose -> Backend -> IO ()
processBlockCmd action _ backend =
  case action of
    BlockShow b -> do
      when (maybe False (isNothing . parseTransactionHash) b) $
        logFatal [printf "invalid block hash '%s'" (fromJust b)]

      v <- withClientJson backend $ withBestBlockHash b getBlockInfo
      runPrinter $ printBlockInfo v

-- |Generate a fresh set of baker keys.
generateBakerKeys :: IO BakerKeys
generateBakerKeys = do
  -- Aggr/bls keys.
  aggrSk <- Bls.generateSecretKey
  let aggrPk = Bls.derivePublicKey aggrSk
  -- Election keys.
  VRF.KeyPair {privateKey=elSk, publicKey=elPk} <- VRF.newKeyPair
  -- Signature keys.
  BlockSig.KeyPair {signKey=sigSk, verifyKey=sigVk} <- BlockSig.newKeyPair
  return BakerKeys { bkAggrSignKey = aggrSk
                   , bkAggrVerifyKey = aggrPk
                   , bkElectionSignKey = elSk
                   , bkElectionVerifyKey = elPk
                   , bkSigSignKey = sigSk
                   , bkSigVerifyKey = sigVk }

-- |Process a 'baker ...' command.
processBakerCmd :: BakerCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processBakerCmd action baseCfgDir verbose backend =
  case action of
    BakerGenerateKeys outputFile -> do
      keys <- generateBakerKeys
      let out = AE.encodePretty keys

      case outputFile of
        Nothing -> do
          -- TODO Store in config.
          BSL8.putStrLn out
          logInfo [ printf "to add a baker to the chain using these keys, store it in a file and use 'baker add FILE'" ]
        Just f -> do
          BSL.writeFile f out
          logSuccess [ printf "keys written to file '%s'" f
                     , "DO NOT LOSE THIS FILE"
                     , printf "to add a baker to the chain using these keys, use 'baker add %s'" f ]
    BakerAdd accountKeysFile txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      batxCfg <- getBakerAddTransactionCfg baseCfg txOpts accountKeysFile
      let txCfg = batcTransactionCfg batxCfg
      when verbose $ do
        runPrinter $ printAccountConfig $ tcAccountCfg txCfg
        putStrLn ""

      -- We already need the account keys to construct the proofs to be included in the transaction.
      accountKeys <- failOnError $ decryptAccountKeyMapInteractive (acKeys $ tcAccountCfg txCfg) Nothing

      let intOpts = toInteractionOpts txOpts
      pl <- bakerAddTransactionPayload batxCfg accountKeysFile (ioConfirm intOpts) accountKeys
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts) (Just accountKeys)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "baker successfully added" ]
    BakerSetAccount bid accRef txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      (_, rewardAccCfg) <- getAccountConfig (Just accRef) baseCfg Nothing Nothing Nothing False
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      rewardAccountKeys <- failOnError $ decryptAccountKeyMapInteractive (acKeys rewardAccCfg) (Just "reward account")
      let rewardAccountSigningData = accountSigningDataFromConfig rewardAccCfg rewardAccountKeys

      bsatcCfg <- getBakerSetAccountTransactionCfg baseCfg txOpts bid rewardAccountSigningData
      let txCfg = bsatcTransactionCfg bsatcCfg

      let intOpts = toInteractionOpts txOpts
      pl <- bakerSetAccountTransactionPayload bsatcCfg (ioConfirm intOpts) rewardAccountKeys
      withClient backend $ do
        -- We still have to ask for the password of the account keys for signing the transaction.
        -- Possible improvement: check if reward account and transaction signing account are the same,
        -- and in that case do not ask twice.
        tx <- startTransaction txCfg pl (ioConfirm intOpts) Nothing
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "baker reward account successfully updated" ]
    BakerSetKey bid file txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      bsktcCfg <- getBakerSetKeyTransactionCfg baseCfg txOpts bid file
      let txCfg = bsktcTransactionCfg bsktcCfg

      let intOpts = toInteractionOpts txOpts
      pl <- bakerSetKeyTransactionPayload bsktcCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    BakerRemove bid txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      brtcCfg <- getBakerRemoveTransactionCfg baseCfg txOpts bid
      let txCfg = brtcTransactionCfg brtcCfg

      let intOpts = toInteractionOpts txOpts
      pl <- bakerRemoveTransactionPayload brtcCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    BakerSetAggregationKey bid file txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      bsaktCfg <- getBakerSetAggregationKeyCfg baseCfg txOpts bid file
      let intOpts = toInteractionOpts txOpts
      let txCfg = bsakTransactionCfg bsaktCfg

      pl <- bakerSetAggregationKeyTransactionPayload bsaktCfg (ioConfirm intOpts)
      withClient backend $ sendAndTailTransaction txCfg pl intOpts

    BakerSetElectionKey bid file txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      let intOpts = toInteractionOpts txOpts
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      bsektCfg <- getBakerSetElectionKeyCfg baseCfg txOpts bid file
      pl <- bakerSetElectionKeyTransactionPayload bsektCfg (ioConfirm intOpts)

      withClient backend $ do
        tx <- startTransaction (bsekTransactionCfg bsektCfg) pl (ioConfirm intOpts) Nothing
        tailTransaction $ getBlockItemHash tx
--          logSuccess [ "baker election key successfully updated" ]


-- |Resolve configuration of a 'baker update' transaction based on persisted config and CLI flags.
-- See the docs for getTransactionCfg for the behavior when no or a wrong amount of energy is allocated.
getBakerSetAccountTransactionCfg :: BaseConfig -> TransactionOpts -> Types.BakerId -> AccountSigningData -> IO BakerSetAccountTransactionConfig
getBakerSetAccountTransactionCfg baseCfg txOpts bid asd = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  return BakerSetAccountTransactionConfig
    { bsatcTransactionCfg = txCfg
    , bsatcBakerId = bid
    , bsatcAccountSigningData = asd }
  where nrgCost _ = return $ Just bakerSetAccountEnergyCost

-- |Resolve configuration of a 'consensus set-election-difficulty' transaction based on persisted config and CLI flags.
-- See the docs for getTransactionCfg for the behavior when no or a wrong amount of energy is allocated.
getSetElectionDifficultyTransactionCfg :: BaseConfig -> TransactionOpts -> Types.ElectionDifficulty -> IO SetElectionDifficultyTransactionConfig
getSetElectionDifficultyTransactionCfg baseCfg txOpts d = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  when (d < 0 || d >= 1) $
    logFatal ["difficulty is out of range"]
  return SetElectionDifficultyTransactionConfig
    { sdtcTransactionCfg = txCfg
    , sdtcDifficulty = d }
  where nrgCost _ = return $ Just setElectionDifficultyEnergyCost

getBakerSetKeyTransactionCfg :: BaseConfig -> TransactionOpts -> Types.BakerId -> FilePath -> IO BakerSetKeyTransactionConfig
getBakerSetKeyTransactionCfg baseCfg txOpts bid f = do
  kp <- eitherDecodeFileStrict f >>= getFromJson
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  return BakerSetKeyTransactionConfig
    { bsktcTransactionCfg = txCfg
    , bsktcBakerId = bid
    , bsktcKeyPair = kp }
  where nrgCost _ = return $ Just bakerSetKeyEnergyCost

getBakerSetAggregationKeyCfg :: BaseConfig -> TransactionOpts -> Types.BakerId -> FilePath -> IO BakerSetAggregationKeyTransactionConfig
getBakerSetAggregationKeyCfg baseCfg txOpts bid file = do
  eitherDecodeFileStrict file >>= \case
    Left err -> logFatal [printf "Cannot read supplied file: %s" err]
    Right v -> do
      let parser =
            withObject "Baker aggregation keys:" $ \obj -> do
              aggSignKey <- obj .: "aggregationSignKey"
              aggVerifyKey <- obj .: "aggregationVerifyKey"
              return (aggSignKey, aggVerifyKey)
      case parse parser v of
        AE.Error err -> logFatal [printf "Cannot read aggregation keys: %s" err]
        AE.Success (bsakSecretKey, bsakPublicKey) -> do
          txCfg <- getTransactionCfg baseCfg txOpts nrgCost
          return BakerSetAggregationKeyTransactionConfig
              { bsakTransactionCfg = txCfg
              , bsakBakerId = bid
              , ..
              }
  where nrgCost _ = return $ Just bakerSetAggregationKeyEnergyCost

getBakerSetElectionKeyCfg :: BaseConfig -> TransactionOpts -> Types.BakerId -> FilePath -> IO BakerSetElectionKeyTransactionConfig
getBakerSetElectionKeyCfg baseCfg txOpts bid file = do
  kp :: VRF.KeyPair <- eitherDecodeFileStrict file >>= getFromJson
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  return BakerSetElectionKeyTransactionConfig
    { bsekTransactionCfg = txCfg
    , bsekBakerId = bid
    , bsekKeyPair = kp }
  where nrgCost _ = return $ Just bakerSetElectionKeyEnergyCost

generateBakerSetAccountChallenge :: Types.BakerId -> Types.AccountAddress -> BS.ByteString
generateBakerSetAccountChallenge bid add = S.runPut (S.put bid <> S.put add)

generateBakerSetKeyChallenge :: Types.BakerId -> Types.BakerSignVerifyKey -> BS.ByteString
generateBakerSetKeyChallenge bid key= S.runPut (S.put bid <> S.put key)

generateBakerSetAggregationKeyCallenge :: Types.BakerId -> Types.BakerAggregationVerifyKey -> BS.ByteString
generateBakerSetAggregationKeyCallenge bid key = S.runPut (S.put bid <> S.put key)

generateBakerSetElectionKeyChallenge :: Types.BakerId -> Types.BakerElectionVerifyKey -> BS.ByteString
generateBakerSetElectionKeyChallenge bid key = S.runPut (S.put bid <> S.put key)

-- |Convert 'baker set-account' transaction config into a valid payload,
-- optionally asking the user for confirmation.
bakerSetAccountTransactionPayload :: BakerSetAccountTransactionConfig -> Bool -> AccountKeyMap -> IO Types.Payload
bakerSetAccountTransactionPayload bsatcCfg confirm accKeys = do
  let BakerSetAccountTransactionConfig
        { bsatcTransactionCfg = TransactionConfig
                                { tcEnergy = energy
                                , tcExpiry = expiry }
        , bsatcAccountSigningData = AccountSigningData{..}
        , bsatcBakerId = bid }
        = bsatcCfg
      challenge = generateBakerSetAccountChallenge bid asdAddress

  logSuccess [ printf "setting reward account for baker %s to '%s'"  (show bid) (show asdAddress)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  proofAccount <- forM accKeys (\key -> Proofs.proveDlog25519KP challenge key `except` "cannot produce account keys proof")
  return Types.UpdateBakerAccount
    { ubaId = bid
    , ubaAddress = asdAddress
    , ubaProof = Types.AccountOwnershipProof (Map.toList proofAccount) }
  where except c err = c >>= \case
          Just x -> return x
          Nothing -> logFatal [err]

-- |Convert 'baker set-key' transaction config into a valid payload.
bakerSetKeyTransactionPayload :: BakerSetKeyTransactionConfig -> Bool -> IO Types.Payload
bakerSetKeyTransactionPayload bsktcCfg confirm = do
  let BakerSetKeyTransactionConfig
        { bsktcTransactionCfg = TransactionConfig
                                { tcEnergy = energy
                                , tcExpiry = expiry }
        , bsktcBakerId = bid
        , bsktcKeyPair = kp }
        = bsktcCfg

  logSuccess [ printf "setting key pair %s for baker %s" (show kp) (show bid)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  let ubsId = bsktcBakerId bsktcCfg
      BlockSig.KeyPair { signKey=sk, verifyKey=ubsKey } = kp
      challenge = generateBakerSetKeyChallenge ubsId ubsKey

  ubsProof <- Proofs.proveDlog25519Block challenge (BlockSig.KeyPair sk ubsKey) `except` "cannot produce signature key proof"

  return Types.UpdateBakerSignKey {..}
  where except c err = c >>= \case
          Just x -> return x
          Nothing -> logFatal [err]

-- |Convert 'baker set-aggregation-key' transaction config into a valid payload
bakerSetAggregationKeyTransactionPayload :: BakerSetAggregationKeyTransactionConfig -> Bool -> IO Types.Payload
bakerSetAggregationKeyTransactionPayload bsaktCfg confirm = do
  let BakerSetAggregationKeyTransactionConfig
        { bsakTransactionCfg = TransactionConfig
                                { tcEnergy = energy
                                , tcExpiry = expiry }
        , bsakBakerId = bid
        , bsakSecretKey = secretKey
        , bsakPublicKey = ubavkKey }
        = bsaktCfg

  logSuccess [ printf "setting aggregation key pair %s for baker %s" (show (secretKey, ubavkKey)) (show bid)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  let ubavkId = bid
      challenge = generateBakerSetAggregationKeyCallenge bid ubavkKey
      ubavkProof = Bls.proveKnowledgeOfSK challenge secretKey

  return Types.UpdateBakerAggregationVerifyKey {..}

-- |Conver 'baker set-election-key' transaction config into a valid payload
bakerSetElectionKeyTransactionPayload :: BakerSetElectionKeyTransactionConfig -> Bool -> IO Types.Payload
bakerSetElectionKeyTransactionPayload bsektCfg confirm = do
  let BakerSetElectionKeyTransactionConfig
        { bsekTransactionCfg = TransactionConfig
                                { tcEnergy = energy
                                , tcExpiry = expiry }
        , bsekBakerId = bid
        , bsekKeyPair = kp } = bsektCfg

  logSuccess [ printf "setting election key pair (private: %s, public: %s) for baker %s"
                          (show $ VRF.privateKey kp) (show $ VRF.publicKey kp)  (show bid)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  let ubekId = bid
      ubekKey = VRF.publicKey kp
      challenge = generateBakerSetElectionKeyChallenge bid ubekKey

  ubekProof <- Proofs.proveDlog25519VRF challenge kp `except` "cannot produce VRF key proof"

  return Types.UpdateBakerElectionKey {..}
    where
      except c err = c >>= \case
        Just x -> return x
        Nothing -> logFatal [err]

processIdentityCmd :: IdentityCmd -> Backend -> IO ()
processIdentityCmd action backend =
  case action of
    IdentityShow c -> processIdentityShowCmd c backend

processIdentityShowCmd :: IdentityShowCmd -> Backend -> IO ()
processIdentityShowCmd action backend =
  case action of
    IdentityShowIPs block -> do
      v <- withClientJson backend $ withBestBlockHash block $ getIdentityProviders
      case v of
        Nothing -> putStrLn "No response received from the gRPC server."
        Just a -> runPrinter $ printIdentityProviders a
    IdentityShowARs block -> do
      v <- withClientJson backend $ withBestBlockHash block $ getAnonymityRevokers
      case v of
        Nothing -> putStrLn "No response received from the gRPC server."
        Just a -> runPrinter $ printAnonymityRevokers a

-- |Process a "legacy" command.
processLegacyCmd :: LegacyCmd -> Backend -> IO ()
processLegacyCmd action backend =
  case action of
    SendTransaction fname nid -> do
      source <- BSL.readFile fname
      t <- withClient backend $ processTransaction source nid
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (getBlockItemHash t)
    GetConsensusInfo -> withClient backend $ getConsensusStatus >>= printJSON
    GetBlockInfo every block -> withClient backend $ withBestBlockHash block getBlockInfo >>= if every then loop else printJSON
    GetBlockSummary block -> withClient backend $ withBestBlockHash block getBlockSummary >>= printJSON
    GetBlocksAtHeight height -> withClient backend $ getBlocksAtHeight height >>= printJSON
    GetAccountList block -> withClient backend $ withBestBlockHash block getAccountList >>= printJSON
    GetInstances block -> withClient backend $ withBestBlockHash block getInstances >>= printJSON
    GetTransactionStatus txhash -> withClient backend $ getTransactionStatus txhash >>= printJSON
    GetTransactionStatusInBlock txhash block -> withClient backend $ getTransactionStatusInBlock txhash block >>= printJSON
    GetAccountInfo account block ->
      withClient backend $ withBestBlockHash block (getAccountInfo account) >>= printJSON
    GetAccountNonFinalized account ->
      withClient backend $ getAccountNonFinalizedTransactions account >>= printJSON
    GetNextAccountNonce account ->
      withClient backend $ getNextAccountNonce account >>= printJSON
    GetInstanceInfo account block ->
      withClient backend $ withBestBlockHash block (getInstanceInfo account) >>= printJSON
    GetRewardStatus block -> withClient backend $ withBestBlockHash block getRewardStatus >>= printJSON
    GetBirkParameters block ->
      withClient backend $ withBestBlockHash block getBirkParameters >>= printJSON
    GetModuleList block -> withClient backend $ withBestBlockHash block getModuleList >>= printJSON
    GetNodeInfo -> withClient backend $ getNodeInfo >>= printNodeInfo
    GetPeerData bootstrapper -> withClient backend $ getPeerData bootstrapper >>= printPeerData
    StartBaker -> withClient backend $ startBaker >>= printSuccess
    StopBaker -> withClient backend $ stopBaker >>= printSuccess
    PeerConnect ip port -> withClient backend $ peerConnect ip port >>= printSuccess
    GetPeerUptime -> withClient backend $ getPeerUptime >>= (liftIO . print)
    BanNode nodeId nodePort nodeIp -> withClient backend $ banNode nodeId nodePort nodeIp >>= printSuccess
    UnbanNode nodeId nodePort nodeIp -> withClient backend $ unbanNode nodeId nodePort nodeIp >>= printSuccess
    JoinNetwork netId -> withClient backend $ joinNetwork netId >>= printSuccess
    LeaveNetwork netId -> withClient backend $ leaveNetwork netId >>= printSuccess
    GetAncestors amount blockHash -> withClient backend $ withBestBlockHash blockHash (getAncestors amount) >>= printJSON
    GetBranches -> withClient backend $ getBranches >>= printJSON
    GetBannedPeers -> withClient backend $ getBannedPeers >>= (liftIO . print)
    Shutdown -> withClient backend $ shutdown >>= printSuccess
    DumpStart -> withClient backend $ dumpStart >>= printSuccess
    DumpStop -> withClient backend $ dumpStop >>= printSuccess
    GetIdentityProviders block -> withClient backend $ withBestBlockHash block getIdentityProviders >>= printJSON
    GetAnonymityRevokers block -> withClient backend $ withBestBlockHash block getAnonymityRevokers >>= printJSON
  where
    printSuccess (Left x)  = liftIO . putStrLn $ x
    printSuccess (Right x) = liftIO $ if x then putStrLn "OK" else putStrLn "FAIL"

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
            Just (AE.Number x) | x > 0 ->
              getBlockInfo parent >>= loop
            _ -> return () -- Genesis block reached.
        _ -> error "Unexpected return value for block parent."
    _ -> error "Unexptected return value for getBlockInfo."

-- |Helper function to specialize the type, avoiding the need for type
-- annotations in many places.
getBlockItemHash :: Types.BareBlockItem -> Types.TransactionHash
getBlockItemHash = getHash

data PeerData = PeerData {
  totalSent     :: Word64,
  totalReceived :: Word64,
  version       :: Text,
  peerStats     :: PeerStatsResponse,
  peerList      :: PeerListResponse
  }

generateBakerAddChallenge :: Types.BakerElectionVerifyKey -> Types.BakerSignVerifyKey -> Types.BakerAggregationVerifyKey -> Types.AccountAddress -> BS.ByteString
generateBakerAddChallenge electionVerifyKey sigVerifyKey aggrVerifyKey accountAddr =
  S.runPut (S.put electionVerifyKey <> S.put sigVerifyKey <> S.put aggrVerifyKey <> S.put accountAddr)

-- | Generate a 'Types.AddBaker' payload. This needs the account keys to create proofs.
generateBakerAddPayload :: BakerKeys -> AccountSigningData -> IO Types.Payload
generateBakerAddPayload bk AccountSigningData{..} = do
  let electionSignKey = bkElectionSignKey bk
      signatureSignKey = bkSigSignKey bk
      aggrSignKey = bkAggrSignKey bk

  let abElectionVerifyKey = bkElectionVerifyKey bk
      abSignatureVerifyKey = bkSigVerifyKey bk
      abAggregationVerifyKey = bkAggrVerifyKey bk

  let abAccount = asdAddress
      keyMap = asdKeys

  let challenge = generateBakerAddChallenge abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey abAccount
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
      putStrLn $ "Peer version: " ++ Text.unpack version
      putStrLn "Peer stats:"
      forM_ (peerStats ^. CF.peerstats) $ \ps -> do
        putStrLn $ "  Peer: " ++ Text.unpack (ps ^. CF.nodeId)
        putStrLn $ "    Packets sent: " ++ show (ps ^. CF.packetsSent)
        putStrLn $ "    Packets received: " ++ show (ps ^. CF.packetsReceived)
        putStrLn $ "    Latency: " ++ show (ps ^. CF.latency)
        putStrLn ""

      putStrLn $ "Peer type: " ++ Text.unpack (peerList ^. CF.peerType)
      putStrLn "Peers:"
      forM_ (peerList ^. CF.peers) $ \pe -> do
        putStrLn $ "  Node id: " ++ Text.unpack (pe ^. CF.nodeId . CF.value)
        putStrLn $ "    Port: " ++ show (pe ^. CF.port . CF.value)
        putStrLn $ "    IP: " ++ Text.unpack (pe ^. CF.ip . CF.value)
        putStrLn $ "    Catchup Status: " ++ showCatchupStatus (pe ^. CF.catchupStatus)
        putStrLn ""
  where showCatchupStatus =
          \case PeerElement'UPTODATE -> "Up to date"
                PeerElement'PENDING -> "Pending"
                PeerElement'CATCHINGUP -> "Catching up"
                _ -> "Unknown" -- this should not happen in well-formed responses


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
      putStrLn $ "Node ID: " ++ show (ni ^. CF.nodeId . CF.value)
      putStrLn $ "Current local time: " ++ show (ni ^. CF.currentLocaltime)
      putStrLn $ "Baker ID: " ++ maybe "not a baker" show (ni ^? CF.maybe'consensusBakerId . _Just . CF.value)
      putStrLn $ "Peer type: " ++ show (ni ^. CF.peerType)
      putStrLn $ "Baker running: " ++ show (ni ^. CF.consensusBakerRunning)
      putStrLn $ "Consensus running: " ++ show (ni ^. CF.consensusRunning)
      putStrLn $ "Consensus type: " ++ show (ni ^. CF.consensusType)
      putStrLn $ "Baker committee member: " ++ show (ni ^. CF.consensusBakerCommittee)
      putStrLn $ "Finalization committee member: " ++ show (ni ^. CF.consensusFinalizerCommittee)

-- |FIXME: Move this some other place in refactoring.
data StatusOfPeers = StatusOfPeers {
  -- |How many peers we deem are up-to-date with us.
  numUpToDate :: !Int,
  -- |How many are in limbo, we don't know what the status is with respect to
  -- consensus status of the both of us.
  numPending :: !Int,
  -- |Number of peers we are catching up with.
  numCatchingUp :: !Int
  } deriving(Show, Generic)

instance ToJSON StatusOfPeers
instance FromJSON StatusOfPeers

-- |Get an indication of how caught up the node is in relation to its peers.
getStatusOfPeers :: ClientMonad IO (Either String StatusOfPeers)
getStatusOfPeers = do
  -- False means we don't include the bootstrap nodes here, since they are not running consensus.
  getPeerList False <&> \case
    Left err -> (Left err)
    Right peerList -> Right $
      L.foldl' (\status peerElem ->
                  case peerElem ^. CF.catchupStatus of
                    PeerElement'UPTODATE -> status { numUpToDate = numUpToDate status + 1 }
                    PeerElement'PENDING -> status { numPending = numPending status + 1 }
                    PeerElement'CATCHINGUP -> status { numCatchingUp = numCatchingUp status + 1 }
                    _ -> status -- this should not happen in well-formed responses
               )
          (StatusOfPeers 0 0 0)
          (peerList ^. CF.peers)


-- |Process a transaction from JSON payload given as a byte string
-- and with keys given explicitly.
-- The transaction is signed with all the provided keys.
processTransaction ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> ClientMonad m Types.BareBlockItem
processTransaction source networkId =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t  -> processTransaction_ t networkId True

-- |Process a transaction with unencrypted keys given explicitly.
-- The transaction is signed with all the provided keys.
-- This is only for testing purposes and currently used by the middleware.
processTransaction_ ::
     (MonadFail m, MonadIO m)
  => TransactionJSON
  -> Int
  -> Verbose
  -> ClientMonad m Types.BareBlockItem
processTransaction_ transaction networkId _verbose = do
  let accountKeys = CT.keys transaction
  tx <- do
    let header = metadata transaction
        sender = thSenderAddress header
        threshold = fromIntegral $ Map.size accountKeys
    nonce <-
      case thNonce header of
        Nothing -> getBestBlockHash >>= getAccountNonce sender
        Just nonce -> return nonce
    txPayload <- convertTransactionJsonPayload $ payload transaction
    return $ encodeAndSignTransaction
      txPayload
      sender
      (thEnergyAmount header)
      nonce
      (thExpiry header)
      accountKeys
      threshold

  sendTransactionToBaker tx networkId >>= \case
    Left err -> fail $ show err
    Right False -> fail "Transaction not accepted by the baker."
    Right True -> return tx

-- |Read a versioned credential from the bytestring, failing if any errors occur.
processCredential ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> ClientMonad m Types.BareBlockItem
processCredential source networkId =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right vCred
        | vVersion vCred == 0 ->
            case fromJSON (vValue vCred) of
              AE.Success cred ->
                let tx = Types.CredentialDeployment cred
                in sendTransactionToBaker tx networkId >>= \case
                  Left err -> fail err
                  Right False -> fail "Transaction not accepted by the baker."
                  Right True -> return tx
              AE.Error err -> fail $ "Cannot parse credential according to V0: " ++ err
        | otherwise ->
          fail $ "Unsupported credential version: " ++ show (vVersion vCred)

-- |Convert JSON-based transaction type to one which is ready to be encoded, signed and sent.
convertTransactionJsonPayload :: (MonadFail m) => CT.TransactionJSONPayload -> ClientMonad m Types.Payload
convertTransactionJsonPayload = \case
  (CT.DeployModule _) ->
    fail "DeployModule is not implemented"
  (CT.InitContract _ _ _ _) ->
    fail "InitContract is not implemented"
  (CT.Update _ _ _ _) ->
    fail "Update is not implemented"
  (CT.Transfer transferTo transferAmount) ->
    return $ Types.Transfer transferTo transferAmount
  (CT.RemoveBaker rbid) -> return $ Types.RemoveBaker rbid
  (CT.UpdateBakerAccount ubid uba ubp) ->
    return $ Types.UpdateBakerAccount ubid uba ubp
  (CT.UpdateBakerSignKey ubsid ubsk ubsp) ->
    return $ Types.UpdateBakerSignKey ubsid ubsk ubsp
  (CT.DelegateStake dsid) -> return $ Types.DelegateStake dsid
  (CT.UpdateElectionDifficulty d) -> return $ Types.UpdateElectionDifficulty d
  CT.TransferToEncrypted{..} -> return $ Types.TransferToEncrypted{..}
  CT.TransferToPublic{..} -> return $ Types.TransferToPublic{..}
  CT.EncryptedAmountTransfer{..} -> return Types.EncryptedAmountTransfer{..}

-- |Sign a transaction payload and configuration into a "normal" transaction,
-- which is ready to be sent.
encodeAndSignTransaction ::
     Types.Payload
  -> Types.AccountAddress
  -> Types.Energy
  -> Types.Nonce
  -> Types.TransactionExpiryTime
  -> AccountKeyMap
  -> ID.SignatureThreshold
  -> Types.BareBlockItem
encodeAndSignTransaction txPayload sender energy nonce expiry accKeys threshold = Types.NormalTransaction $
  let encPayload = Types.encodePayload txPayload
      header = Types.TransactionHeader{
        thSender = sender,
        thPayloadSize = Types.payloadSize encPayload,
        thNonce = nonce,
        thEnergyAmount = energy,
        thExpiry = expiry
      }
      keys = take (fromIntegral threshold) . sortOn fst . Map.toList $ accKeys
  in Types.signTransaction keys header encPayload
