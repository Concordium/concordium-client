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
  , decodeWebFormattedAccountExport
  , accountCfgsFromWalletExportAccounts
  , decodeMobileFormattedAccountExport
  -- * For adding baker support
  , startTransaction
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

import qualified Acorn.Core                          as Core
import qualified Acorn.Core.PrettyPrint              as PP
import qualified Acorn.Parser.Runner                 as PR

import           Concordium.Client.Cli
import           Concordium.Client.Config
import           Concordium.Client.Commands          as COM
import           Concordium.Client.GRPC
import           Concordium.Client.Output
import           Concordium.Client.Parse
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction as CT
import           Concordium.Client.Types.TransactionStatus
import           Concordium.Client.WalletExport
import qualified Concordium.Crypto.BlockSignature    as BlockSig
import qualified Concordium.Crypto.BlsSignature      as Bls
import qualified Concordium.Crypto.Proofs            as Proofs
import           Concordium.Crypto.SignatureScheme   as Sig
import qualified Concordium.Crypto.VRF               as VRF
import qualified Concordium.Types.Transactions       as Types
import           Concordium.Types.HashableTo
import qualified Concordium.Types.Execution          as Types
import qualified Concordium.Types                    as Types
import           Concordium.ID.Types                  (addressFromText)
import           Proto.ConcordiumP2pRpc
import qualified Proto.ConcordiumP2pRpc_Fields       as CF

import           Control.Monad.Fail
import           Control.Monad.Except
import           Control.Monad.Reader                hiding (fail)
import           Control.Exception
import           GHC.Generics
import           Data.IORef
import           Data.Aeson                          as AE
import           Data.Aeson.Types
import qualified Data.Aeson.Encode.Pretty            as AE
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.ByteString.Lazy.Char8          as BSL8
import qualified Data.Char                           as C
import qualified Data.HashMap.Strict                 as Map
import           Data.Maybe
import           Data.List                           as L
import qualified Data.Serialize                      as S
import           Data.String
import           Data.Text                           hiding (take)
import           Data.Text.Encoding
import qualified Data.Text.IO                        as TextIO hiding (putStrLn)
import           Data.Word
import           Lens.Micro.Platform
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client.Exceptions
import           Prelude                             hiding (fail, mod, unlines)
import           System.IO
import           System.Directory
import           System.FilePath
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
    let config = GrpcConfig (COM.grpcHost bkend) (COM.grpcPort bkend) (COM.grpcTarget bkend) (COM.grpcRetryNum bkend) Nothing
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

-- |Helper function for parsing a specific entry of a JSON object to any type that implements
-- FromJSON. Useful for parsing specific keys from a Baker Keys file.
getFromJsonObject :: (MonadIO m, FromJSON a) => Text -> Either String Value -> m a
getFromJsonObject key r = do
  obj <- case r of
         Left err -> logFatal [printf "I/O error: %s" err]
         Right (Object o) -> return o
         Right s -> logFatal [printf "error parsing JSON, expected object but found: %s" $ show s]
  case parse (\o -> o .: key) obj of
    Success k -> return k
    Error err -> logFatal [printf "I/O error: %s" err]

-- |Look up account from the provided name (or defaultAcccountName if missing).
-- Fail if it cannot be found.
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

-- |Validate an account name and fail gracefully if the given account name
-- is invalid.
readAccountName :: Maybe Text -> IO (Maybe Text)
readAccountName Nothing = return Nothing
readAccountName (Just n) =
    case validateAccountName n of
      Left err -> logFatal [printf "cannot parse '%s' as an address name: %s" n err]
      Right v -> return $ Just v

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
      ConfigAccountAdd addr name -> do
        baseCfg <- getBaseConfig baseCfgDir verbose True
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        naAddr <-
          case addressFromText addr of
            Left err -> logFatal [printf "cannot parse '%s' as an address: %s" addr err]
            Right a -> return a
        naName <- readAccountName name
        void $ initAccountConfig baseCfg NamedAddress{..}
      ConfigAccountImport file name format -> do
        baseCfg <- getBaseConfig baseCfgDir verbose True
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        validName <- readAccountName name

        fileExists <- doesFileExist file
        unless fileExists $ logFatal [printf "the given file '%s' does not exist" file]

        importFormat <- inferAccountImportFormat format file >>= askPassword

        accCfgs <- loadAccountImportFile importFormat file validName
        void $ importAccountConfig baseCfg accCfgs


    ConfigKeyCmd c -> case c of
      ConfigKeyAdd addr idx sk vk -> do
        baseCfg <- getBaseConfig baseCfgDir verbose True
        when verbose $ do
          runPrinter $ printBaseConfig baseCfg
          putStrLn ""

        kp <- parseKeyPair sk vk
        (baseCfg', accCfg) <- getAccountConfig (Just addr) baseCfg Nothing Nothing True
        let keys = insertAccountKey idx kp $ acKeys accCfg
            accCfg' = accCfg { acKeys = keys }

        writeAccountKeys baseCfg' accCfg'
  where askPassword = \case
          Web -> return WebFormat
          Mobile -> do
            putStr "Enter encryption password: "
            mfPassword <- bracket_ (hSetEcho stdin False) (hSetEcho stdin True) BS.getLine
            putStrLn ""
            return MobileFormat {..}

-- |Return the input format or infer it from the file extension if it's Nothing.
inferAccountImportFormat :: Maybe AccountImportFormatOpt -> FilePath -> IO AccountImportFormatOpt
inferAccountImportFormat input file =
  case input of
    Nothing -> case splitExtension file of
                 (_, ".concordiumwallet") -> do
                   logInfo ["inferred format \"mobile\" from the file extension (pass flag '--format web' to override)"]
                   return Mobile
                 _ -> do
                   logInfo ["inferred format \"web\" from the file extension (pass flag '--format mobile' to override)"]
                   return Web
    Just f -> return f

data AccountImportFormat = WebFormat | MobileFormat { mfPassword :: BS.ByteString }


-- |Read and parse a file exported from either the web- or mobile wallet.
-- The format specifier tells which format to expect.
-- If the format is "mobile", the user is prompted for a password which is used to decrypt
-- the exported data. This may result in multiple named accounts. If a name is provided,
-- only the account with that name is being selected for import.
-- The "web" format is not encrypted and only contains a single account which is not named.
-- If a name is provided in this case, this will become the account name.
loadAccountImportFile :: AccountImportFormat -> FilePath -> Maybe Text -> IO [AccountConfig]
loadAccountImportFile format file name = do
  contents <- BS.readFile file
  case format of
    MobileFormat password -> do
      accs <- case decodeMobileFormattedAccountExport contents password of
        Left err -> logFatal [printf "cannot load mobile formatted import: %s" err]
        Right v -> return v

      logInfo ["loaded account(s):"]
      forM_ accs $ \a -> do
        logInfo [printf "- %s (%s)" (show $ akAddress $ weaKeys a) (weaName a)]

      let accCfgs = accountCfgsFromWalletExportAccounts accs name
      when (Prelude.null accCfgs) $ logWarn ["no accounts welected for import"]
      return accCfgs
    WebFormat -> do
     accCfg <- case decodeWebFormattedAccountExport contents name of
       Left err -> logFatal [printf "cannot load web formatted import %s: "err]
       Right v -> return v
     return [accCfg]

-- |Decode, decrypt and parse a mobile wallet export.
decodeMobileFormattedAccountExport :: (MonadError String m)
    => BS.ByteString -- ^JSON payload with encrypted accounts and identities.
    -> BS.ByteString -- ^Password to decrypt the payload.
    -> m [WalletExportAccount]
decodeMobileFormattedAccountExport payload password =
  case eitherDecodeStrict payload of
    Left err -> throwError $ printf "cannot decode JSON: %s" err
    Right we -> do
      pl <- decryptWalletExport we password
      return $ wepAccounts pl

-- |Decode and parse a web wallet export into a named account config.
decodeWebFormattedAccountExport :: (MonadError String m)
    => BS.ByteString -- ^JSON payload with the account information.
    -> Maybe Text -- ^Optionally, how to name the account.
    -> m AccountConfig
decodeWebFormattedAccountExport payload name =
  case eitherDecodeStrict payload of
    Left err -> throwError $ printf "cannot decode JSON: %s" err
    Right val ->
      case parseEither accountParser val of
        Left err -> throwError $ printf "cannot parse JSON: %s" err
        Right x -> return x
  where accountParser = AE.withObject "Account data" $ \v -> do
          addr <- v .: "address"
          acKeys <- v .: "accountKeys"
          acThreshold <- v .:? "threshold" .!= fromIntegral (Map.size acKeys)
          return AccountConfig{acAddr = NamedAddress{naName = name, naAddr = addr}, ..}

parseKeyPair :: Text -- ^Private key.
             -> Text -- ^Public key
             -> IO Sig.KeyPair
parseKeyPair sk vk = do
  sk' <- case parseSignKey sk of
           Left err -> logFatal [printf "cannot parse sign key '%s': %s" sk err]
           Right k -> return k
  vk' <- case parseVerifyKey vk of
           Left err -> logFatal [printf "cannot parse verify key '%s': %s" vk err]
           Right k -> return k
  return $ Sig.KeyPairEd25519
    { Sig.signKey = sk'
    , Sig.verifyKey = vk' }

-- |Process a 'transaction ...' command.
processTransactionCmd :: TransactionCmd -> Maybe FilePath -> Verbose -> Backend -> IO ()
processTransactionCmd action baseCfgDir verbose backend =
  case action of
    TransactionSubmit fname intOpts -> do
      -- TODO Ensure that the "nonce" field is optional in the payload.
      mdata <- loadContextData
      source <- BSL.readFile fname

      -- TODO Print transaction details and ask for confirmation if (ioConfirm intOpts)

      PR.evalContext mdata $ withClient backend $ do
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
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "transfer successfully completed" ]

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
  keysArg <- case toKeys txOpts >>= decodeJsonArg of
               Nothing -> return Nothing
               Just act -> act >>= \case
                 Left err -> logFatal [printf "cannot decode keys: %s" err]
                 Right ks -> return $ Just ks
  (_, accCfg) <- getAccountConfig (toSender txOpts) baseCfg Nothing keysArg False

  energyCostFunc <- getEnergyCostFunc accCfg
  let computedCost = case energyCostFunc of
                       Nothing -> Nothing
                       Just ec -> Just $ ec (Prelude.length $ acKeys accCfg)
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
  , bsatcAccountKeys :: AccountKeys }

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

-- |Resolved configuration for an account undelegation transaction.
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

  return Types.Transfer { tToAddress = Types.AddressAccount $ naAddr toAddress, tAmount = amount }

-- |Convert 'baker add' transaction config into a valid payload,
-- optionally asking the user for confirmation.
bakerAddTransactionPayload :: BakerAddTransactionConfig -> FilePath -> Bool -> IO Types.Payload
bakerAddTransactionPayload batxCfg accountKeysFile confirm = do
  let BakerAddTransactionConfig
        { batcTransactionCfg = TransactionConfig
                               { tcAccountCfg = AccountConfig
                                                { acAddr = NamedAddress { naAddr = addr }
                                                , acKeys = keys }
                               , tcEnergy = energy }
        , batcBakerKeys = bakerKeys }
        = batxCfg

  logSuccess [ printf "submitting transaction to add baker using keys from file '%s'" accountKeysFile
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  generateBakerAddPayload bakerKeys AccountKeys {akAddress = addr, akKeys = keys, akThreshold = fromIntegral (Map.size keys)}

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

-- |Encode, sign, and send transaction off to the baker.
-- If confirmNonce is set, the user is asked to confirm using the next nonce
-- if there are pending transactions.
startTransaction :: (MonadFail m, MonadIO m) => TransactionConfig -> Types.Payload -> Bool -> ClientMonad m Types.BareBlockItem
startTransaction txCfg pl confirmNonce = do
  let TransactionConfig
        { tcEnergy = energy
        , tcExpiry = expiry
        , tcNonce = n
        , tcAccountCfg = accountCfg@AccountConfig
                         { acAddr = NamedAddress { naAddr = addr } } }
        = txCfg
  nonce <- getNonce addr n confirmNonce
  let tx = encodeAndSignTransaction pl energy nonce expiry accountCfg

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
      nextNonce <- nanNonce <$> (getNextAccountNonce (pack $ show sender) >>= getFromJson)
      liftIO $ when (currentNonce /= nextNonce) $ do
        logWarn [ printf "there is a pending transaction with nonce %s, but last committed one has %s" (show $ nextNonce-1) (show $ currentNonce-1)
                , printf "this transaction will have nonce %s and might hang if the pending transaction fails" (show nextNonce) ]
        when confirm $ do
          putStrLn "Proceed if you're confident that all currently pending transactions are valid."
          confirmed <- askConfirmation $ Just "proceed"
          unless confirmed exitTransactionCancelled
      return nextNonce
    Just v -> return v

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
    showResponse = unpack . decodeUtf8 . BSL8.toStrict . AE.encodePretty

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
      v <- withClientJson backend $ withBestBlockHash block $ getAccountInfo (pack $ show $ naAddr na)
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
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "delegation successfully performed" ]
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
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "delegation successfully removed" ]

  where getDelegateCostFunc acc = withClient backend $ do
          when verbose $ logInfo ["retrieving instances"]
          info <- getBestBlockHash >>=
                  getAccountInfo (pack $ show $ naAddr $ acAddr acc) >>=
                  getFromJson
          let is = Prelude.length $ airInstances info
          when verbose $ logInfo [printf "retrieved %d instances" is]
          return $ Just $ accountDelegateEnergyCost is

-- |Process a 'module ...' command.
processModuleCmd :: ModuleCmd -> Verbose -> Backend -> IO ()
processModuleCmd action _ backend =
  case action of
    ModuleShowSource ref block -> do
      m <- PR.evalContext emptyContextData $ withClient backend $ withBestBlockHash block $ getModuleSource ref
      case m of
        Left err -> logFatal [printf "RPC error: %s" err]
        Right v -> do
          -- Force utf8 encoding to prevent crashing on non-unicode terminals.
          -- The special characters will be printed wrong on such terminals,
          -- but this allows them to pipe the output to a file and read it
          -- correctly from there.
          -- TODO Make pretty-printer print ASCII-only by default to avoid the issue.
          let locEnc = show localeEncoding
          enc <- ensureUtfEncoding locEnc $ do
            logWarn [ "displaying module source in UTF-8 encoding"
                    , printf "it appears that your terminal uses the non-unicode encoding '%s'" locEnc
                    , "certain characters may not be rendered correctly"
                    , "consider redirecting the output to a file and open it in UTF-8 using an editor" ]
          putStrLn $ show $ PP.showModule v
          logSuccess [printf "displayed source of module '%s' in encoding '%s'" ref enc]
    ModuleList block -> do
      v <- withClient backend $ withBestBlockHash block getModuleList >>= getFromJson
      runPrinter $ printModuleList v

-- |Test if an encoding name is a flavor of unicode. If it it, return it.
-- Otherwise, print the warning and set the encoding of stdout to UTF-8.
ensureUtfEncoding :: String -> IO () -> IO String
ensureUtfEncoding e printWarn =
  if L.isPrefixOf "utf" $ Prelude.map C.toLower e then
    return e
  else do
    printWarn
    hSetEncoding stdout utf8
    return $ show utf8

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
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "election difficulty updated successfully" ]

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

      let intOpts = toInteractionOpts txOpts
      pl <- bakerAddTransactionPayload batxCfg accountKeysFile (ioConfirm intOpts)
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "baker successfully added" ]
    BakerSetAccount bid file txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      bsatcCfg <- getBakerSetAccountTransactionCfg baseCfg txOpts bid file
      let txCfg = bsatcTransactionCfg bsatcCfg

      let intOpts = toInteractionOpts txOpts
      pl <- bakerSetAccountTransactionPayload bsatcCfg (ioConfirm intOpts)
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
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
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "baker signature key successfully updated" ]
    BakerRemove bid txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      brtcCfg <- getBakerRemoveTransactionCfg baseCfg txOpts bid
      let txCfg = brtcTransactionCfg brtcCfg

      let intOpts = toInteractionOpts txOpts
      pl <- bakerRemoveTransactionPayload brtcCfg (ioConfirm intOpts)
      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "baker successfully removed" ]

    BakerSetAggregationKey bid file txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      bsaktCfg <- getBakerSetAggregationKeyCfg baseCfg txOpts bid file
      let intOpts = toInteractionOpts txOpts
      let txCfg = bsakTransactionCfg bsaktCfg

      pl <- bakerSetAggregationKeyTransactionPayload bsaktCfg (ioConfirm intOpts)

      withClient backend $ do
        tx <- startTransaction txCfg pl (ioConfirm intOpts)
        let hash = getBlockItemHash tx
        logSuccess [ printf "transaction '%s' sent to the baker" (show hash) ]
        when (ioTail intOpts) $ do
          tailTransaction hash
--          logSuccess [ "baker aggregation key successfully updated" ]

    BakerSetElectionKey bid file txOpts -> do
      baseCfg <- getBaseConfig baseCfgDir verbose True
      let intOpts = toInteractionOpts txOpts
      when verbose $ do
        runPrinter $ printBaseConfig baseCfg
        putStrLn ""

      bsektCfg <- getBakerSetElectionKeyCfg baseCfg txOpts bid file
      let txCfg = bsekTransactionCfg bsektCfg
      pl <- bakerSetElectionKeyTransactionPayload bsektCfg (ioConfirm intOpts)

      withClient backend $ do
        tx <- startTransaction (bsekTransactionCfg bsektCfg) pl (ioConfirm intOpts)
        tailTransaction $ getBlockItemHash tx
--          logSuccess [ "baker election key successfully updated" ]


-- |Resolve configuration of a 'baker update' transaction based on persisted config and CLI flags.
-- See the docs for getTransactionCfg for the behavior when no or a wrong amount of energy is allocated.
getBakerSetAccountTransactionCfg :: BaseConfig -> TransactionOpts -> Types.BakerId -> FilePath -> IO BakerSetAccountTransactionConfig
getBakerSetAccountTransactionCfg baseCfg txOpts bid f = do
  txCfg <- getTransactionCfg baseCfg txOpts nrgCost
  accountKeys <- eitherDecodeFileStrict f >>= getFromJson
  return BakerSetAccountTransactionConfig
    { bsatcTransactionCfg = txCfg
    , bsatcBakerId = bid
    , bsatcAccountKeys = accountKeys }
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
bakerSetAccountTransactionPayload :: BakerSetAccountTransactionConfig -> Bool -> IO Types.Payload
bakerSetAccountTransactionPayload bsatcCfg confirm = do
  let BakerSetAccountTransactionConfig
        { bsatcTransactionCfg = TransactionConfig
                                { tcEnergy = energy
                                , tcExpiry = expiry }
        , bsatcAccountKeys = AccountKeys
                             { akAddress = address
                             , akKeys = keys }
        , bsatcBakerId = bid }
        = bsatcCfg
      challenge = generateBakerSetAccountChallenge bid address

  logSuccess [ printf "setting reward account for baker %s to '%s'"  (show bid) (show address)
             , printf "allowing up to %s to be spent as transaction fee" (showNrg energy)
             , printf "transaction expires at %s" (showTimeFormatted $ timeFromTransactionExpiryTime expiry) ]
  when confirm $ do
    confirmed <- askConfirmation Nothing
    unless confirmed exitTransactionCancelled

  proofAccount <- forM keys (\key -> Proofs.proveDlog25519KP challenge key `except` "cannot produce account keys proof")
  return Types.UpdateBakerAccount
    { ubaId = bid
    , ubaAddress = address
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

-- |Process a "legacy" command.
processLegacyCmd :: LegacyCmd -> Backend -> IO ()
processLegacyCmd action backend =
  case action of
    LoadModule fname -> do
      mdata <- loadContextData
      cdata <-
        PR.execContext mdata $ do
          source <- liftIO $ TextIO.readFile fname
          PR.processModule fname source
      putStrLn
        "Module processed.\nThe following modules are currently in the local database and can be deployed.\n"
      showLocalModules cdata
      writeContextData cdata
    ListModules -> do
      mdata <- loadContextData
      putStrLn "The following modules are in the local database.\n"
      showLocalModules mdata

    -- The rest of the commands use the backend
    SendTransaction fname nid -> do
      mdata <- loadContextData
      source <- BSL.readFile fname
      t <- PR.evalContext mdata $ withClient backend $ processTransaction source nid
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (getBlockItemHash t)
    GetConsensusInfo -> withClient backend $ getConsensusStatus >>= printJSON
    GetBlockInfo every block -> withClient backend $ withBestBlockHash block getBlockInfo >>= if every then loop else printJSON
    GetBlockSummary block -> withClient backend $ withBestBlockHash block getBlockSummary >>= printJSON
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
    GetModuleSource moduleref block -> do
      mdata <- loadContextData
      modl <-
        PR.evalContext mdata . withClient backend $ withBestBlockHash block (getModuleSource moduleref)
      case modl of
        Left x ->
          print $ "Unable to get the Module from the gRPC server: " ++ show x
        Right v ->
          let s = show (PP.showModule v)
          in do
            putStrLn $ "Retrieved module " ++ show moduleref
            putStrLn s
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

generateBakerAddPayload :: BakerKeys -> AccountKeys -> IO Types.Payload
generateBakerAddPayload bk ak = do
  let electionSignKey = bkElectionSignKey bk
      signatureSignKey = bkSigSignKey bk
      aggrSignKey = bkAggrSignKey bk

  let abElectionVerifyKey = bkElectionVerifyKey bk
      abSignatureVerifyKey = bkSigVerifyKey bk
      abAggregationVerifyKey = bkAggrVerifyKey bk

  let abAccount = akAddress ak
      keyMap = akKeys ak

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
  -> ClientMonad (PR.Context Core.UA m) Types.BareBlockItem
processTransaction source networkId =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t  -> processTransaction_ t networkId True

-- |Process a transaction with keys given explicitly.
-- The transaction is signed with all the provided keys.
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
        accountConfig = AccountConfig { acAddr = NamedAddress Nothing sender
                                      , acKeys = CT.keys transaction
                                      , acThreshold = fromIntegral (Map.size (CT.keys transaction))
                                      }
    nonce <-
      case thNonce header of
        Nothing -> getBestBlockHash >>= getAccountNonce sender
        Just nonce -> return nonce
    txPayload <- convertTransactionJsonPayload $ payload transaction
    return $ encodeAndSignTransaction
      txPayload
      (thEnergyAmount header)
      nonce
      (thExpiry header)
      accountConfig

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

-- |Convert JSON-based transaction type to one which is ready to be encoded, signed and sent.
convertTransactionJsonPayload :: (MonadFail m) => CT.TransactionJSONPayload -> ClientMonad (PR.Context Core.UA m) Types.Payload
convertTransactionJsonPayload = \case
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
  (CT.RemoveBaker rbid) -> return $ Types.RemoveBaker rbid
  (CT.UpdateBakerAccount ubid uba ubp) ->
    return $ Types.UpdateBakerAccount ubid uba ubp
  (CT.UpdateBakerSignKey ubsid ubsk ubsp) ->
    return $ Types.UpdateBakerSignKey ubsid ubsk ubsp
  (CT.DelegateStake dsid) -> return $ Types.DelegateStake dsid
  (CT.UpdateElectionDifficulty d) -> return $ Types.UpdateElectionDifficulty d

-- |Sign a transaction payload and configuration into a "normal" transaction,
-- which is ready to be sent.
encodeAndSignTransaction ::
     Types.Payload
  -> Types.Energy
  -> Types.Nonce
  -> Types.TransactionExpiryTime
  -> AccountConfig
  -> Types.BareBlockItem
encodeAndSignTransaction txPayload energy nonce expiry accountConfig = 
  Types.NormalTransaction $
    let encPayload = Types.encodePayload txPayload
        sender = acAddress accountConfig
        threshold = acThreshold accountConfig
        header = Types.TransactionHeader{
          thSender = sender,
          thPayloadSize = Types.payloadSize encPayload,
          thNonce = nonce,
          thEnergyAmount = energy,
          thExpiry = expiry
        }
        keys = take (fromIntegral threshold) . sortOn fst . Map.toList $ acKeys accountConfig
    in Types.signTransaction keys header encPayload
