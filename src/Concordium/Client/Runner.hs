{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Concordium.Client.Runner
  ( process
  , sendTransactionToBaker
  , getConsensusStatus
  , getAccountInfo
  , ClientMonad(..)
  , runInClient
  , EnvData(..)
  , GrpcConfig
  ) where

import qualified Acorn.Core                          as Core
import qualified Acorn.Parser.Runner                 as PR
import           Concordium.Client.Commands          as COM hiding (networkId)
import           Concordium.Client.GRPC
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction as CT
import           Concordium.Crypto.SignatureScheme   (KeyPair (..))

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

import           Data.Aeson                          as AE
import qualified Data.HashMap.Strict                 as Map
import           Data.Maybe
import           Data.Text

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
    { _runClientMonad :: ReaderT EnvData m a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader EnvData
           , MonadFail
           , MonadIO
           )

liftContext :: PR.Context Core.UA m a -> ClientMonad (PR.Context Core.UA m) a
liftContext comp = ClientMonad {_runClientMonad = ReaderT (const comp)}

runInClient :: (MonadIO m) => Backend -> ClientMonad m a -> m a
runInClient bkend comp = do
  client <-
    liftIO $ mkGrpcClient $
    GrpcConfig (COM.host bkend) (COM.port bkend) (COM.target bkend)
  (runReaderT . _runClientMonad) comp $! EnvData client

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
      maybe (print "No Backend provided") (useBackend act) (backend command)

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
        show (Types.trHash t)
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
        Right v -> do
          s <-
            PR.evalContext
              mdata
              (PR.ppModuleInCtx v :: PR.Context Core.UA IO String)
          putStrLn $ "Retrieved module " ++ show moduleref
          putStrLn s
    _ -> undefined

processTransaction ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> Bool
  -> ClientMonad (PR.Context Core.UA m) Types.Transaction
processTransaction source networkId hookit =
  case AE.eitherDecode source of
    Left err -> fail $ "Error decoding JSON: " ++ err
    Right t -> do
      transaction <-
        case t of
          Just transaction -> do
            nonce <-
              case thNonce . metadata $ transaction of
                Nothing    -> undefined --askBaker
                Just nonce -> return nonce
            let properT =
                  makeTransactionHeaderWithNonce (metadata transaction) nonce
            encodeAndSignTransaction
              (payload transaction)
              properT
              (KeyPair (CT.signKey transaction) (Types.thSenderKey properT))
          Nothing -> undefined
      when hookit $ do
        liftIO . putStrLn $ "Installing hook for transaction " ++
          show (Types.trHash transaction)
        printJSON =<< sendHookToBaker (Types.trHash transaction)
      sendTransactionToBaker transaction networkId
      return transaction

readModule :: MonadIO m => FilePath -> ClientMonad m (Core.Module Core.UA)
readModule filePath = do
  source <- liftIO $ BSL.readFile filePath
  case S.decodeLazy source of
    Left err  -> liftIO (die err)
    Right mod -> return mod

encodeAndSignTransaction ::
     (MonadFail m, MonadIO m)
  => CT.TransactionJSONPayload
  -> Types.TransactionHeader
  -> KeyPair
  -> ClientMonad (PR.Context Core.UA m) Types.Transaction
encodeAndSignTransaction pl th keys =
  Types.signTransaction keys th . Types.encodePayload <$>
  case pl of
    (CT.DeployModuleFromSource fileName) ->
      Types.DeployModule <$> readModule fileName -- deserializing is not necessary, but easiest for now.
    (CT.DeployModule mnameText) ->
      Types.DeployModule <$> liftContext (PR.getModule mnameText)
    (CT.InitContract initAmount mnameText cNameText paramExpr) -> do
      (mref, _, tys) <- liftContext $ PR.getModuleTmsTys mnameText
      case Map.lookup cNameText tys of
        Just contName -> do
          params <- liftContext $ PR.processTmInCtx mnameText paramExpr
          return $ Types.InitContract initAmount mref contName params 0
        Nothing -> error (show cNameText)
    (CT.Update mnameText updateAmount updateAddress msgText) -> do
      msg <- liftContext $ PR.processTmInCtx mnameText msgText
      return $ Types.Update updateAmount updateAddress msg 0
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

sendHookToBaker ::
     (MonadIO m)
  => Types.TransactionHash
  -> ClientMonad m (Either String [Value])
sendHookToBaker txh = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "hookTransaction")
        client
        (defMessage & CF.transactionHash .~ pack (show txh))
    return $ processJSON ret

sendTransactionToBaker ::
     (MonadIO m) => Types.Transaction -> Int -> ClientMonad m ()
sendTransactionToBaker t nid = do
  client <- asks grpc
  d <-
    liftIO $
    rawUnary
      (RPC :: RPC P2P "sendTransaction")
      client
      (defMessage & CF.networkId .~ fromIntegral nid & CF.payload .~ S.encode t)
  d `seq` return ()

hookTransaction :: Text -> ClientMonad IO (Either String [Value])
hookTransaction txh = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "hookTransaction")
        client
        (defMessage & CF.transactionHash .~ txh)
    return $ processJSON ret

getConsensusStatus :: ClientMonad IO (Either String [Value])
getConsensusStatus = do
  client <- asks grpc
  liftIO $ do
    ret <- rawUnary (RPC :: RPC P2P "getConsensusStatus") client defMessage
    return $ processJSON ret

getBlockInfo :: Text -> ClientMonad IO (Either String [Value])
getBlockInfo hash = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getBlockInfo")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getAccountList :: Text -> ClientMonad IO (Either String [Value])
getAccountList hash = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getAccountList")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getInstances :: Text -> ClientMonad IO (Either String [Value])
getInstances hash = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getInstances")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getAccountInfo :: Text -> Text -> ClientMonad IO (Either String [Value])
getAccountInfo hash account = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getAccountInfo")
        client
        (defMessage & CF.blockHash .~ hash & CF.address .~ account)
    return $ processJSON ret

getInstanceInfo :: Text -> Text -> ClientMonad IO (Either String [Value])
getInstanceInfo hash account = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getInstanceInfo")
        client
        (defMessage & CF.blockHash .~ hash & CF.address .~ account)
    return $ processJSON ret

getRewardStatus :: Text -> ClientMonad IO (Either String [Value])
getRewardStatus hash = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getRewardStatus")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getBirkParameters :: Text -> ClientMonad IO (Either String [Value])
getBirkParameters hash = do
  client <- asks grpc
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getBirkParameters")
        client
        (defMessage & CF.blockHash .~ hash)
    return $ processJSON ret

getModuleList :: Text -> ClientMonad IO (Either String [Value])
getModuleList hash = do
  client <- asks grpc
  liftIO $ do
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
  liftIO $ do
    ret <-
      rawUnary
        (RPC :: RPC P2P "getModuleSource")
        client
        (defMessage & CF.blockHash .~ hash & CF.moduleRef .~ moduleref)
    return $ S.decode (ret ^. unaryOutput . CF.payload)
