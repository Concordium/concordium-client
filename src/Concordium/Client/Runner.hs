{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Concordium.Client.Runner
  ( process
  , sendTransactionToBaker
  , ClientMonad
  , runInClient
  ) where

import qualified Acorn.Core                          as Core
import qualified Acorn.Parser.Runner                 as PR
import           Concordium.Client.Commands          as COM
import           Concordium.Client.GRPC
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction as CT
import           Concordium.Crypto.Ed25519Signature  (randomKeyPair)
import           Concordium.Crypto.SignatureScheme   (KeyPair (..))

import           Data.ProtoLens                      (defMessage, fieldsByTag)
import           Proto.Concordium
import qualified Proto.Concordium_Fields             as CF

import qualified Concordium.Scheduler.Types          as Types

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader                hiding (fail)
import           Control.Monad.State                 hiding (fail)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as BSL
import           Data.ByteString.Lazy.Char8          (null, unlines)
import qualified Data.Serialize                      as S
import qualified Data.Text.IO                        as TextIO hiding (putStrLn)
import           Lens.Simple

import           Network.GRPC.Client
import           Network.GRPC.Client.Helpers
import           Network.HTTP2.Client

import           Data.Aeson                          as AE
import qualified Data.HashMap.Strict                 as Map
import           Data.Maybe
import           Data.Text
import           Data.Text.Encoding
import           System.Random

import           Prelude                             hiding (fail, null,
                                                      unlines)
import           System.Exit(die)

newtype EnvData =
  EnvData
    { grpc :: IO GrpcClient
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

client :: PR.Context Core.UA m a -> ClientMonad (PR.Context Core.UA m) a
client comp = ClientMonad {_runClientMonad = ReaderT (const comp)}

runInClient :: Monad m => Backend -> ClientMonad m a -> m a
runInClient bkend action =
  (runReaderT . _runClientMonad)
    action
  -- separate in cases for different backends (maybe abstract in a typeclass)
    (EnvData . mkGrpcClient $ GrpcConfig (COM.host bkend) (COM.port bkend) (COM.target bkend))

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
    SendTransaction fname nid -> do
      mdata <- loadContextData
      source <- BSL.readFile fname
      t <- PR.evalContext mdata $ runInClient b $ processTransaction source nid
      putStrLn $ "Transaction sent to the baker. Its hash is " ++
        show (Types.trHash t)
    GetConsensusInfo -> runInClient b getConsensusStatus
    GetBlockInfo block -> runInClient b $ getBlockInfo block
    GetAccountList block -> runInClient b $ getAccountList block
    GetInstances block -> runInClient b $ getInstances block
    GetAccountInfo block account -> runInClient b $ getAccountInfo block account
    GetInstanceInfo block account ->
      runInClient b $ getInstanceInfo block account
    GetRewardStatus block -> runInClient b $ getRewardStatus block
    GetBirkParameters block -> runInClient b $ getBirkParameters block
    GetModuleList block -> runInClient b $ getModuleList block
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

processTransaction ::
     (MonadFail m, MonadIO m)
  => BSL.ByteString
  -> Int
  -> ClientMonad (PR.Context Core.UA m) Types.Transaction
processTransaction source networkId =
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
      sendTransactionToBaker transaction networkId

readModule :: MonadIO m => FilePath -> ClientMonad m (Core.Module Core.UA)
readModule filePath = do
  source <- liftIO $ BSL.readFile filePath
  case S.decodeLazy source of
    Left err -> liftIO (die err)
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
      Types.DeployModule <$> readModule fileName  -- deserializing is not necessary, but easiest for now.
    (CT.DeployModule mnameText) ->
      Types.DeployModule <$> client (PR.getModule mnameText)
    (CT.InitContract initAmount mnameText cNameText paramExpr) -> do
      (mref, _, tys) <- client $ PR.getModuleTmsTys mnameText
      case Map.lookup cNameText tys of
        Just contName -> do
          params <- client $ PR.processTmInCtx mnameText paramExpr
          return $ Types.InitContract initAmount mref contName params 0
        Nothing -> error (show cNameText)
    (CT.Update mnameText updateAmount updateAddress msgText) -> do
      msg <- client $ PR.processTmInCtx mnameText msgText
      return $ Types.Update updateAmount updateAddress msg 0
    (CT.Transfer transferTo transferAmount) ->
      return $ Types.Transfer transferTo transferAmount
    (CT.DeployCredential cred) -> return $ Types.DeployCredential cred
    (CT.DeployEncryptionKey key) -> return $ Types.DeployEncryptionKey key
    (CT.AddBaker evk svk ba p) -> return $ Types.AddBaker evk svk ba p
    (CT.RemoveBaker rbid rbp) -> return $ Types.RemoveBaker rbid rbp
    (CT.UpdateBakerAccount ubid uba ubp) ->
      return $ Types.UpdateBakerAccount ubid uba ubp
    (CT.UpdateBakerSignKey ubsid ubsk ubsp) ->
      return $ Types.UpdateBakerSignKey ubsid ubsk ubsp
    (CT.DelegateStake dsid) -> return $ Types.DelegateStake dsid

sendTransactionToBaker ::
     (Monad m, MonadIO m)
  => Types.Transaction
  -> Int
  -> ClientMonad m Types.Transaction
sendTransactionToBaker t nid = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "sendTransaction")
        client
        (defMessage & CF.networkId .~ fromIntegral nid & CF.payload .~
         S.encode t)
    outputGRPC ret (\_ -> return ())
  return t

getConsensusStatus :: ClientMonad IO ()
getConsensusStatus = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <- rawUnary (RPC :: RPC P2P "getConsensusStatus") client defMessage
    printJSON ret

getBlockInfo :: Text -> ClientMonad IO ()
getBlockInfo hash = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getBlockInfo")
        client
        (defMessage & CF.blockHash .~ hash)
    printJSON ret

getAccountList :: Text -> ClientMonad IO ()
getAccountList hash = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getAccountList")
        client
        (defMessage & CF.blockHash .~ hash)
    printJSON ret

getInstances :: Text -> ClientMonad IO ()
getInstances hash = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getInstances")
        client
        (defMessage & CF.blockHash .~ hash)
    printJSON ret

getAccountInfo :: Text -> Text -> ClientMonad IO ()
getAccountInfo hash account = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getAccountInfo")
        client
        (defMessage & CF.blockHash .~ hash & CF.address .~ account)
    printJSON ret

getInstanceInfo :: Text -> Text -> ClientMonad IO ()
getInstanceInfo hash account = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getInstanceInfo")
        client
        (defMessage & CF.blockHash .~ hash & CF.address .~ account)
    printJSON ret

getRewardStatus :: Text -> ClientMonad IO ()
getRewardStatus hash = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getRewardStatus")
        client
        (defMessage & CF.blockHash .~ hash)
    printJSON ret

getBirkParameters :: Text -> ClientMonad IO ()
getBirkParameters hash = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getBirkParameters")
        client
        (defMessage & CF.blockHash .~ hash)
    printJSON ret

getModuleList :: Text -> ClientMonad IO ()
getModuleList hash = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getModuleList")
        client
        (defMessage & CF.blockHash .~ hash)
    printJSON ret

getModuleSource ::
     (Monad m, MonadIO m)
  => Text
  -> Text
  -> ClientMonad (PR.Context Core.UA m) (Either String (Core.Module Core.UA))
getModuleSource hash moduleref = do
  env <- ask
  liftIO $ do
    client <- grpc env
    ret <-
      rawUnary
        (RPC :: RPC P2P "getModuleSource")
        client
        (defMessage & CF.blockHash .~ hash & CF.moduleRef .~ moduleref)
    return $ S.decode (ret ^. unaryOutput . CF.payload)
