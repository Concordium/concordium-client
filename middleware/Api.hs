{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import qualified Data.HashMap.Strict as HM
import           Data.Aeson (decode')
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (FromJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Control.Exception as C
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Environment
import           System.IO.Error
import           Lens.Micro.Platform ((^.), (^?), _Just)
import           Data.Time.Clock.POSIX

import           Concordium.Client.Commands as COM
import           Concordium.Client.Encryption (Password(..))
import           Concordium.Client.GRPC
import qualified Concordium.Client.GRPC as GRPC
import           Concordium.Client.Runner
import           Concordium.Client.Types.Transaction
import           Concordium.Client.Cli
import           Concordium.Client.Config

import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import           Concordium.Types.HashableTo
import qualified Concordium.Types.Execution as Execution
import           Concordium.Client.Types.Account
import           Control.Monad.Except
import           Data.Word
import qualified Proto.ConcordiumP2pRpc_Fields as CF
import           Api.Types

data Routes r = Routes
    -- Public Middleware APIs
    { consensusStatus :: r :-
        "v1" :> "consensusStatus" :> Get '[JSON] Aeson.Value

    , blockSummary :: r :-
        "v1" :> "blockSummary" :> Capture "blockHash" Text
                               :> Get '[JSON] Aeson.Value

    , blockInfo :: r :-
        "v1" :> "blockInfo" :> Capture "blockHash" Text
                            :> Get '[JSON] Aeson.Value

    , blocksByHeight :: r :-
        "v1" :> "blocksByHeight" :> Capture "blockHeight" Word64
                                 :> Get '[JSON] Aeson.Value

    , transactionStatus :: r :-
        "v1" :> "transactionStatus" :> Capture "hash" Text
                                    :> Get '[JSON] Aeson.Value

    -- Private Middleware APIs (accessible on local client instance of Middleware only)
    , getNodeState :: r :-
        "v1" :> "nodeState" :> Get '[JSON] GetNodeStateResponse

    , setNodeState :: r :-
        "v1" :> "nodeState" :> ReqBody '[JSON] SetNodeStateRequest
                            :> Post '[JSON] SetNodeStateResponse

    , removeBaker :: r :-
        "v1" :> "removeBaker" :> ReqBody '[JSON] RemoveBakerRequest
                              :> Post '[JSON] RemoveBakerResponse

    , getBakers :: r :-
        "v1" :> "getBakers" :> Get '[JSON] GetBakersResponse
    }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)

type Account = (IDTypes.AccountAddress, AccountKeyMap)

servantApp :: EnvData -> FilePath -> Application
servantApp nodeBackend cfgDir = genericServe routesAsServer
 where
  routesAsServer = Routes {..} :: Routes AsServer

  consensusStatus :: Handler Aeson.Value
  consensusStatus = liftIO $ proxyGrpcCall nodeBackend GRPC.getConsensusStatus

  blockSummary :: Text -> Handler Aeson.Value
  blockSummary blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockSummary blockhash)

  blockInfo :: Text -> Handler Aeson.Value
  blockInfo blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockInfo blockhash)

  blocksByHeight :: Word64 -> Handler Aeson.Value
  blocksByHeight height = liftIO $
    proxyGrpcCall nodeBackend (GRPC.getBlocksAtHeight Types.BlockHeight {theBlockHeight = height})

  transactionStatus :: Text -> Handler Aeson.Value
  transactionStatus hash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getTransactionStatus hash)

  getNodeState :: Handler GetNodeStateResponse
  getNodeState = do
    timestamp <- liftIO $ round `fmap` getPOSIXTime
    infoE <- liftIO $ runGRPC nodeBackend getNodeInfo

    nameQuery <- liftIO $ tryIOError (Text.pack <$> getEnv "NODE_NAME")
    let name = case nameQuery of
                 Right x -> x
                 _ -> "unknown"

    versionQuery <- liftIO $ runGRPC nodeBackend getPeerVersion
    let version = case versionQuery of
                  Right x -> x
                  _ -> "unknown"

    uptimeQuery <- liftIO $ runGRPC nodeBackend getPeerUptime
    let runningSince = case uptimeQuery of
                       Right x -> fromIntegral x
                       _ -> 0


    sentQuery <- liftIO $ runGRPC nodeBackend getPeerTotalSent
    let sent = case sentQuery of
               Right x -> fromIntegral x
               _ -> 0

    receivedQuery <- liftIO $ runGRPC nodeBackend getPeerTotalReceived
    let received = case receivedQuery of
                   Right x -> fromIntegral x
                   _ -> 0

    peersStatusQuery <- liftIO $ runGRPC nodeBackend getStatusOfPeers
    let peersStatus = case peersStatusQuery of
                        Right x -> x
                        Left _ -> StatusOfPeers 0 0 0

    case infoE of
      Right ni ->
        pure $
          GetNodeStateResponse
            { id = ni ^. (CF.nodeId . CF.value)
            , running = ni ^. CF.consensusRunning
            , isBaking = ni ^. CF.consensusBakerRunning
            , bakerId = ni ^? CF.maybe'consensusBakerId . _Just . CF.value
            , isInBakingCommittee = ni ^. CF.consensusBakerCommittee
            , isFinalizing = True
            , isInFinalizingCommittee = ni ^. CF.consensusFinalizerCommittee
            , signatureVerifyKey = ""
            , selectionVerifyKey = ""
            , timestamp = timestamp
            , ..
            }

      Left err ->
        throwError $ err400 { errBody = BS8.pack $ show err }


  setNodeState :: SetNodeStateRequest -> Handler SetNodeStateResponse
  setNodeState _ =
    pure $
      SetNodeStateResponse
        { success = True }

  removeBaker :: RemoveBakerRequest -> Handler RemoveBakerResponse
  removeBaker RemoveBakerRequest{..} = do
      -- get base configuration
      baseCfg <- wrapIOError $ getBaseConfig (Just cfgDir) False
      -- generate options for the transaction
      accCfg' <- wrapIOError $ snd <$> getAccountConfig sender baseCfg Nothing Nothing Nothing AssumeInitialized
      let accCfg = accCfg' { acThreshold = fromIntegral (HM.size $ acKeys accCfg') }
      -- get Baker add transaction config
      let energy = bakerRemoveEnergyCost (HM.size $ acKeys accCfg)

      expiry <- wrapIOError $ (600 +) <$> getCurrentTimeUnix

      let AccountConfig{..} = accCfg
      let senderAddr = naAddr acAddr

      accountKeysRes <- liftIO $ decryptAccountKeyMap acKeys (passwordFromText password)
      accountKeys <- (case accountKeysRes of
        Left err -> throwError' err401 $ Just ("", err)
        Right acc -> return acc)


      let pl = Execution.RemoveBaker
      -- run the transaction
      res <- liftIO $ runClient nodeBackend $ do
        currentNonce <- getBestBlockHash >>= getAccountNonce senderAddr
        let tx = encodeAndSignTransaction pl senderAddr energy currentNonce expiry accountKeys acThreshold
        sendTransactionToBaker tx defaultNetId >>= \case
          Left err -> fail err
          Right False -> fail "transaction not accepted by the baker"
          Right True -> return $ getBlockItemHash tx

      case res of
        Left err -> throwError' err409 $ Just ("", err)
        Right v -> return . RemoveBakerResponse . Just . Text.pack $ show v

  getBakers :: Handler GetBakersResponse
  getBakers = GetBakersResponse <$> do
      eeParams <- wrapIOError $ runClient nodeBackend $ withBestBlockHash Nothing getBirkParameters
      case eeParams of
          Right (Right jParams) -> case Aeson.fromJSON jParams of
                                    Aeson.Success params -> return $ Prelude.map (\b ->
                                      Baker { bakerId = bpbrId b
                                            , account = Text.pack $ show $ bpbrAccount b
                                            , lotteryPower = bpbrLotteryPower b }) $ bprBakers params
                                    Aeson.Error e -> wrapSerializationError jParams "birk paramaeters" (Left e)
          Left e -> throwError' err500 (Just ("GRPC error: ", e))
          Right (Left e) -> throwError' err500 (Just ("GRPC error: ", e))

proxyGrpcCall :: EnvData -> ClientMonad IO (Either a Aeson.Value) -> IO Aeson.Value
proxyGrpcCall nodeBackend query = do
  result <- runGRPC nodeBackend query
  case result of
    Right obj ->
      pure $ Aeson.toJSON obj
    _ ->
      pure Aeson.Null

-- | Encode a password received over the network as 'Text' into a 'Password'.
passwordFromText :: Text -> Password
passwordFromText txt = Password (Text.encodeUtf8 txt)

-- IO Errors will be translated to 500 internal server error
-- we will ignore the body as it usually comes from an exitFailure
-- and it doesn't give much information
wrapIOError :: forall b. IO b -> Handler b
wrapIOError f =
  (liftIO (C.try f :: IO (Either C.IOException b))) >>= \case
    Left _ -> throwError' err500 (Nothing :: Maybe (String, String))
    Right val -> return val

-- Serialization errors will be translated to 400 bad request
wrapSerializationError :: (Show a) => a -> String -> Either String b -> Handler b
wrapSerializationError _ _ (Right val) = return val
wrapSerializationError value typ (Left err) = throwError' err400 $ Just ("cannot parse '" ++ show value ++ "' as type " ++ typ ++ ": ", err)

throwError' :: (Show err) => ServerError -> Maybe (String, err) -> Handler a
throwError' e (Just (desc, err)) = throwError $ e { errBody = BS8.pack (desc ++ show err) }
throwError' e Nothing = throwError e

-- |A wrapper around 'throwError'' to reduce the amount of case analysis on use-sites.
embedServerErrM :: (Show err) => Handler (Either e' a) -> ServerError -> (e' -> Maybe err) -> Handler a
embedServerErrM action serverErr f = do
  action >>= \case
    Left err' -> throwError' serverErr (("",) <$> f err')
    Right v -> return v

-- For beta, uses the middlewareGodAccount which is seeded with funds
runGodTransaction :: EnvData -> Account -> TransactionJSONPayload -> IO Types.TransactionHash
runGodTransaction nodeBackend middlewareGodAccount payload =
  runTransaction nodeBackend payload middlewareGodAccount


runGRPC :: (MonadIO m) => EnvData -> ClientMonad m a -> m a
runGRPC envData c =
  runClient envData c >>= \case
    Left err -> liftIO $ fail (show err)
    Right x -> return x

runTransaction :: EnvData -> TransactionJSONPayload -> Account -> IO Types.TransactionHash
runTransaction nodeBackend payload (address, keyMap) = do

  nonce <- fst <$> runGRPC nodeBackend (getAccountNonceBestGuess address)

  currentTime <- liftIO $ round `fmap` getPOSIXTime

  let
    transactionExpiry = currentTime + (60*30) -- Expires in 30 minutes from now

    -- These are the agreed fixed costs for testnet, they
    -- will change when tokenomics is finalized
    energyAmount =
      case payload of
        Transfer _ _       -> simpleTransferEnergyCost (HM.size keyMap)
        _                  -> 10000

    transaction =
      TransactionJSON
        { metadata = transactionHeader
        , payload = payload
        , keys = keyMap
        }

    transactionHeader =
          TransactionJSONHeader
            { thSenderAddress = address
            , thNonce = Just nonce
            , thEnergyAmount = energyAmount
            , thExpiry = Types.TransactionTime transactionExpiry
            }

  executeTransaction nodeBackend transaction

executeTransaction :: EnvData -> TransactionJSON -> IO Types.TransactionHash
executeTransaction nodeBackend transaction = do
  -- The networkId is for running multiple networks that's not the same chain, but hasn't been taken into use yet
  let nid = 1000

  t <- runGRPC nodeBackend $ processTransaction_ transaction nid True

  putStrLn $ "✅ Transaction sent to the baker and hooked: " ++ show (getHash t :: Types.TransactionHash)
  print transaction

  pure $ getHash t

-- Dirty helper to help us with "definitely certain" value decoding
certainDecode :: (FromJSON a) => Text -> a
certainDecode t =
  case decode' $ BS8.fromStrict $ Text.encodeUtf8 t of
    Just v -> v
    Nothing -> error $ "Well... not so certain now huh! certainDecode failed on:\n\n" ++ Text.unpack t

-- Simple helper to parse key files
accountParser :: Aeson.Value -> Aeson.Parser (IDTypes.AccountAddress, AccountKeyMap)
accountParser = Aeson.withObject "Account keys" $ \v -> do
          accountAddr <- v Aeson..: "address"
          accountData <- v Aeson..: "accountData"
          keyMap <- accountData Aeson..: "keys"
          return (accountAddr, keyMap)

-- Helper function to read the keyfile and bail if not possible
getGtuDropKeys :: Maybe Text -> IO (Maybe Account)
getGtuDropKeys keyFileLocation =
  case keyFileLocation of
    Just keyFile -> do
      keyFileData <- LBS.readFile (Text.unpack keyFile)
      let getKeys = Aeson.eitherDecode' keyFileData >>= Aeson.parseEither accountParser
      return $ either (const Nothing) Just getKeys
    _ -> return $ Nothing

debugGrpc :: IO GetNodeStateResponse
debugGrpc = do
  let nodeBackend = COM.GRPC { grpcHost = "localhost", grpcPort = 11103, grpcAuthenticationToken = "rpcadmin", grpcTarget = Nothing, grpcRetryNum = 5 }

  infoE <- withClient nodeBackend getNodeInfo

  nameQuery <- liftIO $ tryIOError (Text.pack <$> getEnv "NODE_NAME")
  let name = case nameQuery of
        Right x -> x
        _ -> "unknown"

  versionQuery <- withClient nodeBackend getPeerVersion
  let version = case versionQuery of
                  Right x -> x
                  _ -> "unknown"

  uptimeQuery <- withClient nodeBackend getPeerUptime
  let runningSince = case uptimeQuery of
                       Right x -> fromIntegral x
                       _ -> 0


  sentQuery <- withClient nodeBackend getPeerTotalSent
  let sent = case sentQuery of
               Right x -> fromIntegral x
               _ -> 0

  receivedQuery <- withClient nodeBackend getPeerTotalReceived
  let received = case receivedQuery of
                   Right x -> fromIntegral x
                   _ -> 0

  peersStatusQuery <- withClient nodeBackend getStatusOfPeers
  let peersStatus = case peersStatusQuery of
                      Right x -> x
                      Left _ -> StatusOfPeers 0 0 0

  case infoE of
    Right ni -> do
      print ni

      pure $
        GetNodeStateResponse
          { id = ni ^. (CF.nodeId . CF.value)
          , running = ni ^. CF.consensusRunning
          , isBaking = ni ^. CF.consensusBakerRunning
          , isInBakingCommittee = ni ^. CF.consensusBakerCommittee
          , bakerId = ni ^? CF.maybe'consensusBakerId . _Just . CF.value
          , isFinalizing = True
          , isInFinalizingCommittee = ni ^. CF.consensusFinalizerCommittee
          , signatureVerifyKey = ""
          , selectionVerifyKey = ""
          , timestamp = 0
          , ..
          }

    Left err ->
      error $ show err
