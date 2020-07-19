{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Api where

import           Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as HM
import           Data.Aeson (encode, decode')
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (FromJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Control.Exception as C
import qualified Data.ByteString.Short as BSS
import           Data.Maybe (fromJust)
import           Data.List.Split
import           Data.Map
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Environment
import           System.IO.Error
import           Text.Read (readMaybe)
import           Lens.Micro.Platform ((^.), (^?), _Just)
import           Data.Time
import           Data.Time.Clock.POSIX
import           NeatInterpolation
import qualified Data.List as L
import           System.FilePath.Posix

import qualified Acorn.Parser.Runner as PR
import           Concordium.Client.Commands as COM
import           Concordium.Client.Encryption (Password(..))
import           Concordium.Client.GRPC
import qualified Concordium.Client.GRPC as GRPC
import           Concordium.Client.Runner
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction
import           Concordium.Client.Cli
import           Concordium.Client.Config
import           Concordium.Client.Utils

import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import           Concordium.Types.HashableTo
import qualified Concordium.Types.Transactions as Types
import qualified Concordium.Types.Execution as Execution
import           Concordium.Client.Types.Account
import           Control.Monad.Except
import qualified Proto.ConcordiumP2pRpc_Fields as CF


import qualified Config
import           SimpleIdClientApi
import           Api.Types

-- Account transactions
import           Conduit
import           PerAccountTransactions
import qualified Concordium.Client.TransactionStatus

data Routes r = Routes
    -- Public Middleware APIs
    { betaIdProvision :: r :-
        "v1" :> "betaIdProvision" :> ReqBody '[JSON] BetaIdProvisionRequest
                                  :> Post '[JSON] Aeson.Value

    , betaAccountProvision :: r :-
        "v1" :> "betaAccountProvision" :> ReqBody '[JSON] BetaAccountProvisionRequest
                                       :> Post '[JSON] BetaAccountProvisionResponse

    , testnetGtuDrop :: r :-
        "v1" :> "testnetGtuDrop" :> ReqBody '[JSON] Types.Address
                              :> Post '[JSON] TestnetGtuDropResponse

    , accountTransactions :: r :-
        "v1" :> "accountTransactions" :> ReqBody '[JSON] Types.AccountAddress
                                      -- @TODO get streaming working instead, requires a non-sinking conduit from Persist.SQL
                                      -- :> StreamPost NewlineFraming JSON (ConduitM () (Either String PrettyEntry) (ReaderT SqlBackend IO) ())
                                      :> Post '[JSON] AccountTransactionsResponse

    , accountNonFinalizedTransactions :: r :-
        "v1" :> "accountNonFinalizedTransactions" :> ReqBody '[JSON] Text
                                      :> Post '[JSON] Aeson.Value

    , accountBestBalance :: r :-
        "v1" :> "accountBestBalance" :> ReqBody '[JSON] Text
                                     :> Post '[JSON] Aeson.Value


    , transfer :: r :-
        "v1" :> "transfer" :> ReqBody '[JSON] TransferRequest
                           :> Post '[JSON] TransferResponse

    , consensusStatus :: r :-
        "v1" :> "consensusStatus" :> Get '[JSON] Aeson.Value

    , blockInfo :: r :-
        "v1" :> "blockInfo" :> Capture "blockHash" Text
                              :> Get '[JSON] Aeson.Value

    , blockSummary :: r :-
        "v1" :> "blockSummary" :> Capture "blockHash" Text
                              :> Get '[JSON] Aeson.Value

    , transactionStatus :: r :-
        "v1" :> "transactionStatus" :> Capture "hash" Text
                              :> Get '[JSON] Aeson.Value

    , simpleTransactionStatus :: r :-
        "v1" :> "simpleTransactionStatus" :> Capture "hash" Text
                              :> Get '[JSON] Aeson.Value

    -- Private Middleware APIs (accessible on local client instance of Middleware only)
    , getNodeState :: r :-
        "v1" :> "nodeState" :> Get '[JSON] GetNodeStateResponse

    , setNodeState :: r :-
        "v1" :> "nodeState" :> ReqBody '[JSON] SetNodeStateRequest
                            :> Post '[JSON] SetNodeStateResponse

    , importAccount :: r :-
        "v1" :> "importAccount" :> ReqBody '[JSON] ImportAccountRequest
                                :> Post '[JSON] ()

    , getAccounts :: r :-
        "v1" :> "getAccounts" :> Get '[JSON] GetAccountsResponse

    , addBaker :: r :-
        "v1" :> "addBaker" :> ReqBody '[JSON] AddBakerRequest
                           :> Post '[JSON] AddBakerResponse
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

servantApp :: EnvData -> Maybe Account -> Text -> Text -> FilePath -> FilePath -> Application
servantApp nodeBackend dropAccount pgUrl idUrl cfgDir dataDir = genericServe routesAsServer
 where
  routesAsServer = Routes {..} :: Routes AsServer

  betaIdProvision :: BetaIdProvisionRequest -> Handler Aeson.Value
  betaIdProvision BetaIdProvisionRequest{..} = do

    liftIO $ putStrLn "✅ Got the following attributes:"
    liftIO $ print attributes

    creationTime <- liftIO getCurrentTime

    let
      expiryDate = addUTCTime (60*60*24*365) creationTime -- Expires in 365 days from now
      idObjectRequest =
          IdObjectRequest
            { ipIdentity = 0
            , attributes =
                Attributes
                  { chosenAttributes = attributes
                  , createdAt = toISOYearMonth creationTime
                  , validTo = toISOYearMonth expiryDate
                  , maxAccounts = 30
                  }
            , anonymityRevokers = [0,1,2]
            , threshold = 2
            }

    idObjectResponse <- liftIO $ postIdObjectRequest idUrl idObjectRequest

    liftIO $ putStrLn "✅ Got IdObjectResponse"

    pure $ Aeson.toJSON idObjectResponse


  betaAccountProvision :: BetaAccountProvisionRequest -> Handler BetaAccountProvisionResponse
  betaAccountProvision accountProvisionRequest = do

    let credentialRequest =
          IdCredentialRequest
            { ipIdentity = ipIdentity (accountProvisionRequest :: BetaAccountProvisionRequest)
            , identityObject = identityObject (accountProvisionRequest :: BetaAccountProvisionRequest)
            , idUseData = idUseData (accountProvisionRequest :: BetaAccountProvisionRequest)
            , revealedAttributes = revealedAttributes (accountProvisionRequest :: BetaAccountProvisionRequest)
            , accountNumber = accountNumber (accountProvisionRequest :: BetaAccountProvisionRequest)
            }

    idCredentialResponse <- liftIO $ postIdCredentialRequest idUrl credentialRequest

    liftIO $ putStrLn "✅ Got idCredentialResponse"

    let
      newAccountAddress = accountAddress (idCredentialResponse :: IdCredentialResponse)

    transactionHash <- liftIO $ deployCredential nodeBackend (credential (idCredentialResponse :: IdCredentialResponse))

    pure $
      BetaAccountProvisionResponse
        { accountKeys = SimpleIdClientApi.keys $ SimpleIdClientApi.accountData (idCredentialResponse :: IdCredentialResponse)
        , spio = credential (idCredentialResponse :: IdCredentialResponse)
        , address = Text.pack . show $ newAccountAddress
        , transactionHash = transactionHash
        }


  testnetGtuDrop :: Types.Address -> Handler TestnetGtuDropResponse
  testnetGtuDrop toAddress =
    case toAddress of
      Types.AddressAccount address -> do
        case dropAccount of
          Just dropAccountData -> do
            accountResult <- liftIO $ runGRPC nodeBackend (withBestBlockHash Nothing (getAccountInfo $ Text.pack . show $ address))
            nonce <- liftIO $ fst <$> runGRPC nodeBackend (getAccountNonceBestGuess (fst dropAccountData))

            case accountResult of
              Right Aeson.Null -> throwError $ err409 { errBody = "Account is not yet on the network." }
              Right accountJson -> do
                case Aeson.fromJSON accountJson of
                  Aeson.Success account ->
                    if nonce == Types.minNonce && accountAmount account == 0
                      then do
                        liftIO $ putStrLn $ "✅ Requesting GTU Drop for " ++ show toAddress
                        transactionId <- liftIO $ runGodTransaction nodeBackend dropAccountData $ Transfer { toaddress = toAddress, amount = 1000000 }
                        pure $ TestnetGtuDropResponse { transactionId = transactionId }

                      else
                        throwError $ err403 { errBody = "GTU drop can only be used once per account." }

                  Aeson.Error err ->
                      throwError $ err502 { errBody = "JSON error: " <> BS8.pack err }

              Left err ->
                throwError $ err502 { errBody = "GRPC error: " <> BS8.pack err }

          Nothing ->
            throwError $ err502 { errBody = "Can't do GTU drop - missing keys" }
      _ ->
        throwError $ err403 { errBody = "GTU drop can only be used for Account addresses." }


  accountTransactions :: Types.AccountAddress -> Handler AccountTransactionsResponse
  accountTransactions address = do
    let toOutcome (Right p) = Just (outcomeFromPretty p)
        toOutcome _ = Nothing

    outcomes <- liftIO $ processAccounts (Text.encodeUtf8 pgUrl) address (mapWhileC toOutcome .| sinkList)

    pure $ AccountTransactionsResponse outcomes address


  accountNonFinalizedTransactions :: Text -> Handler Aeson.Value
  accountNonFinalizedTransactions address = liftIO $ proxyGrpcCall nodeBackend (GRPC.getAccountNonFinalizedTransactions address)

  accountBestBalance :: Text -> Handler Aeson.Value
  accountBestBalance address =
    liftIO $ proxyGrpcCall nodeBackend
      (GRPC.withBestBlockHash Nothing (GRPC.getAccountInfo address))


  transfer :: TransferRequest -> Handler TransferResponse
  transfer TransferRequest{..} = do

    let (AccountWithKeys accountAddress _) = account

    liftIO $ putStrLn $ "✅ Sending " ++ show amount ++ " from " ++ show accountAddress ++ " to " ++ show to

    transactionId <- liftIO $ runTransaction nodeBackend (Transfer { toaddress = to, amount = amount }) (accountToPair account)

    pure $ TransferResponse { transactionId = transactionId }

  consensusStatus :: Handler Aeson.Value
  consensusStatus = liftIO $ proxyGrpcCall nodeBackend GRPC.getConsensusStatus


  blockInfo :: Text -> Handler Aeson.Value
  blockInfo blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockInfo blockhash)


  blockSummary :: Text -> Handler Aeson.Value
  blockSummary blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockSummary blockhash)


  transactionStatus :: Text -> Handler Aeson.Value
  transactionStatus hash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getTransactionStatus hash)


  simpleTransactionStatus :: Text -> Handler Aeson.Value
  simpleTransactionStatus hashRaw = do

    -- @TODO why does using TransactionHash instead of Text in args cause `No instance for (FromHttpApiData SHA256.Hash)`?
    let hash_ = readMaybe (Text.unpack hashRaw)

    case hash_ of
      Just hash -> do
        res <- liftIO $ runGRPC nodeBackend $ Concordium.Client.TransactionStatus.getSimpleTransactionStatus hash
        case res of
          Right status -> pure status
          Left err -> throwError $ err502 { errBody = BS8.pack $ show err }

      Nothing ->
        throwError $ err400 { errBody = "Invalid transaction hash." }


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

  importAccount :: ImportAccountRequest -> Handler ()
  importAccount request = do
    -- init configuration if missing
    baseCfg <- wrapIOError $ getBaseConfig (Just cfgDir) False True
    accCfgs <- case request of
      ImportAccountRequestMobile{..} -> do
        ((liftIO $ decodeMobileFormattedAccountExport (Text.encodeUtf8 contents) Nothing (passwordFromText password))
          `embedServerErrM` err400) Just
      ImportAccountRequestWeb{..} -> do
        accCfg <- ((liftIO $ decodeWebFormattedAccountExport (Text.encodeUtf8 contents) alias (passwordFromText password))
                   `embedServerErrM` err400) Just
        return [accCfg]
    liftIO $ void $ (importAccountConfig baseCfg) accCfgs

  getAccounts :: Handler GetAccountsResponse
  getAccounts = GetAccountsResponse <$> wrapIOError go
   where go :: IO [GetAccountsResponseItem]
         go = do
             -- get base config
             baseCfg <- getBaseConfig (Just cfgDir) False True
             -- get all accounts
             allAccs <- Prelude.map (naAddr . acAddr) <$> getAllAccountConfigs baseCfg
             let named = HM.toList $ bcAccountNameMap baseCfg
                 namedAddresses = Prelude.map snd named
                 namedAsText = Prelude.map (\(a, b) -> GetAccountsResponseItem (Just a) (Text.pack $ show b)) named
                 unnamedAsText = Prelude.map (\b -> GetAccountsResponseItem Nothing (Text.pack $ show b)) (allAccs L.\\ namedAddresses)
             return $ namedAsText ++ unnamedAsText


  addBaker :: AddBakerRequest -> Handler AddBakerResponse
  addBaker AddBakerRequest{..} = AddBakerResponse <$> wrapIOError (do
      -- get base configuration
      baseCfg <- getBaseConfig (Just cfgDir) False True
      -- generate options for the transaction
      accCfg' <- snd <$> getAccountConfig sender baseCfg Nothing Nothing False
      let accCfg = accCfg' { acThreshold = fromIntegral (HM.size $ acKeys accCfg') }
          file = dataDir </> "baker-credentials.json"
          -- get Baker add transaction config
          energy = bakerAddEnergyCost (HM.size $ acKeys accCfg)

      expiry <- (600 +) <$> (liftIO getCurrentTimeUnix)

      let AccountConfig{..} = accCfg
      let senderAddr = naAddr acAddr

      bakerKeys <- liftIO $ Aeson.eitherDecodeFileStrict file >>= getFromJson

      accountKeys <- liftIO (failOnError $ decryptAccountKeyMap acKeys (passwordFromText password))

      pl <- liftIO $ generateBakerAddPayload bakerKeys $ accountSigningDataFromConfig accCfg accountKeys
      -- run the transaction
      res <- runClient nodeBackend $ do
        currentNonce <- getBestBlockHash >>= getAccountNonce senderAddr
        let tx = encodeAndSignTransaction pl senderAddr energy currentNonce expiry accountKeys acThreshold
        return $ getBlockItemHash tx
      case res of
        Left _ -> return Nothing
        Right v -> return . Just . Text.pack $ show v)

  removeBaker :: RemoveBakerRequest -> Handler RemoveBakerResponse
  removeBaker RemoveBakerRequest{..} = RemoveBakerResponse <$> wrapIOError (do
      -- get base configuration
      baseCfg <- getBaseConfig (Just cfgDir) False True
      -- generate options for the transaction
      accCfg' <- snd <$> getAccountConfig sender baseCfg Nothing Nothing False
      let accCfg = accCfg' { acThreshold = fromIntegral (HM.size $ acKeys accCfg') }
      -- get Baker add transaction config
      let energy = bakerRemoveEnergyCost (HM.size $ acKeys accCfg)

      expiry <- (600 +) <$> getCurrentTimeUnix

      let AccountConfig{..} = accCfg
      let senderAddr = naAddr acAddr

      accountKeys <- liftIO $ failOnError $ decryptAccountKeyMap acKeys (passwordFromText password)

      let pl = Execution.RemoveBaker $ Types.BakerId bakerId
      -- run the transaction
      res <- runClient nodeBackend $ do
        currentNonce <- getBestBlockHash >>= getAccountNonce senderAddr
        let tx = encodeAndSignTransaction pl senderAddr energy currentNonce expiry accountKeys acThreshold
        return $ getBlockItemHash tx
      case res of
        Left _ -> return Nothing
        Right v -> return . Just . Text.pack $ show v)

  getBakers :: Handler GetBakersResponse
  getBakers = GetBakersResponse <$> do
      eeParams <- wrapIOError $ runClient nodeBackend $ withBestBlockHash Nothing getBirkParameters
      case eeParams of
          Right (Right jParams) -> case Aeson.fromJSON jParams of
                                    Aeson.Success params -> return $ Prelude.map (\b -> ((bpbrId b), (Text.pack $ show $ bpbrAccount b), (bpbrLotteryPower b))) $ bprBakers params
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


deployCredential :: EnvData -> IDTypes.CredentialDeploymentInformation -> IO Types.TransactionHash
deployCredential nodeBackend cdi = do
  let toDeploy = Types.CredentialDeployment cdi
  let cdiHash = getHash toDeploy :: Types.TransactionHash

  putStrLn $ "✅ Credentials sent to the baker and hooked: " ++ show cdiHash

  runGRPC nodeBackend (cdiHash <$ sendTransactionToBaker toDeploy 100)


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
            , thExpiry = Types.TransactionExpiryTime transactionExpiry
            }

  executeTransaction nodeBackend transaction

executeTransaction :: EnvData -> TransactionJSON -> IO Types.TransactionHash
executeTransaction nodeBackend transaction = do

  mdata <- loadContextData
  -- The networkId is for running multiple networks that's not the same chain, but hasn't been taken into use yet
  let nid = 1000

  t <- PR.evalContext mdata $ runGRPC nodeBackend $ processTransaction_ transaction nid True

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
{--

This chains together the full process that is split into seperate APIs above so
it can be more easily tested via `stack ghci` using `:r` for quick reloads.

Once proven together, relevant parts are integrated into the APIs above.

Currently, this seems to work all the way through to submitting onto the chain
and being accepted baked into a block with the new account credited 100 GTU.

NOTE: Remember to point the environment variable `GTU_DROP_KEYFILE` to a valid
keyfile (otherwise it'll not have any drop account to use to send GTUs from)

--}
debugTestFullProvision :: IO String
debugTestFullProvision = do

  nodeUrl <- Config.lookupEnvText "NODE_URL" "localhost:32798"
  pgUrl <- Config.lookupEnvText "PG_URL" "http://localhost:9200"
  idUrl <- Config.lookupEnvText "SIMPLEID_URL" "http://localhost:8000"
  rpcAdminToken <- Config.lookupEnvText "RPC_PASSWORD" "rpcadmin"

  putStrLn $ "✅ nodeUrl = " ++ Text.unpack nodeUrl
  putStrLn $ "✅ pgUrl = " ++ Text.unpack pgUrl
  putStrLn $ "✅ idUrl = " ++ Text.unpack idUrl
  putStrLn $ "✅ rpcAdminToken = " ++ Text.unpack rpcAdminToken

  putStrLn "➡️  Submitting IdObjectRequest"

  let
    (nodeHost, nodePort) =
      case splitOn ":" $ Text.unpack nodeUrl of
        nodeHostText:nodePortText:_ -> case readMaybe nodePortText of
          Just nodePortText' ->
            (nodeHostText, nodePortText')
          Nothing ->
            error $ "Could not parse port for given NODE_URL: " ++ nodePortText
        _ ->
          error $ "Could not parse host:port for given NODE_URL: " ++ Text.unpack nodeUrl

    grpcConfig = GrpcConfig { host = nodeHost, port = nodePort, grpcAuthenticationToken = Text.unpack rpcAdminToken, target = Nothing, retryNum = 5, timeout = Nothing }

  nodeBackend <- runExceptT (mkGrpcClient grpcConfig Nothing) >>= \case
    Left err -> fail (show err)
    Right envData -> return envData

  creationTime <- liftIO getCurrentTime

  let attributesStub =
        fromList
          [ ("dob", "19800101")
          , ("countryOfResidence", "184") -- (GB) Great Britain
          ]

      expiryDate = addUTCTime (60*60*24*365) creationTime -- Expires in 365 days from now

      idObjectRequest =
        IdObjectRequest
          { ipIdentity = 0
          , attributes =
              Attributes
                { chosenAttributes = attributesStub
                , createdAt = toISOYearMonth creationTime
                , validTo = toISOYearMonth expiryDate
                , maxAccounts = 30
                }
          , anonymityRevokers = [0,1,2]
          , threshold = 2
          }

  idObjectResponse <- postIdObjectRequest idUrl idObjectRequest

  putStrLn "✅ Got IdObjectResponse"

  let credentialRequest =
        IdCredentialRequest
          { ipIdentity = ipIdentity (idObjectResponse :: IdObjectResponse)
          , identityObject = identityObject (idObjectResponse :: IdObjectResponse)
          , idUseData = idUseData (idObjectResponse :: IdObjectResponse)
          , revealedAttributes = fromList [("dob","19800101")]
          , accountNumber = 0
          }

  putStrLn "➡️  Submitting IdCredentialRequest"

  idCredentialResponse <- postIdCredentialRequest idUrl credentialRequest

  putStrLn "✅ Got idCredentialResponse"

  putStrLn "✅ Generating JSON for idCredentialResponse:"
  putStrLn $ BS8.unpack $ encode idCredentialResponse

  putStrLn $ "✅ Deploying credentials for: " ++ show (SimpleIdClientApi.accountAddress idCredentialResponse)

  transactionHash <- liftIO $ deployCredential nodeBackend (credential (idCredentialResponse :: IdCredentialResponse))

  putStrLn $ "✅ Deployed credentials, transactionHash: " ++ show transactionHash

  let keyMap = SimpleIdClientApi.keys (accountData idCredentialResponse)
  let selfAddress = SimpleIdClientApi.accountAddress idCredentialResponse

  debugTestTransactions nodeBackend selfAddress keyMap


debugTestTransactions :: EnvData -> Types.AccountAddress -> AccountKeyMap -> IO String
debugTestTransactions nodeBackend selfAddress keyMap = do

  gtuDropAccountFile <- Config.lookupEnvTextWithoutDefault "GTU_DROP_KEYFILE"
  gtuDropAccount <- getGtuDropKeys gtuDropAccountFile
  putStrLn $ "✅ Requesting GTU Drop for: " ++ show selfAddress

  _ <- runGodTransaction nodeBackend (fromJust gtuDropAccount) $ Transfer { toaddress = Types.AddressAccount selfAddress, amount = 1000000 }

  let txHeader txBody nonce = Types.TransactionHeader {
    thSender = selfAddress,
    thNonce = nonce,
    thEnergyAmount = 10000,
    thPayloadSize = Types.payloadSize txBody,
    thExpiry = Types.TransactionExpiryTime maxBound
    }
  let sign txBody nonce = Types.NormalTransaction $ Types.signTransaction (HM.toList keyMap) (txHeader txBody nonce) txBody

  -- transfer to myself
  let txBodyTransfer = Execution.encodePayload (Execution.Transfer (Types.AddressAccount selfAddress) 1) -- transfer 1 GTU to myself.
  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign txBodyTransfer 1) 100)

  let txDelegateStake = Execution.encodePayload (Execution.DelegateStake 0) -- assuming baker 0 exists
  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign txDelegateStake 2) 100)

  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign txDelegateStake 3) 100)

  bakerKeys <- generateBakerKeys
  addBakerPayload <- Execution.encodePayload <$> generateBakerAddPayload bakerKeys AccountSigningData { asdAddress = selfAddress
                                                                                                      , asdKeys = keyMap
                                                                                                      , asdThreshold = fromIntegral (HM.size keyMap) }

  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign addBakerPayload 4) 100)

  let txDelegateStake' = Execution.encodePayload (Execution.DelegateStake 12312312312) -- should fail because baker should not exist
  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign txDelegateStake' 5) 100)

  -- invalid add baker payload
  addBakerPayload' <- Execution.encodePayload <$> generateBakerAddPayload bakerKeys AccountSigningData { asdAddress = selfAddress
                                                                                                       , asdKeys = HM.empty
                                                                                                       , asdThreshold = 1 }
  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign addBakerPayload' 6) 100)

  -- garbage payload
  let garbagePayload = Types.EncodedPayload BSS.empty
  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign garbagePayload 7) 100)

  let removeBakerPayload = Execution.encodePayload (Execution.RemoveBaker 0) -- should fail if baker 0 existed already
  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign removeBakerPayload 8) 100)

  pure "Done."


runDebugTestTransactions :: IO String
runDebugTestTransactions = do
  nodeUrl <- Config.lookupEnvText "NODE_URL" "localhost:32798"
  pgUrl <- Config.lookupEnvText "PG_URL" "http://localhost:9200"
  idUrl <- Config.lookupEnvText "SIMPLEID_URL" "http://localhost:8000"
  rpcPassword <- Config.lookupEnvText "RPC_PASSWORD" "rpcadmin"


  putStrLn $ "✅ nodeUrl = " ++ Text.unpack nodeUrl
  putStrLn $ "✅ pgUrl = " ++ Text.unpack pgUrl
  putStrLn $ "✅ idUrl = " ++ Text.unpack idUrl
  putStrLn $ "✅ rpcPassword = " ++ Text.unpack rpcPassword

  putStrLn "➡️  Submitting IdObjectRequest"

  let
    (nodeHost, nodePort) =
      case splitOn ":" $ Text.unpack nodeUrl of
        nodeHostText:nodePortText:_ -> case readMaybe nodePortText of
          Just nodePortText' ->
            (nodeHostText, nodePortText')
          Nothing ->
            error $ "Could not parse port for given NODE_URL: " ++ nodePortText
        _ ->
          error $ "Could not parse host:port for given NODE_URL: " ++ Text.unpack nodeUrl

    grpcConfig = GrpcConfig { host = nodeHost, port = nodePort, grpcAuthenticationToken = Text.unpack rpcPassword, target = Nothing, retryNum = 5, timeout = Nothing }

  nodeBackend <- runExceptT (mkGrpcClient grpcConfig Nothing) >>= \case
    Left err -> fail (show err)
    Right envData -> return envData


  uncurry (debugTestTransactions nodeBackend) localTestAccount


-- If you export a clean created account from wallet (without GTU Drop) and then
-- drop the details here, running `:r` and then `runDebugTestTransactions` in ghci
-- will auto-create a bunch of transaction types that will then show up in the wallet
localTestAccount :: Account
localTestAccount = do
  let
    acKeys =
      certainDecode [text|
        {
          "0": {
            "signKey": "2b691fa23b44587db2530aa72167c8c129405fe0997d92aa57b56c5cffa7be3f",
            "verifyKey": "bb22fbf84e0dfd58e0e66f44828cfbba1e345fef34e0d3b84c2d0d9820c1cc2e"
          },
          "1": {
            "signKey": "8cce05dbb16fa77e9ea17b7ad76a037ec9d01bc0bc5729e751b6c324f392a3d8",
            "verifyKey": "28ab64e9ce014e8fc1f78c06e1f68a5b31f42a9192f439d1474d090c073ba7d7"
          },
          "2": {
            "signKey": "d958933b532d2bc39cfd20ecec6a2351e531115d68ab1c3a6e0fff565de4051a",
            "verifyKey": "538baa3460dcd92aa2a0ef8b80977ae03f6074e885b08822cb6c8b1a7acc003f"
          }
        }
      |]
    acAddr =
      certainDecode "\"43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt\""
  (acKeys, acAddr)

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


toISOYearMonth :: UTCTime -> Text
toISOYearMonth t = Text.pack $ formatTime defaultTimeLocale "%Y%m" t
