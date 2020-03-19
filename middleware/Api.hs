{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Api where

import           Network.Wai (Application)
import           Control.Monad.Managed (liftIO)
import           Data.Aeson (encode, decode')
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (FromJSON)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.List.Split
import           Data.Map
import           Data.Time.Clock.POSIX
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Directory
import           System.Exit
import           System.Environment
import           System.IO.Error
import           System.Process
import           Text.Read (readMaybe)
import           Lens.Simple ((^.))
import           Safe (headMay)
import NeatInterpolation

import qualified Acorn.Parser.Runner as PR
import           Concordium.Client.Commands as COM
import           Concordium.Client.GRPC
import qualified Concordium.Client.GRPC as GRPC
import           Concordium.Client.Runner
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction

import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import           Concordium.Types.HashableTo
import qualified Concordium.Types.Transactions as Types
import qualified Concordium.Types.Execution as Execution
import qualified Concordium.Client.Types.Transaction as Types
import           Control.Monad.Except
import qualified Proto.ConcordiumP2pRpc_Fields as CF

import qualified Config
import           SimpleIdClientApi
import           Api.Types

-- Account transactions
import           Conduit
import           PerAccountTransactions
import qualified Concordium.Client.TransactionStatus


middlewareGodAccount :: Account
middlewareGodAccount = do
  -- https://gitlab.com/Concordium/genesis-data/-/blob/master/beta_accounts/beta-account-0.json
  let
    keyMap =
      certainDecode [text|
        {
          "0": {
            "signKey": "db864d164a9cbb572aec4a86ab8601d5daf3b5ca20bf47a72188da67f29f6e9b",
            "verifyKey": "d1775f75013889777fedebf6511153adff6b567239edcc10a7e62bc4e88b9c62"
          },
          "1": {
            "signKey": "bcfce29a522b44fa07d16629c8731b470de7df99a0af6b504e3b880d414b316e",
            "verifyKey": "00d56fe011d7fad86eb19af6d8111ac786197a446bf49f4744baf8bf2f8f27b9"
          },
          "2": {
            "signKey": "05b88a98c986f326bd7523092c3088ad5f31c80836669c0562fe6ff7e58054bc",
            "verifyKey": "d3782ec10031ae864373b12d5b05e79576d0221f95d1d4de5649d8f449e0d4ed"
          }
        }
      |]
    address =
      certainDecode "\"3ZFGxLtnUUSJGW2WqjMh1DDjxyq5rnytCwkSqxFTpsWSFdQnNn\""
  (address, keyMap)


adminAuthToken :: Text
adminAuthToken = "47434137412923191713117532"


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

    , transfer :: r :-
        "v1" :> "transfer" :> ReqBody '[JSON] TransferRequest
                           :> Post '[JSON] TransferResponse

    , typecheckContract :: r :-
        "v1" :> "typecheckContract" :> ReqBody '[JSON] Text
                                    :> Post '[JSON] Text

    , consensusStatus :: r :-
        "v1" :> "consensusStatus" :> Get '[JSON] Aeson.Value

    , blockInfo :: r :-
        "v1" :> "blockInfo" :> Capture "blockHash" Text
                              :> Get '[JSON] Aeson.Value

    , transactionStatus :: r :-
        "v1" :> "transactionStatus" :> Capture "hash" Text
                              :> Get '[JSON] Aeson.Value

    , simpleTransactionStatus :: r :-
        "v1" :> "simpleTransactionStatus" :> Capture "hash" Text
                              :> Get '[JSON] Concordium.Client.TransactionStatus.SimpleTransactionStatusResult

    -- Private Middleware APIs (accessible on local client instance of Middleware only)
    , getNodeState :: r :-
        "v1" :> "nodeState" :> Get '[JSON] GetNodeStateResponse

    , setNodeState :: r :-
        "v1" :> "nodeState" :> ReqBody '[JSON] SetNodeStateRequest
                            :> Post '[JSON] SetNodeStateResponse
    }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)


servantApp :: EnvData -> Text -> Text -> Application
servantApp nodeBackend pgUrl idUrl = genericServe routesAsServer
 where
  routesAsServer = Routes {..} :: Routes AsServer


  typecheckContract :: Text -> Handler Text
  typecheckContract contractCode =
    liftIO $ do

      {- Rather hacky but KISS approach to "integrating" with the oak compiler
      In future this code will probably be directly integrated into the client
      and call the compiler in memory, avoiding filesystem entirely.

      Known issues (some quick fixes ahead of proper future integration):

      - Not thread-safe, if we get two contracts compiling at the same time it'll overwrite and cause issues
        - Fix by using proper temp file system for target + compilation
      - elm.json required to be manually placed in project / folder
        - Fix by inlining it and ensuring it's written before compilation
      - Fairly dodgy string based status mapping between FE/BE

      -}

      createDirectoryIfMissing True "tmp/"
      TIO.writeFile "tmp/Contract.elm" contractCode

      (exitCode, stdout, stderr)
        <- readProcessWithExitCode "oak" ["build", "tmp/Contract.elm"] []

      case exitCode of
        ExitSuccess ->
          pure $ Text.pack stdout
        ExitFailure code ->
          if code == 1 then
              pure $ Text.pack stderr
          else
              pure $ "Unexpected exit code" <> Text.pack (show code)


  betaIdProvision :: BetaIdProvisionRequest -> Handler Aeson.Value
  betaIdProvision BetaIdProvisionRequest{..} = do

    liftIO $ putStrLn "✅ Got the following attributes:"
    liftIO $ print attributes

    creationTime :: Int <- liftIO $ round `fmap` getPOSIXTime

    let
      expiryDate = creationTime + (60*60*24*365) -- Expires in 365 days from now
      idObjectRequest =
          IdObjectRequest
            { ipIdentity = 5
            , name = "concordium-testnet-issuer"
            , attributes =
                Attributes
                  { chosenAttributes =
                      fromList
                        ([ ("CreationTime", Text.pack $ show creationTime)
                        , ("MaxAccount", "30")
                        ] ++ attributes)
                  , expiryDate = expiryDate
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
            , revealedItems = revealedItems (accountProvisionRequest :: BetaAccountProvisionRequest)
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
  testnetGtuDrop toAddress = do
    case toAddress of
      Types.AddressAccount address -> do

        accountResult <- liftIO $ runGRPC nodeBackend (withBestBlockHash Nothing (getAccountInfo $ Text.pack . show $ address))
        nonce <- liftIO $ fst <$> runGRPC nodeBackend (getAccountNonceBestGuess address)

        case accountResult of
          Right accountJson -> do
            let (accountR :: Aeson.Result AccountInfoResponse) = Aeson.fromJSON accountJson
            case accountR of
              Aeson.Success account ->
                if nonce == Types.minNonce && accountAmount account == 0
                  then do
                    liftIO $ putStrLn $ "✅ Requesting GTU Drop for " ++ show toAddress
                    transactionId <- liftIO $ runGodTransaction nodeBackend $ Transfer { toaddress = toAddress, amount = 1000000 }
                    pure $ TestnetGtuDropResponse { transactionId = transactionId }

                  else
                    throwError $ err403 { errBody = "GTU drop can only be used once per account." }

              Aeson.Error err ->
                if err == "parsing Account info failed, expected Object, but encountered Null" then
                  throwError $ err409 { errBody = "Account is not yet on the network." }
                else
                  throwError $ err502 { errBody = "JSON error: " <> BS8.pack err }

          Left err ->
            throwError $ err502 { errBody = "GRPC error: " <> BS8.pack err }
      _ ->
        throwError $ err403 { errBody = "GTU drop can only be used for Account addresses." }


  accountTransactions :: Types.AccountAddress -> Handler AccountTransactionsResponse
  accountTransactions address = do
    let
      accountAddress address =
        case address of
          Types.AddressAccount a -> Just a
          _ -> Nothing

      contractAddress address =
        case address of
          Types.AddressContract a -> Just a
          _ -> Nothing

      toTransactionOutcome ep =
        case ep of
          Right p -> do

            hash <- Text.pack . show <$> peTransactionHash p
            summary <-
              case peTransactionSummary p of
                Right s -> Just s
                Left _ -> Nothing

            event <-
              case Execution.tsResult summary of
                Execution.TxSuccess events ->
                  headMay events

                Execution.TxReject reason ->
                  Nothing

            (from, amount, to) <-
              case event of
                Execution.Transferred f a t ->
                  Just (f, a, t)

                _ -> Nothing

            Just $
              TransactionOutcome
                { id = hash
                , message_type = "DirectTransfer"
                -- JS expects timestamps in milliseconds
                , timestamp = Text.pack . show $ (peBlockTime p * 1000)
                , block_hash = Text.pack . show $ peBlockHash p
                , slot = Text.pack . show $ peBlockHeight p
                , transaction_hash = hash
                , amount = Text.pack . show $ amount
                , from_account = accountAddress from
                , to_account = accountAddress to
                , from_contract = contractAddress from
                , to_contract = contractAddress to
                , finalized = True
                }

          Left _ -> Nothing

    outcomes <- liftIO $ processAccounts (Text.encodeUtf8 pgUrl) address $ (mapWhileC toTransactionOutcome .| sinkList)

    pure $ AccountTransactionsResponse outcomes address


  accountNonFinalizedTransactions :: Text -> Handler Aeson.Value
  accountNonFinalizedTransactions address = liftIO $ proxyGrpcCall nodeBackend (GRPC.getAccountNonFinalizedTransactions address)


  transfer :: TransferRequest -> Handler TransferResponse
  transfer TransferRequest{..} = do

    let
      (accountAddress, keymap) = account

    liftIO $ putStrLn $ "✅ Sending " ++ show amount ++ " from " ++ show accountAddress ++ " to " ++ show to

    transactionId <- liftIO $ runTransaction nodeBackend (Transfer { toaddress = to, amount = amount }) account

    pure $ TransferResponse { transactionId = transactionId }


  consensusStatus :: Handler Aeson.Value
  consensusStatus = liftIO $ proxyGrpcCall nodeBackend (GRPC.getConsensusStatus)


  blockInfo :: Text -> Handler Aeson.Value
  blockInfo blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockInfo blockhash)


  transactionStatus :: Text -> Handler Aeson.Value
  transactionStatus hash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getTransactionStatus hash)


  simpleTransactionStatus :: Text -> Handler Concordium.Client.TransactionStatus.SimpleTransactionStatusResult
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
        throwError $ err400 { errBody = "Invalid transcation hash." }


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

    case infoE of
      Right ni ->
        pure $
          GetNodeStateResponse
            { id = ni ^. (CF.nodeId . CF.value)
            , running = ni ^. CF.consensusRunning
            , isBaking = ni ^. CF.consensusBakerRunning
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

proxyGrpcCall :: EnvData -> ClientMonad IO (Either a Aeson.Value) -> IO Aeson.Value
proxyGrpcCall nodeBackend query = do
  result <- runGRPC nodeBackend query
  case result of
    Right obj ->
      pure $ Aeson.toJSON obj
    _ ->
      pure $ Aeson.Null



-- For beta, uses the middlewareGodAccount which is seeded with funds
runGodTransaction :: EnvData -> TransactionJSONPayload -> IO Types.TransactionHash
runGodTransaction nodeBackend payload =
  runTransaction nodeBackend payload middlewareGodAccount


runGRPC :: (MonadIO m) => EnvData -> ClientMonad m a -> m a
runGRPC envData c =
  runClient envData c >>= \case
    Left err -> fail (show err)
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
        Transfer _ _       -> 165
        DeployCredential _ -> 10000
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


type Account = (IDTypes.AccountAddress, Types.KeyMap)


executeTransaction :: EnvData -> TransactionJSON -> IO Types.TransactionHash
executeTransaction nodeBackend transaction = do

  mdata <- loadContextData
  -- The networkId is for running multiple networks that's not the same chain, but hasn't been taken into use yet
  let nid = 1000

  t <- do
    PR.evalContext mdata $ runGRPC nodeBackend $ processTransaction_ transaction nid True

  putStrLn $ "✅ Transaction sent to the baker and hooked: " ++ show (getHash t :: Types.TransactionHash)
  print transaction

  pure $ getHash t

-- Dirty helper to help us with "definitely certain" value decoding
certainDecode :: (FromJSON a) => Text -> a
certainDecode t =
  case decode' $ BS8.fromStrict $ Text.encodeUtf8 t of
    Just v -> v
    Nothing -> error $ "Well... not so certain now huh! certainDecode failed on:\n\n" ++ Text.unpack t


{--

This chains together the full process that is split into seperate APIs above so
it can be more easily tested via `stack ghci` using `:r` for quick reloads.

Once proven together, relevant parts are integrated into the APIs above.

Currently, this seems to work all the way through to submitting onto the chain
and being accepted baked into a block with the new account credited 100 GTU.

--}
debugTestFullProvision :: IO String
debugTestFullProvision = do

  nodeUrl <- Config.lookupEnvText "NODE_URL" "localhost:11104"
  pgUrl <- Config.lookupEnvText "ES_URL" "http://localhost:9200"
  idUrl <- Config.lookupEnvText "SIMPLEID_URL" "http://localhost:8000"

  putStrLn $ "✅ nodeUrl = " ++ Text.unpack nodeUrl
  putStrLn $ "✅ pgUrl = " ++ Text.unpack pgUrl
  putStrLn $ "✅ idUrl = " ++ Text.unpack idUrl

  putStrLn "➡️  Submitting IdObjectRequest"

  let
    (nodeHost, nodePort) =
      case splitOn ":" $ Text.unpack nodeUrl of
        nodeHostText:nodePortText:_ -> case readMaybe nodePortText of
          Just nodePortText ->
            (nodeHostText, nodePortText)
          Nothing ->
            error $ "Could not parse port for given NODE_URL: " ++ nodePortText
        _ ->
          error $ "Could not parse host:port for given NODE_URL: " ++ Text.unpack nodeUrl

    grpcConfig = GrpcConfig { host = nodeHost, port = nodePort, target = Nothing }

  nodeBackend <- runExceptT (mkGrpcClient grpcConfig) >>= \case
    Left err -> fail (show err)
    Right envData -> return envData

  creationTime :: Int <- liftIO $ round `fmap` getPOSIXTime

  let attributesStub =
        [ ("DateOfBirth", "2000")
        , ("CountryOfResidence", "184") -- (GB) Great Britain
        ]

      expiryDate = creationTime + (60*60*24*365) -- Expires in 365 days from now

      idObjectRequest =
        IdObjectRequest
          { ipIdentity = 5
          , name = "middleware-beta-debug"
          , attributes =
              Attributes
                { chosenAttributes =
                    fromList
                      ([ ("CreationTime", Text.pack $ show creationTime)
                      , ("MaxAccount", "30")
                      ] ++ attributesStub)
                , expiryDate = expiryDate
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
          , revealedItems = ["DateOfBirth"]
          , accountNumber = 0
          }

  putStrLn "➡️  Submitting IdCredentialRequest"

  idCredentialResponse <- postIdCredentialRequest idUrl credentialRequest

  putStrLn "✅ Got idCredentialResponse"

  putStrLn "✅ Generating JSON for idCredentialResponse:"
  putStrLn $ BS8.unpack $ encode idCredentialResponse

  let
    newAddress = Types.AddressAccount $ SimpleIdClientApi.accountAddress idCredentialResponse

  putStrLn $ "✅ Deploying credentials for: " ++ show (SimpleIdClientApi.accountAddress idCredentialResponse)

  transactionHash <- liftIO $ deployCredential nodeBackend (credential (idCredentialResponse :: IdCredentialResponse))

  putStrLn $ "✅ Deployed credentials, transactionHash: " ++ show transactionHash

  putStrLn $ "✅ Requesting GTU Drop for: " ++ show newAddress

  _ <- runGodTransaction nodeBackend $ Transfer { toaddress = newAddress, amount = 100 }

  pure "Done."


debugGrpc :: IO GetNodeStateResponse
debugGrpc = do
  let nodeBackend = COM.GRPC { grpcHost = "localhost", grpcPort = 11103, grpcTarget = Nothing }

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

  case infoE of
    Right ni -> do
      print ni

      pure $
        GetNodeStateResponse
          { id = ni ^. (CF.nodeId . CF.value)
          , running = ni ^. CF.consensusRunning
          , isBaking = ni ^. CF.consensusBakerRunning
          , isInBakingCommittee = ni ^. CF.consensusBakerCommittee
          , isFinalizing = True
          , isInFinalizingCommittee = ni ^. CF.consensusFinalizerCommittee
          , signatureVerifyKey = ""
          , selectionVerifyKey = ""
          , timestamp = 0
          , ..
          }

    Left err ->
      error $ show err
