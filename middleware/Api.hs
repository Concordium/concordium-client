{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Api where

import           Network.Wai (Application)
import           Control.Monad.Managed (liftIO)
import           Control.Exception (bracket)
import qualified Data.HashMap.Strict as HM
import           Data.Aeson (encode, decode')
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (FromJSON)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Parser
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Control.Exception as C
import qualified Data.ByteString.Short as BSS
import           Data.List.Split
import           Data.Map
import           Data.Time.Clock (addUTCTime, UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           System.Directory
import           System.Exit
import           System.Environment
import           System.IO.Error
import           System.IO
import           Control.Concurrent
import           System.Process
import           Text.Read (readMaybe)
import           Lens.Micro.Platform ((^.))
import           Safe (headMay)
import           Data.Time
import           Data.Time.Clock.POSIX
import           NeatInterpolation

import qualified Acorn.Parser.Runner as PR
import           Concordium.Client.Commands as COM
import           Concordium.Client.GRPC
import qualified Concordium.Client.GRPC as GRPC
import qualified Network.GRPC.Client.Helpers as GRPC
import           Concordium.Client.Runner
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction
import           Concordium.Client.Cli

import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import           Concordium.Types.HashableTo
import qualified Concordium.Types.Transactions as Types
import qualified Concordium.Types.Execution as Execution
import qualified Concordium.Client.Types.Transaction as Types
import           Control.Monad.Except
import           Control.Monad (forM_)
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
    address =
      certainDecode "\"43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt\""
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

    , accountBestBalance :: r :-
        "v1" :> "accountBestBalance" :> ReqBody '[JSON] Text
                                     :> Post '[JSON] Aeson.Value


    , transfer :: r :-
        "v1" :> "transfer" :> ReqBody '[JSON] TransferRequest
                           :> Post '[JSON] TransferResponse

    , typecheckCode :: r :-
        "v1" :> "typecheckCode" :> ReqBody '[JSON] BuildModulesRequest
                                    :> Post '[JSON] Aeson.Value


    , consensusStatus :: r :-
        "v1" :> "consensusStatus" :> Get '[JSON] Aeson.Value

    , blockInfo :: r :-
        "v1" :> "blockInfo" :> Capture "blockHash" Text
                              :> Get '[JSON] Aeson.Value

    , blockSummary :: r :-
        "v1" :> "blockSummary" :> Capture "blockHash" Text
                              :> Get '[JSON] Aeson.Value

    , contractState :: r :-
        "v1" :> "contractState" :> Capture "contractAddress" Text
                              :> Get '[JSON] Aeson.Value

    , transactionStatus :: r :-
        "v1" :> "transactionStatus" :> Capture "hash" Text
                              :> Get '[JSON] Aeson.Value

    , simpleTransactionStatus :: r :-
        "v1" :> "simpleTransactionStatus" :> Capture "hash" Text
                              :> Get '[JSON] Aeson.Value

    , publishCode :: r :-
        "v1" :> "publishCode" :> ReqBody '[JSON] PublishCodeRequest
                                  :> Post '[JSON] Aeson.Value

    , createContract :: r :-
        "v1" :> "createContract" :> ReqBody '[JSON] CreateContractRequest
                                  :> Post '[JSON] Aeson.Value

    , messageContract :: r :-
        "v1" :> "messageContract" :> ReqBody '[JSON] MessageContractRequest
                                  :> Post '[JSON] Aeson.Value

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


writeSourceCodeFile :: (ModuleName, Text) -> IO String
writeSourceCodeFile (moduleName, contents) = do
  -- TODO Validate name is only in [a-zA-Z]*
  let fileName = Text.unpack moduleName ++ ".midlang"
  TIO.writeFile fileName contents
  return fileName


{- Creates a fresh Midlang project on disk to run an action in, then deletes
 - the project after the action has completed, even if an exception is thrown
 -}
freshProjectBracket :: [(ModuleName, ModuleSourceCode)] -> ([String] -> IO a) -> IO a
freshProjectBracket moduleSources projectAction =
  let
    projectFiles :: String -> IO [String]
    projectFiles projectDir = do
      createDirectoryIfMissing True projectDir

      withCurrentDirectory projectDir $ do

        _ <- readProcessWithExitCode "/bin/bash" [] "yes | mid init"

        fileNames <- forM moduleSources writeSourceCodeFile

        return fileNames

    getFreshDir :: IO String
    getFreshDir = do
      time <- getCurrentTime
      let unixTime = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds time
      return $ "tmp/" ++ show unixTime ++ "/"
  in
    do
      freshDir <- getFreshDir
      bracket
        (projectFiles freshDir)
        (\_ -> removePathForcibly freshDir)
        (\files -> withCurrentDirectory freshDir $ projectAction files)


servantApp :: EnvData -> Text -> Text -> Application
servantApp nodeBackend pgUrl idUrl = genericServe routesAsServer
 where
  routesAsServer = Routes {..} :: Routes AsServer


  -- Authority is a host and port, e.g. localhost:32000
  nodeAuthority :: String
  nodeAuthority =
    let
      nodeConfig = GRPC.config nodeBackend
      host = GRPC._grpcClientConfigHost nodeConfig
      port = show $ GRPC._grpcClientConfigPort nodeConfig
    in
      host ++ ":" ++ port


  typecheckCode :: BuildModulesRequest -> Handler Aeson.Value
  typecheckCode (BuildModulesRequest moduleContents) =

      {- Rather hacky but KISS approach to "integrating" with the Midlang compiler
      In future this code will probably be directly integrated into the client
      and call the compiler in memory, avoiding filesystem entirely.

      Known issues (some quick fixes ahead of proper future integration):

      - Not thread-safe, if we get two contracts compiling at the same time it'll overwrite and cause issues
        - Fix by using proper temp file system for target + compilation
      - elm.json required to be manually placed in project / folder
        - Fix by inlining it and ensuring it's written before compilation
      - Fairly dodgy string based status mapping between FE/BE

      -}

    liftIO $ freshProjectBracket moduleContents (\(sourceCodeFileNames) -> do

      let midlangArgs = ["build"] ++ sourceCodeFileNames ++ ["--report=json"]

      (exitCode, stdout, stderr)
        <- readProcessWithExitCode "mid" midlangArgs []

      let decodedCompilerResponse =
            case exitCode of
              ExitSuccess ->
                Aeson.eitherDecode' $ BS8.pack stdout
              ExitFailure code ->
                if code == 1 then
                    Aeson.eitherDecode' $ BS8.pack stderr
                else
                    Left $ "Unexpected exit code: " <> show code

      case decodedCompilerResponse of
        Right json -> pure json
        Left errMsg -> pure $ Aeson.String $ Text.pack errMsg -- TODO Encode a new kind of JSON error that the Elm code can understand?
    )


  publishCode :: PublishCodeRequest -> Handler Aeson.Value
  publishCode (PublishCodeRequest account energyLimit moduleContents) =

    liftIO $ freshProjectBracket moduleContents (\(sourceCodeFileNames) -> do

      -- TODO Give credentials via the command line (needs enabling in the Midlang compiler)
      let midlangArgs = ["deploy", nodeAuthority, show energyLimit, "--report=json" ] ++ sourceCodeFileNames

      (exitCode, stdout, stderr)
        <- readProcessWithExitCode "mid" midlangArgs []

      let decodedCompilerResponse =
            case exitCode of
              ExitSuccess ->
                Aeson.eitherDecode' $ BS8.pack stdout
              ExitFailure code ->
                if code == 1 then
                    Aeson.eitherDecode' $ BS8.pack stderr
                else
                    Left $ "Unexpected exit code: " <> show code

      case decodedCompilerResponse of
        Right json -> pure json
        Left errMsg -> pure $ Aeson.String $ Text.pack errMsg -- TODO Encode a new kind of JSON error that the Elm code can understand?
    )


  createContract :: CreateContractRequest -> Handler Aeson.Value
  createContract (CreateContractRequest account creationArgs energyLimit moduleName contractName moduleContents) =
    {-
     - The `start-contract` initializes an Midlang contract on the Concordium Network:
     -
     -     mid start-contract <midlang-expression> <node> <energy> <Midlang-module-name> <Midlang-contract-name>
     -
     - For example:
     -
     -     mid start-contract Nat.Zero 127.0.0.1:32000 50 ExampleContractModule ExampleContract
     -
     - This tries to compile the given Midlang expression and pass that to the named
     - contract's `init` function in order to create an instance of it via a node at
     - socket address 127.0.0.1:32000, with an energy limit of 50.
     -}

    liftIO $ freshProjectBracket moduleContents (\(sourceCodeFileNames) -> do

      -- TODO Give credentials via the command line (needs enabling in the Midlang compiler)
      let midlangArgs = ["start-contract", Text.unpack creationArgs, nodeAuthority, show energyLimit, Text.unpack moduleName, Text.unpack contractName, "--report=json" ]

      (exitCode, stdout, stderr)
        <- readProcessWithExitCode "mid" midlangArgs []

      let decodedCompilerResponse =
            case exitCode of
              ExitSuccess ->
                Aeson.eitherDecode' $ BS8.pack stdout
              ExitFailure code ->
                if code == 1 then
                    Aeson.eitherDecode' $ BS8.pack stderr
                else
                    Left $ "Unexpected exit code: " <> show code

      case decodedCompilerResponse of
        Right json -> pure json
        Left errMsg -> pure $ Aeson.String $ Text.pack errMsg -- TODO Encode a new kind of JSON error that the Elm code can understand?
    )



  messageContract :: MessageContractRequest -> Handler Aeson.Value
  messageContract (MessageContractRequest account amount maybeMessageContents energyLimit moduleName contractName contractAddress moduleContents) =
    {-
     - The `message-contract` sends a message to an Midlang contract on the Concordium
     - Network:
     -
     -     mid message-contract <GTU-amount> <midlang-expression> <node> <energy> <Midlang-module-name> <Midlang-contract-name> <contract-address>
     -
     - For example:
     -
     -     mid message-contract 123 Op.Increment 127.0.0.1:32000 50 ExampleContractModule ExampleContract '{ "index" : 100, "subindex" : 1 }'
     -}

    -- If there are no message contents, this amounts to just a simple transfer to a contract, and that isn't something `mid message-contract` supports at the moment
    case maybeMessageContents of

      Nothing ->
        error "No message given for messaging contract" -- TODO Complete a simple transfer (or equivalent for sending GTU to a contract instance) instead

      Just messageArgs ->

        liftIO $ freshProjectBracket moduleContents (\(sourceCodeFileNames) -> do

          -- TODO Give credentials via the command line (needs enabling in the Midlang compiler)
          let midlangArgs =
                [ "message-contract"
                , show amount
                , Text.unpack messageArgs
                , nodeAuthority
                , show energyLimit
                , Text.unpack moduleName
                , Text.unpack contractName
                , BS8.unpack $ Aeson.encode contractAddress
                , "--report=json"
                ]

          (exitCode, stdout, stderr)
            <- readProcessWithExitCode "mid" midlangArgs []

          let decodedCompilerResponse =
                case exitCode of
                  ExitSuccess ->
                    Aeson.eitherDecode' $ BS8.pack stdout
                  ExitFailure code ->
                    if code == 1 then
                        Aeson.eitherDecode' $ BS8.pack stderr
                    else
                        Left $ "Unexpected exit code: " <> show code

          case decodedCompilerResponse of
            Right json -> pure json
            Left errMsg -> pure $ Aeson.String $ Text.pack errMsg -- TODO Encode a new kind of JSON error that the Elm code can understand?
        )


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

      toOutcome (Right p) = Just (outcomeFromPretty p)
      toOutcome _ = Nothing

    outcomes <- liftIO $ processAccounts (Text.encodeUtf8 pgUrl) address $ (mapWhileC toOutcome .| sinkList)

    pure $ AccountTransactionsResponse outcomes address


  accountNonFinalizedTransactions :: Text -> Handler Aeson.Value
  accountNonFinalizedTransactions address = liftIO $ proxyGrpcCall nodeBackend (GRPC.getAccountNonFinalizedTransactions address)

  accountBestBalance :: Text -> Handler Aeson.Value
  accountBestBalance address = do
    liftIO $ proxyGrpcCall nodeBackend
      (GRPC.withBestBlockHash Nothing (GRPC.getAccountInfo address))


  transfer :: TransferRequest -> Handler TransferResponse
  transfer TransferRequest{..} = do

    let (AccountWithKeys accountAddress keymap) = account

    liftIO $ putStrLn $ "✅ Sending " ++ show amount ++ " from " ++ show accountAddress ++ " to " ++ show to

    transactionId <- liftIO $ runTransaction nodeBackend (Transfer { toaddress = to, amount = amount }) (accountToPair account)

    pure $ TransferResponse { transactionId = transactionId }


  consensusStatus :: Handler Aeson.Value
  consensusStatus = liftIO $ proxyGrpcCall nodeBackend (GRPC.getConsensusStatus)

  
  blockInfo :: Text -> Handler Aeson.Value
  blockInfo blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockInfo blockhash)


  blockSummary :: Text -> Handler Aeson.Value
  blockSummary blockhash = liftIO $ proxyGrpcCall nodeBackend (GRPC.getBlockSummary blockhash)


  contractState :: Text -> Handler Aeson.Value
  contractState contractAddress = liftIO $ proxyGrpcCall nodeBackend (withBestBlockHash Nothing (GRPC.getInstanceInfo contractAddress))


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

  nodeUrl <- Config.lookupEnvText "NODE_URL" "localhost:32798"
  pgUrl <- Config.lookupEnvText "PG_URL" "http://localhost:9200"
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

    grpcConfig = GrpcConfig { host = nodeHost, port = nodePort, target = Nothing, retryNum = 5, timeout = Nothing }

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


debugTestTransactions :: EnvData -> Types.AccountAddress -> Types.KeyMap -> IO String
debugTestTransactions nodeBackend selfAddress keyMap = do

  putStrLn $ "✅ Requesting GTU Drop for: " ++ show selfAddress

  _ <- runGodTransaction nodeBackend $ Transfer { toaddress = Types.AddressAccount selfAddress, amount = 1000000 }

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

  let txUndelegateStake = Execution.encodePayload Execution.UndelegateStake
  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign txDelegateStake 3) 100)

  bakerKeys <- generateBakerKeys
  addBakerPayload <- Execution.encodePayload <$> generateAddBakerPayload bakerKeys AccountKeys{akAddress = selfAddress,
                                                                                               akKeys = keyMap,
                                                                                               akThreshold = fromIntegral (HM.size keyMap) }

  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign addBakerPayload 4) 100)

  let txDelegateStake' = Execution.encodePayload (Execution.DelegateStake 12312312312) -- should fail because baker should not exist
  _ <- runGRPC nodeBackend (sendTransactionToBaker (sign txDelegateStake' 5) 100)

  -- invalid add baker payload
  addBakerPayload' <- Execution.encodePayload <$> generateAddBakerPayload bakerKeys AccountKeys { akAddress = selfAddress,
                                                                                                  akKeys = HM.empty,
                                                                                                  akThreshold = 1 }
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

    grpcConfig = GrpcConfig { host = nodeHost, port = nodePort, target = Nothing, retryNum = 5, timeout = Nothing }

  nodeBackend <- runExceptT (mkGrpcClient grpcConfig Nothing) >>= \case
    Left err -> fail (show err)
    Right envData -> return envData


  debugTestTransactions nodeBackend (fst localTestAccount) (snd localTestAccount)


-- If you export a clean created account from wallet (without GTU Drop) and then
-- drop the details here, running `:r` and then `runDebugTestTransactions` in ghci
-- will auto-create a bunch of transaction types that will then show up in the wallet
localTestAccount :: Account
localTestAccount = do
  let
    keyMap =
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
    address =
      certainDecode "\"43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt\""
  (address, keyMap)


debugGrpc :: IO GetNodeStateResponse
debugGrpc = do
  let nodeBackend = COM.GRPC { grpcHost = "localhost", grpcPort = 11103, grpcTarget = Nothing, grpcRetryNum = 5 }

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
