{-# LANGUAGE DataKinds #-}
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
import           Lens.Simple
import qualified Data.ByteString.Base16 as Base16
import qualified Data.HashMap.Strict as Map
import NeatInterpolation

import qualified Acorn.Parser.Runner as PR
import           Concordium.Client.Commands as COM
import           Concordium.Client.GRPC
import           Concordium.Client.Runner
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction
import           Concordium.Crypto.Ed25519Signature (deriveVerifyKey)
import           Concordium.Crypto.SignatureScheme (KeyPair(..), correspondingVerifyKey)
import qualified Concordium.Crypto.SHA256 as SHA256

import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import qualified Concordium.Types.Transactions as Types
import qualified Concordium.Client.Types.Transaction as Types
import           Control.Monad
import qualified Proto.ConcordiumP2pRpc_Fields as CF

import qualified Config
import           SimpleIdClientApi
import           EsApi
import           Api.Messages


middlewareGodAccount :: Account
middlewareGodAccount = do
  -- https://gitlab.com/Concordium/genesis-data/-/blob/master/beta_accounts/beta-account-0.json
  let
    keyMap =
      certainDecode [text|
        {
          "0": {
            "signKey": "438fe75467a2b7167f8834691ecf0b91e7dc9443b4b6056431e484db00934967",
            "verifyKey": "92efe3e541a90661bf0cdead5eb34b19843c1473143b551d90fb5bbbbda69ac8"
          },
          "1": {
            "signKey": "38cfc93274834c9c4f4bfad26f3251ac212986a4e464ade632c40e358009d6af",
            "verifyKey": "cb87fd5472146a4b9c914c2038704c139ae83057bba1170b05a4580e4f8353fd"
          },
          "2": {
            "signKey": "41b0919117d1e006ac069834b01f8c8f7f1267ac0dc288f200fde981917c3558",
            "verifyKey": "42b581be3991b77fe9ef3b40bbd2944cc7a23799b941b31231adb162df054d4b"
          }
        }
      |]
    address =
      certainDecode "\"3c3WY2QPmM8jfFb1bVDMtW9o51FwoiB3rEJDvxadUqMgFXKMk3\""
  (address, keyMap)


adminAuthToken :: Text
adminAuthToken = "47434137412923191713117532"


data Routes r = Routes
    -- Public Middleware APIs
    { betaIdProvision :: r :-
        "v1" :> "betaIdProvision" :> ReqBody '[JSON] BetaIdProvisionRequest
                                  :> Post '[JSON] BetaIdProvisionResponse

    , betaAccountProvision :: r :-
        "v1" :> "betaAccountProvision" :> ReqBody '[JSON] BetaAccountProvisionRequest
                                       :> Post '[JSON] BetaAccountProvisionResponse

    , betaGtuDrop :: r :-
        "v1" :> "betaGtuDrop" :> ReqBody '[JSON] Types.Address
                              :> Post '[JSON] BetaGtuDropResponse

    , accountTransactions :: r :-
        "v1" :> "accountTransactions" :> ReqBody '[JSON] Text
                                      :> Post '[JSON] AccountTransactionsResponse

    , transfer :: r :-
        "v1" :> "transfer" :> ReqBody '[JSON] TransferRequest
                           :> Post '[JSON] TransferResponse

    , typecheckContract :: r :-
        "v1" :> "typecheckContract" :> ReqBody '[JSON] Text
                                    :> Post '[JSON] Text

    -- Private Middleware APIs (accessible on local client instance of Middleware only)
    , getNodeState :: r :-
        "v1" :> "nodeState" :> Get '[JSON] GetNodeStateResponse

    , setNodeState :: r :-
        "v1" :> "nodeState" :> ReqBody '[JSON] SetNodeStateRequest
                            :> Post '[JSON] SetNodeStateResponse

    , replayTransactions :: r :-
        "v1" :> "replayTransactions" :> ReqBody '[JSON] ReplayTransactionsRequest
                                     :> Post '[JSON] ReplayTransactionsResponse
    }
  deriving (Generic)

api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)


servantApp :: COM.Backend -> Text -> Text -> Application
servantApp nodeBackend esUrl idUrl = genericServe routesAsServer
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

      (exitCode, _, stderr)
        <- readProcessWithExitCode "oak" ["build", "tmp/Contract.elm"] []

      case exitCode of
        ExitSuccess ->
          pure "ok"
        ExitFailure code ->
          if code == 1 then
              pure $ Text.pack stderr
          else
              pure "unknownerr"


  betaIdProvision :: BetaIdProvisionRequest -> Handler BetaIdProvisionResponse
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

    pure idObjectResponse


  betaAccountProvision :: BetaAccountProvisionRequest -> Handler BetaAccountProvisionResponse
  betaAccountProvision accountProvisionRequest = do

    -- Re-use the account nonce mechanism with the hashed prfKey for accountNumber increment per credential
    let idUseData_ = idUseData (accountProvisionRequest :: BetaAccountProvisionRequest)
        aci_ = aci (idUseData_ :: IdUseData)
        prfKey_ = prfKey (aci_ :: IdUseDataAci)
        accountIdKey = Text.decodeUtf8 . Base16.encode . SHA256.hashToByteString . SHA256.hash . Text.encodeUtf8 $ prfKey_

    (Types.Nonce nonce) <- liftIO $ EsApi.takeNextNonceFor esUrl accountIdKey

    let credentialRequest =
          IdCredentialRequest
            { ipIdentity = ipIdentity (accountProvisionRequest :: BetaAccountProvisionRequest)
            , identityObject = identityObject (accountProvisionRequest :: BetaAccountProvisionRequest)
            , idUseData = idUseData (accountProvisionRequest :: BetaAccountProvisionRequest)
            , revealedItems = revealedItems (accountProvisionRequest :: BetaAccountProvisionRequest)
            , accountNumber = fromIntegral nonce
            }

    idCredentialResponse <- liftIO $ postIdCredentialRequest idUrl credentialRequest

    liftIO $ putStrLn "✅ Got idCredentialResponse"

    let
      newAccountAddress = accountAddress (idCredentialResponse :: IdCredentialResponse)

    _ <- liftIO $ runGodTransaction nodeBackend esUrl $ DeployCredential { credential = credential (idCredentialResponse :: IdCredentialResponse) }

    pure $
      BetaAccountProvisionResponse
        { accountKeys = SimpleIdClientApi.keys $ SimpleIdClientApi.accountData (idCredentialResponse :: IdCredentialResponse)
        , spio = credential (idCredentialResponse :: IdCredentialResponse)
        , address = Text.pack . show $ newAccountAddress
        }


  betaGtuDrop :: Types.Address -> Handler BetaGtuDropResponse
  betaGtuDrop toAddress = do

    liftIO $ putStrLn $ "✅ Requesting GTU Drop for " ++ show toAddress

    transactionId <- liftIO $ runGodTransaction nodeBackend esUrl $ Transfer { toaddress = toAddress, amount = 1000000 }

    pure $ BetaGtuDropResponse { transactionId = transactionId }


  accountTransactions :: Text -> Handler AccountTransactionsResponse
  accountTransactions address =
    liftIO $ EsApi.getAccountTransactions esUrl address


  transfer :: TransferRequest -> Handler TransferResponse
  transfer TransferRequest{..} = do

    let
      (accountAddress, keymap) = account

    liftIO $ putStrLn $ "✅ Sending " ++ show amount ++ " from " ++ show accountAddress ++ " to " ++ show to

    transactionId <- liftIO $ runTransaction nodeBackend esUrl (Transfer { toaddress = to, amount = amount }) account

    pure $ TransferResponse { transactionId = transactionId }


  getNodeState :: Handler GetNodeStateResponse
  getNodeState = do
    timestamp <- liftIO $ round `fmap` getPOSIXTime
    infoE <- liftIO $ withClient nodeBackend getNodeInfo

    nameQuery <- liftIO $ tryIOError (Text.pack <$> getEnv "NODE_NAME")
    let name = case nameQuery of
                 Right x -> x
                 _ -> "unknown"

    versionQuery <- liftIO $ withClient nodeBackend getPeerVersion
    let version = case versionQuery of
                  Right x -> x
                  _ -> "unknown"

    uptimeQuery <- liftIO $ withClient nodeBackend getPeerUptime
    let runningSince = case uptimeQuery of
                       Right x -> fromIntegral x
                       _ -> 0


    sentQuery <- liftIO $ withClient nodeBackend getPeerTotalSent
    let sent = case sentQuery of
               Right x -> fromIntegral x
               _ -> 0

    receivedQuery <- liftIO $ withClient nodeBackend getPeerTotalReceived
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
        error $ show err

  setNodeState :: SetNodeStateRequest -> Handler SetNodeStateResponse
  setNodeState _ =
    pure $
      SetNodeStateResponse
        { success = True }

  replayTransactions :: ReplayTransactionsRequest -> Handler ReplayTransactionsResponse
  replayTransactions req =
    if adminToken req == adminAuthToken then do
      transactions <- liftIO $ getTransactionsForReplay esUrl

      let nid = 1000
      forM_ transactions (withClient nodeBackend . flip sendTransactionToBaker nid)

      return $ ReplayTransactionsResponse True
    else
      return $ ReplayTransactionsResponse False

-- For beta, uses the middlewareGodAccount which is seeded with funds
runGodTransaction :: Backend -> Text -> TransactionJSONPayload -> IO Types.TransactionHash
runGodTransaction nodeBackend esUrl payload =
  runTransaction nodeBackend esUrl payload middlewareGodAccount


runTransaction :: Backend -> Text -> TransactionJSONPayload -> Account -> IO Types.TransactionHash
runTransaction nodeBackend esUrl payload (address, keyMap) = do

  let accountAddressText = Text.pack $ show address

  nonce <- EsApi.takeNextNonceFor esUrl accountAddressText

  currentTime <- liftIO $ round `fmap` getPOSIXTime

  let
    transactionExpiry = currentTime + (60*30) -- Expires in 30 minutes from now

    -- These are the agreed fixed costs for testnet, they
    -- will change when tokenomics is finalized
    energyAmount =
      case payload of
        Transfer _ _       -> 10
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

  executeTransaction esUrl nodeBackend transaction address


type Account = (IDTypes.AccountAddress, Types.KeyMap)


executeTransaction :: Text -> Backend -> TransactionJSON -> Types.AccountAddress -> IO Types.TransactionHash
executeTransaction esUrl nodeBackend transaction address = do

  mdata <- loadContextData
  -- The networkId is for running multiple networks that's not the same chain, but hasn't been taken into use yet
  let nid = 1000

  t <- do
    let hookIt = True
    PR.evalContext mdata $ withClient nodeBackend $ processTransaction_ transaction nid hookIt True

  putStrLn $ "✅ Transaction sent to the baker and hooked: " ++ show (Types.transactionHash t)
  print transaction

  EsApi.logBareTransaction esUrl t address

  putStrLn "✅ Bare tansaction logged into ElasticSearch"

  pure $ Types.transactionHash t

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

  nodeUrl <- Config.lookupEnvText "NODE_URL" "localhost:11100"
  esUrl <- Config.lookupEnvText "ES_URL" "http://localhost:9200"
  idUrl <- Config.lookupEnvText "SIMPLEID_URL" "http://localhost:8000"

  putStrLn $ "✅ nodeUrl = " ++ Text.unpack nodeUrl
  putStrLn $ "✅ esUrl = " ++ Text.unpack esUrl
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

    nodeBackend = COM.GRPC { host = nodeHost, port = nodePort, target = Nothing }

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
          { ipIdentity = ipIdentity (idObjectResponse :: BetaIdProvisionResponse)
          , identityObject = identityObject (idObjectResponse :: BetaIdProvisionResponse)
          , idUseData = idUseData (idObjectResponse :: BetaIdProvisionResponse)
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

  _ <- runGodTransaction nodeBackend esUrl $ DeployCredential { credential = credential (idCredentialResponse :: IdCredentialResponse) }

  putStrLn $ "✅ Requesting GTU Drop for: " ++ show newAddress

  _ <- runGodTransaction nodeBackend esUrl $ Transfer { toaddress = newAddress, amount = 100 }

  pure "Done."


debugGrpc :: IO GetNodeStateResponse
debugGrpc = do
  let nodeBackend = COM.GRPC { host = "localhost", port = 11103, target = Nothing }

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
