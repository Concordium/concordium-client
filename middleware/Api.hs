{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Api where

import Network.Wai                   (Application)
import Control.Monad.Managed         (liftIO)
import Data.Aeson                    (encode, decode')
import Data.Aeson.Types              (FromJSON)
import Data.Text                     (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.List.Split
import Data.Map
import Data.Time.Clock.POSIX
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import System.Directory
import System.Exit
import System.Environment
import System.IO.Error
import System.Process
import Text.Read (readMaybe)
import Lens.Simple

import           Concordium.Client.Runner
import           Concordium.Client.GRPC
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction
import           Concordium.Client.Commands          as COM
import qualified Acorn.Parser.Runner                 as PR
import qualified Concordium.Types                    as Types
import qualified Concordium.Types.Transactions       as Types
import           Concordium.Crypto.SignatureScheme   (KeyPair(..), correspondingVerifyKey)
import           Concordium.Crypto.Ed25519Signature  (deriveVerifyKey)
import qualified Concordium.ID.Account
import qualified Proto.Concordium_Fields             as CF
import Control.Monad

import qualified Config
import SimpleIdClientApi
import EsApi
import Api.Messages

godsToken :: Text
godsToken = "47434137412923191713117532"

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
            { ipIdentity = 0
            , name = "middleware-beta-autogen"
            , attributes = fromList
                ([ ("creationTime", Text.pack $ show creationTime)
                , ("expiryDate", Text.pack $ show expiryDate)
                , ("maxAccount", "30")
                , ("variant", "0")
                ] ++ attributes)
            , anonymityRevokers = [0,1,2]
            , threshold = 2
            }

    idObjectResponse <- liftIO $ postIdObjectRequest idUrl idObjectRequest

    liftIO $ putStrLn "✅ Got IdObjectResponse"

    pure idObjectResponse


  betaAccountProvision :: BetaAccountProvisionRequest -> Handler BetaAccountProvisionResponse
  betaAccountProvision accountProvisionRequest = do

    let credentialRequest =
          IdCredentialRequest
            { ipIdentity = ipIdentity (accountProvisionRequest :: BetaAccountProvisionRequest)
            , preIdentityObject = preIdentityObject (accountProvisionRequest :: BetaAccountProvisionRequest)
            , privateData = privateData (accountProvisionRequest :: BetaAccountProvisionRequest)
            , signature = signature (accountProvisionRequest :: BetaAccountProvisionRequest)
            , revealedItems = revealedItems (accountProvisionRequest :: BetaAccountProvisionRequest)
            , accountNumber = 0 -- @TODO auto increment account number from Elastic?
            }

    idCredentialResponse <- liftIO $ postIdCredentialRequest idUrl credentialRequest

    liftIO $ putStrLn "✅ Got idCredentialResponse"

    let newAccountKeyPair = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
        newAccountAddress = Concordium.ID.Account.accountAddress (correspondingVerifyKey newAccountKeyPair)

    _ <- liftIO $ runGodTransaction nodeBackend esUrl $ DeployCredential { credential = certainDecode $ encode $ credential (idCredentialResponse :: IdCredentialResponse) }

    pure $
      BetaAccountProvisionResponse
        { accountKeys = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
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

    let accountAddress = Concordium.ID.Account.accountAddress (correspondingVerifyKey keypair)

    liftIO $ putStrLn $ "✅ Sending " ++ show amount ++ " from " ++ show accountAddress ++ " to " ++ show to

    transactionId <- liftIO $ runTransaction nodeBackend esUrl (Transfer { toaddress = to, amount = amount }) keypair

    pure $ TransferResponse { transactionId = transactionId }


  getNodeState :: Handler GetNodeStateResponse
  getNodeState = do
    timestamp <- liftIO $ round `fmap` getPOSIXTime
    infoE <- liftIO $ runInClient nodeBackend getNodeInfo

    nameQuery <- liftIO $ tryIOError (Text.pack <$> getEnv "NODE_NAME")
    let name = case nameQuery of
                 Right x -> x
                 _ -> "unknown"

    versionQuery <- liftIO $ runInClient nodeBackend getPeerVersion
    let version = case versionQuery of
                  Right x -> x
                  _ -> "unknown"

    uptimeQuery <- liftIO $ runInClient nodeBackend getPeerUptime
    let runningSince = case uptimeQuery of
                       Right x -> fromIntegral x
                       _ -> 0


    sentQuery <- liftIO $ runInClient nodeBackend getPeerTotalSent
    let sent = case sentQuery of
               Right x -> fromIntegral x
               _ -> 0

    receivedQuery <- liftIO $ runInClient nodeBackend getPeerTotalReceived
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
    if adminToken req == godsToken then do
      transactions <- liftIO $ getTransactionsForReplay esUrl

      let nid = 1000
      forM_ transactions (runInClient nodeBackend . flip sendTransactionToBaker nid)

      return $ ReplayTransactionsResponse True
    else
      return $ ReplayTransactionsResponse False

-- For beta, uses the middlewareGodKP which is seeded with funds
runGodTransaction :: Backend -> Text -> TransactionJSONPayload -> IO Types.TransactionHash
runGodTransaction nodeBackend esUrl payload =
  runTransaction nodeBackend esUrl payload middlewareGodKP


runTransaction :: Backend -> Text -> TransactionJSONPayload -> KeyPair -> IO Types.TransactionHash
runTransaction nodeBackend esUrl payload keypair = do

  let accountAddressText = Text.pack . show . Concordium.ID.Account.accountAddress . correspondingVerifyKey $ keypair

  nonce <- EsApi.takeNextNonceFor esUrl accountAddressText

  let
    gasAmount =
      case payload of
        Transfer _ _       -> 10
        DeployCredential _ -> 10000
        _                  -> 10000

    transaction =
      TransactionJSON
        { metadata = transactionHeader
        , payload = payload
        , signKey = keypair
        }

    transactionHeader =
          TransactionJSONHeader
            { thSenderKey = correspondingVerifyKey keypair
            , thNonce = Just nonce
            , thGasAmount = gasAmount
            }

  executeTransaction esUrl nodeBackend transaction


middlewareGodKP :: KeyPair
middlewareGodKP = do
  -- https://gitlab.com/Concordium/p2p-client/blob/d41aba2cc3bfed7c5be21fe0612581f9c90e9e45/scripts/genesis-data/beta_accounts/beta-account-0.json
  let signKey = certainDecode "\"016fb793ef489eaf256eac1ebbe2513919b60dfd4fad9645f2425127af6e109f\""
  KeyPairEd25519 signKey (deriveVerifyKey signKey)


executeTransaction :: Text -> Backend -> TransactionJSON -> IO Types.TransactionHash
executeTransaction esUrl nodeBackend transaction = do

  mdata <- loadContextData
  -- The networkId is for running multiple networks that's not the same chain, but hasn't been taken into use yet
  let nid = 1000

  t <- do
    let hookIt = True
    PR.evalContext mdata $ runInClient nodeBackend $ processTransaction_ transaction nid hookIt

  putStrLn $ "✅ Transaction sent to the baker and hooked: " ++ show (Types.transactionHash t)
  print transaction


  EsApi.logBareTransaction esUrl t (Concordium.ID.Account.accountAddress $ thSenderKey $ metadata transaction)

  putStrLn "✅ Bare tansaction logged into ElasticSearch"

  pure $ Types.transactionHash t

-- Dirty helper to help us with "definitely certain" value decoding
certainDecode :: (FromJSON a) => BS8.ByteString -> a
certainDecode bytestring =
  case decode' bytestring of
    Just v -> v
    Nothing -> error $ "Well... not so certain now huh! certainDecode failed on:\n\n" ++ show bytestring


-- Dirty helper to help us with "definitely certain" account address decoding
certainAccountAddress :: Text -> Types.Address
certainAccountAddress addressText =
  case decode' $ BS8.fromStrict $ Text.encodeUtf8 addressText of
    Just address -> Types.AddressAccount address
    Nothing -> error $ "Well... not so certain now huh! certainAccountAddress failed on:\n\n" ++ show (Text.unpack addressText)


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
        [ ("birthYear", "2000")
        , ("residenceCountryCode", "184") -- (GB) Great Britain
        ]

      expiryDate = creationTime + (60*60*24*365) -- Expires in 365 days from now

      idObjectRequest =
        IdObjectRequest
          { ipIdentity = 5
          , name = "middleware-beta-debug"
          , attributes = fromList -- @TODO make these a dynamic in future
              ([ ("creationTime", Text.pack $ show creationTime)
              , ("expiryDate", Text.pack $ show expiryDate)
              , ("maxAccount", "30")
              , ("variant", "0")
              ] ++ attributesStub)
          , anonymityRevokers = [0,1,2]
          , threshold = 2
          }

  idObjectResponse <- postIdObjectRequest idUrl idObjectRequest

  putStrLn "✅ Got IdObjectResponse"

  let credentialRequest =
        IdCredentialRequest
          { ipIdentity = ipIdentity (idObjectResponse :: BetaIdProvisionResponse)
          , preIdentityObject = preIdentityObject (idObjectResponse :: BetaIdProvisionResponse)
          , privateData = privateData (idObjectResponse :: BetaIdProvisionResponse)
          , signature = signature (idObjectResponse :: BetaIdProvisionResponse)
          , revealedItems = ["birthYear"] -- @TODO take revealed items preferences from user
          , accountNumber = 0
          }

  idCredentialResponse <- postIdCredentialRequest idUrl credentialRequest

  putStrLn "✅ Got idCredentialResponse"

  putStrLn "✅ Generating JSON for idCredentialResponse:"
  putStrLn $ BS8.unpack $ encode idCredentialResponse

  let newAccountKeyPair = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
      newAddress = Types.AddressAccount $ Concordium.ID.Account.accountAddress (correspondingVerifyKey newAccountKeyPair)

  putStrLn $ "✅ Deploying credentials for: " ++ show newAddress

  _ <- runGodTransaction nodeBackend esUrl $ DeployCredential { credential = credential (idCredentialResponse :: IdCredentialResponse) }

  putStrLn $ "✅ Requesting GTU Drop for: " ++ show newAddress

  _ <- runGodTransaction nodeBackend esUrl $ Transfer { toaddress = newAddress, amount = 100 }

  pure "Done."

debugGrpc :: IO GetNodeStateResponse
debugGrpc = do
  let nodeBackend = COM.GRPC { host = "localhost", port = 11103, target = Nothing }

  infoE <- runInClient nodeBackend getNodeInfo

  nameQuery <- liftIO $ tryIOError (Text.pack <$> getEnv "NODE_NAME")
  let name = case nameQuery of
        Right x -> x
        _ -> "unknown"

  versionQuery <- runInClient nodeBackend getPeerVersion
  let version = case versionQuery of
                  Right x -> x
                  _ -> "unknown"

  uptimeQuery <- runInClient nodeBackend getPeerUptime
  let runningSince = case uptimeQuery of
                       Right x -> fromIntegral x
                       _ -> 0


  sentQuery <- runInClient nodeBackend getPeerTotalSent
  let sent = case sentQuery of
               Right x -> fromIntegral x
               _ -> 0

  receivedQuery <- runInClient nodeBackend getPeerTotalReceived
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
