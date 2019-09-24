{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Api where

import Network.Wai                   (Application)
import Control.Monad.Managed         (liftIO)
import Data.Aeson                    (encode, decode')
import Data.Aeson.Types              (ToJSON, FromJSON)
import Data.Text                     (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.List.Split
import Data.Map
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import System.Directory
import System.Exit
import System.Process
import System.Random
import Text.Read (readMaybe)

import           Concordium.Client.Runner
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction
import           Concordium.Client.Commands          as COM
import qualified Acorn.Parser.Runner                 as PR
import qualified Concordium.Scheduler.Types          as Types
import           Concordium.Crypto.SignatureScheme   (SchemeId (..), VerifyKey, KeyPair(..))
import           Concordium.Crypto.Ed25519Signature  (randomKeyPair)
import qualified Concordium.ID.Account
import qualified Concordium.ID.Types
import qualified Concordium.Scheduler.Utils.Init.Example

import qualified Config
import SimpleIdClientMock
import SimpleIdClientApi
import EsApi


data Routes r = Routes
    { sendTransaction :: r :-
        "v1" :> "sendTransaction" :> ReqBody '[JSON] TransactionJSON
                                  :> Post '[JSON] Text

    , typecheckContract :: r :-
        "v1" :> "typecheckContract" :> ReqBody '[JSON] Text
                                    :> Post '[JSON] Text

    , betaIdProvision :: r :-
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

    , identityGenerateChi :: r :-
        "v1" :> "identityGenerateChi" :> ReqBody '[JSON] Text
                                      :> Post '[JSON] Text

    , identityCreateAciPio :: r :-
        "v1" :> "identityCreateAciPio" :> ReqBody '[JSON] CreateAciPioRequest
                                       :> Post '[JSON] CreateAciPioResponse

    , identitySignPio :: r :-
        "v1" :> "identitySignPio" :> ReqBody '[JSON] SignPioRequest
                                  :> Post '[JSON] Text

    }
  deriving (Generic)


data BetaIdProvisionRequest =
  BetaIdProvisionRequest
    { attributes :: [(Text,Text)]
    , accountKeys :: Maybe AccountKeyPair
    }
  deriving (FromJSON, Generic, Show)

-- The BetaIdProvisionResponse is just what the SimpleIdClient returns for Identity Object provisioning
type BetaIdProvisionResponse = IdObjectResponse


data BetaAccountProvisionRequest =
  BetaAccountProvisionRequest
    { ipIdentity :: Int
    , preIdentityObject :: PreIdentityObject
    , privateData :: PrivateData
    , signature :: Text
    , revealedItems :: [Text]
    }
  deriving (ToJSON, FromJSON, Generic, Show)


data BetaAccountProvisionResponse =
  BetaAccountProvisionResponse
    { accountKeys :: AccountKeyPair
    , spio :: IdCredential
    , address :: Text
    }
  deriving (ToJSON, Generic, Show)


data BetaGtuDropResponse =
  BetaGtuDropResponse
    { transactionId :: Types.TransactionHash
    }
  deriving (ToJSON, Generic, Show)


-- Legacy @TODO remove?

data CreateAciPioRequest =
  CreateAciPioRequest
    { scheme :: IdAttributesScheme
    , chi :: Text
    }
  deriving (FromJSON, Generic, Show)


data CreateAciPioResponse =
  CreateAciPioResponse
    { aci :: Text
    , pio :: Text
    }
  deriving (ToJSON, Generic, Show)


data SignPioRequest =
  SignPioRequest
    { pio :: Text
    , identityProviderId :: Text
    }
  deriving (FromJSON, Generic, Show)


api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)


servantApp :: COM.Backend -> Text -> Text -> Application
servantApp nodeBackend esUrl idUrl = genericServe routesAsServer
 where
  routesAsServer = Routes {..} :: Routes AsServer

  sendTransaction :: TransactionJSON -> Handler Text
  sendTransaction transaction = do
    liftIO $ do
      mdata <- loadContextData

      -- The networkId is for running multiple networks that's not the same chain, but hasn't been taken into use yet
      let nid = 1000

      t <- do
        let hookIt = False
        PR.evalContext mdata $ runInClient nodeBackend $ processTransaction_ transaction nid hookIt

      created <- getCurrentTime

      putStrLn $ "Transaction sent to the baker: " ++ show (bareTransactionHash t created)

    -- @TODO What response should we send?
    pure "Submitted"


  typecheckContract :: Text -> Handler Text
  typecheckContract contractCode = do
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
        ExitSuccess -> do
          pure "ok"
        ExitFailure code -> do
          if code == 1 then
              pure $ Text.pack stderr
          else
              pure "unknownerr"


  betaIdProvision :: BetaIdProvisionRequest -> Handler BetaIdProvisionResponse
  betaIdProvision BetaIdProvisionRequest{ attributes, accountKeys } = do

    liftIO $ putStrLn "✅ Got the following attributes:"
    liftIO $ putStrLn $ show attributes

    creationTime <- liftIO $ round `fmap` getPOSIXTime

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

    -- liftIO $ putStrLn $ show attributes

    let newAccountKeyPair = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
        newVerifyKey = verifyKey (newAccountKeyPair :: AccountKeyPair)
        -- @TODO fix me with proper ToJSON not show which includes "AddressAccount" label
        -- Fix the tempDecodeAddress in wallet frontend at the same time
        newAccountAddress = verifyKeyToAccountAddress newVerifyKey

    _ <- liftIO $ godTransaction nodeBackend esUrl $ DeployCredential { credential = certainDecode $ encode $ credential (idCredentialResponse :: IdCredentialResponse) }

    pure $
      BetaAccountProvisionResponse
        { accountKeys = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
        , spio = credential (idCredentialResponse :: IdCredentialResponse)
        , address = Text.pack . show $ newAccountAddress
        }


  betaGtuDrop :: Types.Address -> Handler BetaGtuDropResponse
  betaGtuDrop toAddress = do

    liftIO $ putStrLn $ "✅ Requesting GTU Drop for " ++ show toAddress

    transactionId <- liftIO $ godTransaction nodeBackend esUrl $ Transfer { toaddress = toAddress, amount = 100 }

    pure $ BetaGtuDropResponse { transactionId = transactionId }


  accountTransactions :: Text -> Handler AccountTransactionsResponse
  accountTransactions address =
    liftIO $ EsApi.getAccountTransactions esUrl address


  identityGenerateChi :: Text -> Handler Text
  identityGenerateChi name = do
    liftIO $ SimpleIdClientMock.createChi name


  identityCreateAciPio :: CreateAciPioRequest -> Handler CreateAciPioResponse
  identityCreateAciPio CreateAciPioRequest{ scheme, chi } = do
    (aci, pio) <- liftIO $ SimpleIdClientMock.createAciPio scheme chi
    pure $ CreateAciPioResponse { aci = aci, pio = pio }


  identitySignPio :: SignPioRequest -> Handler Text
  identitySignPio SignPioRequest{ pio, identityProviderId } = do
    liftIO $ SimpleIdClientMock.signPio pio identityProviderId


-- For beta, uses the mateuszKP which is seeded with funds
godTransaction :: Backend -> Text -> TransactionJSONPayload -> IO Types.TransactionHash
godTransaction nodeBackend esUrl payload = do

  let mateuszAccountText = Text.pack $ show Concordium.Scheduler.Utils.Init.Example.mateuszAccount

  nonce <- EsApi.takeNextNonceFor esUrl mateuszAccountText

  let
    transaction =
      TransactionJSON
        { metadata = transactionHeader
        , payload = payload
        , signKey = Concordium.Crypto.SignatureScheme.signKey mateuszKP
        }

    transactionHeader =
          TransactionJSONHeader
            { thSenderKey = Concordium.Crypto.SignatureScheme.verifyKey mateuszKP
            , thNonce = Just nonce
            , thGasAmount = 100000
            }

  executeTransaction nodeBackend transaction


mateuszKP :: KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))


executeTransaction :: Backend -> TransactionJSON -> IO Types.TransactionHash
executeTransaction nodeBackend transaction = do

  mdata <- loadContextData
  -- The networkId is for running multiple networks that's not the same chain, but hasn't been taken into use yet
  let nid = 1000

  t <- do
    let hookIt = True
    PR.evalContext mdata $ runInClient nodeBackend $ processTransaction_ transaction nid hookIt

  created <- getCurrentTime

  putStrLn $ "✅ Transaction sent to the baker and hooked: " ++ show (bareTransactionHash t created)
  putStrLn $ show transaction

  pure $ bareTransactionHash t created


verifyKeyToAddress :: VerifyKey -> Types.Address
verifyKeyToAddress verifyKey =
  Types.AddressAccount $ verifyKeyToAccountAddress verifyKey


verifyKeyToAccountAddress :: VerifyKey -> Concordium.ID.Types.AccountAddress
verifyKeyToAccountAddress verifyKey =
  Concordium.ID.Account.accountAddress verifyKey Ed25519


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
        nodeHost:nodePortText:_ -> case readMaybe nodePortText of
          Just nodePort ->
            (nodeHost, nodePort)
          Nothing ->
            error $ "Could not parse port for given NODE_URL: " ++ nodePortText
        _ ->
          error $ "Could not parse host:port for given NODE_URL: " ++ Text.unpack nodeUrl

    nodeBackend = COM.GRPC { host = nodeHost, port = nodePort, target = Nothing }

  creationTime <- liftIO $ round `fmap` getPOSIXTime

  let attributesStub =
        [ ("birthYear", "2000")
        , ("residenceCountryCode", "184") -- (GB) Great Britain
        ]

      expiryDate = creationTime + (60*60*24*365) -- Expires in 365 days from now

      idObjectRequest =
        IdObjectRequest
          { ipIdentity = 0
          , name = "middleware-beta-debug"
          , attributes = fromList -- @TODO make these a dynamic in future
              ([ ("creationTime", Text.pack $ show creationTime)
              , ("expiryDate", Text.pack $ show expiryDate)
              , ("maxAccount", "30")
              , ("variant", "0")
              ] ++ attributesStub)
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
      newVerifyKey = verifyKey (newAccountKeyPair :: AccountKeyPair)
      newAddress = verifyKeyToAddress newVerifyKey

      betaAccountProvisionResponse =
        BetaAccountProvisionResponse
          { accountKeys = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
          , spio = credential (idCredentialResponse :: IdCredentialResponse)
          , address = Text.pack . show $ newAddress
          }

  putStrLn $ "✅ Deploying credentials for: " ++ show newAddress

  _ <- godTransaction nodeBackend esUrl $ DeployCredential { credential = certainDecode $ encode $ credential (idCredentialResponse :: IdCredentialResponse) }

  putStrLn $ "✅ Requesting GTU Drop for: " ++ show newAddress

  _ <- godTransaction nodeBackend esUrl $ Transfer { toaddress = newAddress, amount = 100 }

  pure "Done."


bareTransactionHash :: Types.BareTransaction -> UTCTime -> Types.TransactionHash
bareTransactionHash bare currentTime =
  Types.trHash $ Types.fromBareTransaction currentTime bare
