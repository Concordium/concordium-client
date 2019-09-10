{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api where

import Network.Wai                   (Application)
import Control.Monad.Managed         (liftIO)
import Data.Aeson.Types              (ToJSON, FromJSON)
import Data.Text                     (Text)
import Data.Maybe                    (fromMaybe)
import Data.Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.Process
import System.Exit
import Servant
import Servant.API.Generic
import Servant.Server.Generic

import           Concordium.Client.Runner
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction
import           Concordium.Client.Commands          as COM
import qualified Acorn.Parser.Runner                 as PR
import qualified Concordium.Scheduler.Types          as Types
import Concordium.Crypto.SignatureScheme (KeyPair)
import SimpleIdClientMock
import SimpleIdClientApi

data Routes r = Routes
    { sendTransaction :: r :-
        "v1" :> "sendTransaction" :> ReqBody '[JSON] TransactionJSON
                                  :> Post '[JSON] Text

    , typecheckContract :: r :-
        "v1" :> "typecheckContract" :> ReqBody '[JSON] Text
                                    :> Post '[JSON] Text

    , betaComboProvision :: r :-
        "v1" :> "betaComboProvision" :> ReqBody '[JSON] ComboProvisionRequest
                                     :> Post '[JSON] ComboProvisionResponse

    , accountTransactions :: r :-
        "v1" :> "accountTransactions" :> ReqBody '[JSON] Text
                                     :> Post '[JSON] [AccountTransaction]

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


data ComboProvisionRequest =
  ComboProvisionRequest
    { scheme :: Text
    , attributes :: [(Text,Text)]
    , accountKeys :: Maybe Text -- @TODO fix type
    }
  deriving (FromJSON, Generic, Show)


data ComboProvisionResponse =
  ComboProvisionResponse
    { accountKeys :: AccountKeyPair
    , spio :: IdCredential
    }
  deriving (ToJSON, Generic, Show)


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


servantApp :: COM.Backend -> Application
servantApp backend = genericServe routesAsServer
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
        PR.evalContext mdata $ runInClient backend $ processTransaction_ transaction nid hookIt

      putStrLn $ "Transaction sent to the baker: " ++ show (Types.trHash t)

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
              pure $ T.pack stderr
          else
              pure "unknownerr"


  betaComboProvision :: ComboProvisionRequest -> Handler ComboProvisionResponse
  betaComboProvision ComboProvisionRequest{ scheme, attributes, accountKeys } = do

    let attributesStub = -- @TODO inject attribute list components
          [ ("birthYear", "2013")
          , ("residenceCountryCode", "386")
          ]

        idObjectRequest =
          IdObjectRequest
            { ipIdentity = 0
            , name = "Ales" -- @TODO inject name
            , attributes = fromList -- @TODO make these a dynamic in future
                ([ ("creationTime", "1341324324")
                , ("expiryDate", "1910822399")
                , ("maxAccount", "30")
                , ("variant", "0")
                ] ++ attributesStub)
            }

    idObjectRespose <- liftIO $ postIdObjectRequest idObjectRequest

    liftIO $ putStrLn "✅ Got IdObjectResponse"

    let credentialRequest =
          IdCredentialRequest
            { ipIdentity = ipIdentity (idObjectRespose :: IdObjectResponse)
            , preIdentityObject = preIdentityObject (idObjectRespose :: IdObjectResponse)
            , privateData = privateData (idObjectRespose :: IdObjectResponse)
            , signature = signature (idObjectRespose :: IdObjectResponse)
            , revealedItems = ["birthYear"] -- @TODO take revealed items preferences from user
            , accountNumber = 0
            }

    idCredentialResponse <- liftIO $ postIdCredentialRequest credentialRequest

    liftIO $ putStrLn "✅ Got idCredentialResponse"

    -- putStrLn $ BS.unpack $ encode idCredentialResponse
    -- (aci, pio) <- liftIO $ SimpleIdClientMock.createAciPio SimpleIdClientMock.V2 "x"
    -- spio <- liftIO $ SimpleIdClientMock.signPio "x" "x"

    -- liftIO $ putStrLn $ show attributes

    -- @TODO need to fetch finalizedPointer from GetConsensusStatus and inject it into deploy command
    pure $
      ComboProvisionResponse
        { accountKeys = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
        , spio = credential (idCredentialResponse :: IdCredentialResponse)
        }

        -- { accountKeys = fromMaybe "@TODO generate new account keys here" accountKeys
        -- , aci = aci
        -- , spio = spio
        -- }

  accountTransactions :: Text -> Handler [AccountTransaction]
  accountTransactions address =
    pure $ SimpleIdClientMock.accountTransactions address


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
