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
import Data.Aeson                    (encode, decode')
import Data.Aeson.Types              (ToJSON, FromJSON)
import Data.Text                     (Text)
import Data.Maybe                    (fromMaybe)
import Data.Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Base16              as BS16
import qualified Data.ByteString.Short               as BSS
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
import           Concordium.Crypto.SignatureScheme   ( SchemeId (..)
                                                     , SignKey (..)
                                                     , VerifyKey (..)
                                                     )
import qualified Concordium.ID.Account
import SimpleIdClientMock
import SimpleIdClientApi


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


data BetaIdProvisionRequest =
  BetaIdProvisionRequest
    { scheme :: Text
    , attributes :: [(Text,Text)]
    , accountKeys :: Maybe Text -- @TODO fix type
    }
  deriving (FromJSON, Generic, Show)

-- The BetaIdProvisionResponse is just what the SimpleIdClient returns for Identity Object provisioning
type BetaIdProvisionResponse = IdObjectResponse


-- The BetaAccountProvisionRequest is just what the SimpleIdClient expects for Identity Credential provisioning
-- @TODO but will shortly expand to inclkude relvealedAttributes and accountNumber fields
type BetaAccountProvisionRequest = IdCredentialRequest

data BetaAccountProvisionResponse =
  BetaAccountProvisionResponse
    { accountKeys :: AccountKeyPair
    , spio :: IdCredential
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
              pure $ Text.pack stderr
          else
              pure "unknownerr"


  betaIdProvision :: BetaIdProvisionRequest -> Handler BetaIdProvisionResponse
  betaIdProvision BetaIdProvisionRequest{ scheme, attributes, accountKeys } = do

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

    idObjectResponse <- liftIO $ postIdObjectRequest idObjectRequest

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
            , revealedItems = ["birthYear"] -- @TODO take revealed items preferences from user
            , accountNumber = 0
            }

    idCredentialResponse <- liftIO $ postIdCredentialRequest credentialRequest

    liftIO $ putStrLn "✅ Got idCredentialResponse"

    -- liftIO $ putStrLn $ show attributes

    pure $
      BetaAccountProvisionResponse
        { accountKeys = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
        , spio = credential (idCredentialResponse :: IdCredentialResponse)
        }


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




{--

This chains together the full process that is split into seperate APIs above so
it can be more easily tested via `stack ghci` using `:r` for quick reloads.

Once proven together, relevant parts are integrated into the APIs above.

Currently, this seems to work all the way through to submitting onto the chain
and being accepted baked into a block.

There is however an issue with GetAccountInfo gRPC call on the new account:

https://gist.github.com/supermario/8e58460662e4d6cb769a7c82fda0723b

--}
debugTestFullProvision :: IO String
debugTestFullProvision = do

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

  idObjectResponse <- postIdObjectRequest idObjectRequest

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

  idCredentialResponse <- postIdCredentialRequest credentialRequest

  putStrLn "✅ Got idCredentialResponse"

  putStrLn "✅ Generating JSON for idCredentialResponse:"
  putStrLn $ BS.unpack $ encode idCredentialResponse

  let betaAccountProvisionResponse =
        BetaAccountProvisionResponse
          { accountKeys = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
          , spio = credential (idCredentialResponse :: IdCredentialResponse)
          }

  let transactionHeader =
        TransactionJSONHeader
          { thSenderKey = certainDecode "\"3e4f11b8a43f5b4c63f9d85ae9de365057c9bce8c57caf84e34f1040e5f59ecd\"" -- verifyKey @TODO figure out how to create this value directly
          , thNonce = Just 1
          , thGasAmount = 4000
          , thFinalizedPointer = certainDecode "\"170afb6659a6d4a375524124606177d5704629c55335edeac28b3cfabc0ff11a\"" -- @NOTE unused, will be removed
          }

      deployTransaction =
        TransactionJSON
          { metadata = transactionHeader
          , payload =
              DeployCredential
                { credential = certainDecode $ encode $ credential (idCredentialResponse :: IdCredentialResponse) }
          -- @TODO is this the right way to make this value...? Taken from the FromJSON instance. If so make a helper
          , signKey = SignKey . BSS.toShort . fst . BS16.decode . Text.encodeUtf8 $ "b52f4ce89e78e45934851e395c6258f7240ce6902526c78a8960927c8959a363"
          }

  putStrLn "✅ Generating JSON for deployTransaction:"
  putStrLn $ show deployTransaction

  let newAccountKeyPair = accountKeyPair (idCredentialResponse :: IdCredentialResponse)
      -- @TODO is this the right way to make this value...? Copied from signKey above. If so make a helper
      newVerifyKey = VerifyKey . BSS.toShort . fst . BS16.decode . Text.encodeUtf8 $ verifyKey (newAccountKeyPair :: AccountKeyPair)
      newAccountAddress = Concordium.ID.Account.accountAddress newVerifyKey Ed25519

  putStrLn $ "✅ Deployed account address will be: " ++ show newAccountAddress

  mdata <- loadContextData
  -- The networkId is for running multiple networks that's not the same chain, but hasn't been taken into use yet
  let nid = 1000
  let backend = COM.GRPC { host = "127.0.0.1", port = 11160, target = Nothing }

  t <- do
    let hookIt = True
    PR.evalContext mdata $ runInClient backend $ processTransaction_ deployTransaction nid hookIt

  putStrLn $ "✅ Transaction sent to the baker and hooked: " ++ show (Types.trHash t)

  pure "Done."


certainDecode value =
  case decode' value of
    Just v -> v
    Nothing -> error $ "Well... not so certain now huh! certainDecode failed on:\n\n" ++ show value
