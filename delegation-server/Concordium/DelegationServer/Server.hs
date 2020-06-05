module Concordium.DelegationServer.Server (module Concordium.DelegationServer.Server, addHeaders) where

import Concordium.Client.Cli
import Concordium.Client.Config (AccountConfig (..), getDefaultBaseConfigDir)
import Concordium.Client.GRPC
import qualified Concordium.DelegationServer.Api.Definition as Api
import qualified Concordium.DelegationServer.Api.Implementation as Api
import qualified Concordium.DelegationServer.Config as Config
import Concordium.DelegationServer.Logic
import Concordium.Types
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.STM.TMVar
import Control.Monad.Except
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Network.Wai (Application, Middleware)
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import qualified Network.Wai.Middleware.ForceSSL as M (forceSSL)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Network.Wai.Middleware.Static (Policy, policy, staticPolicy)
import Servant
import System.FilePath ((</>))
import Text.Read (readMaybe)

type Middlewares = (Network.Wai.Application -> Network.Wai.Application)

accountParser :: Value -> Parser AccountConfig
accountParser = withObject "Account keys" $ \v -> do
  accountAddr <- v .: "address"
  accountData <- v .: "accountData"
  keyMap <- accountData .: "keys"
  return $
    AccountConfig
      (NamedAddress Nothing accountAddr)
      keyMap
      (fromIntegral $ HM.size keyMap)

{- Fork a thread and boot the http server as a Wai app on Warp -}
runHttp :: Middlewares -> IO ()
runHttp middlewares = do
  -- Get environment variables
  serverPort <- Config.lookupEnv "PORT" 8081
  env <- Config.lookupEnv "ENV" Config.Development
  nodeUrl <- Config.lookupEnvText "NODE_URL" "localhost:11100"
  cfgDir <- T.unpack <$> (Config.lookupEnvText "ACC_DIR" . T.pack =<< getDefaultBaseConfigDir)
  let (nodeHost, nodePort) =
        case splitOn ":" (T.unpack nodeUrl) of
          nodeHost_ : nodePortText : _ -> case readMaybe nodePortText of
            Just nodePort_ ->
              (nodeHost_, nodePort_)
            Nothing ->
              error $ "Could not parse port for given NODE_URL: " ++ nodePortText
          _ ->
            error $ "Could not parse host:port for given NODE_URL: " ++ T.unpack nodeUrl
      grpcConfig = GrpcConfig {host = nodeHost, port = nodePort, target = Nothing, retryNum = 5, timeout = Nothing}
  runExceptT (mkGrpcClient grpcConfig Nothing) >>= \case
    Left err -> fail (show err) -- cannot connect to grpc server
    Right grpc -> do
      transitionChan <- newChan
      -- prepare servant app
      let app :: State -> Application
          app s = serve Api.api $ hoistServer Api.api (`runReaderT` s) $ Api.server transitionChan
      -- create the state
      tmv <- atomically $ newTMVar defaultLocalState
      outertmv <- atomically $ newTMVar defaultLocalState
      jStatus <- either (fail . show) (either (fail . show) return) =<< runClient grpc getConsensusStatus
      case fromJSON jStatus of
        Success status -> do
          delegationAccounts <-
            Vec.fromList
              <$> mapM
                ( \delegator -> do
                    keyfile <- LBS.readFile (cfgDir </> ("delegate-account-" ++ show delegator ++ ".json"))
                    let eacc = eitherDecode' keyfile >>= parseEither accountParser
                    case eacc of
                      Left err -> fail err
                      Right acc -> return acc
                )
                ([1 .. 4] :: [Int])
          let state = State tmv outertmv grpc delegationAccounts (durationToNominalDiffTime $ Duration (csrEpochDuration status)) (csrGenesisTime status)
          -- prepare wai app
          let waiApp = app state
              printStatus = do
                putStrLn $ "NODE_URL: " ++ show nodeUrl
                putStrLn $ "Server started: http://localhost:" ++ show serverPort
                putStrLn $ "Config directory: " ++ cfgDir
              run =
                W.defaultSettings
                  & W.setBeforeMainLoop printStatus
                  & W.setPort serverPort
                  & W.runSettings
          -- fork the wai app and the state machine
          _ <- forkIO $ run $ Config.logger env . middlewares $ waiApp
          _ <- forkIO $ forkStateMachine state transitionChan
          pure ()
        Error e -> fail e

-- Middlewares
--
-- The following functions can be composed together and wrapped around a Wai app to
-- add specific middleware behavior. For example, a common full stack might look like;
--
--    middlewares = compression . staticFiles "public" . allowCsrf . corsified
--    runApp      = run port $ middlewares httpApp

defaultMiddlewares :: Network.Wai.Application -> Network.Wai.Application
defaultMiddlewares = compression . staticFiles "public" . allowCsrf . corsified

-- | Basic HTTP Auth
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
auth :: ByteString -> ByteString -> Middleware
auth username password = basicAuth (\u p -> pure $ u == username && p == password) "Authentication"

-- | @x-csrf-token@ allowance.
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
allowCsrf :: Middleware
allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]

-- | CORS middleware configured with 'appCorsResourcePolicy'.
corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

-- | Adds static files directory, i.e. `staticFiles "public"` to serve from public folder
-- | Uses `index.html` as the index file on directory listing, i.e. `public/`
staticFiles :: String -> Middleware
staticFiles path = staticPolicy $ addBaseWithIndex path "index.html"

-- | Middleware to route static files and look for a default on index
addBaseWithIndex :: String -> String -> Policy
addBaseWithIndex base fallback =
  policy
    ( \req -> case req of
        "" -> Just (base </> fallback)
        _ -> Just (base </> req)
    )

compression :: Middleware
compression = gzip def

forceSSL :: Middleware
forceSSL = M.forceSSL

-- | Cors resource policy to be used with 'corsified' middleware.
--
-- This policy will set the following:
--
-- * RequestHeaders: @Content-Type, Authorization, Origin@
-- * MethodsAllowed: @OPTIONS, GET, PUT, POST@
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing,
      corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"],
      corsRequestHeaders = ["Authorization", "Content-Type", "Origin"],
      corsExposedHeaders = Nothing,
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }
-- LocalWords:  withObject
