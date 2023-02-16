module Server (module Server, addHeaders) where

import           Control.Concurrent (forkIO)
import           Control.Monad.Except
import           Data.Function ((&))
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.Wai (Application, Middleware)
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.AddHeaders (addHeaders)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors)
import           Network.Wai.Logger
import           Text.Read (readMaybe)
import qualified Config
import           Concordium.Client.GRPC2
import qualified Api


type Middlewares = (Network.Wai.Application -> Network.Wai.Application)

{- Fork a thread and boot the http server as a Wai app on Warp -}
runHttp :: Middlewares -> IO ()
runHttp middlewares = do

  serverPort <- Config.lookupEnv "PORT" 8081

  nodeUrl <- Config.lookupEnvText "NODE_URL" "localhost:11100"
  grpcAdminToken <- Config.lookupEnvText "RPC_PASSWORD" "rpcadmin"
  to <- Config.lookupEnv "MIDDLEWARE_REQUEST_TIMEOUT" 15
  secure <- Config.lookupEnvTextWithoutDefault "MIDDLEWARE_USE_TLS"

  let
    (nodeHost, nodePort) =
      case splitOn ":" (T.unpack nodeUrl) of
        nodeHost_:nodePortText:_ -> case readMaybe nodePortText of
          Just nodePort_ ->
            (nodeHost_, nodePort_)
          Nothing ->
            error $ "Could not parse port for given NODE_URL: " ++ nodePortText
        _ ->
          error $ "Could not parse host:port for given NODE_URL: " ++ T.unpack nodeUrl

    grpcConfig = GrpcConfig { host = nodeHost, port = nodePort, grpcAuthenticationToken = T.unpack grpcAdminToken, target = Nothing, retryNum = 5, timeout = Just to, useTls = isJust secure }

  runExceptT (mkGrpcClient grpcConfig (Just T.putStrLn)) >>= \case
    Left err -> fail (show err) -- cannot connect to grpc server
    Right nodeBackend -> do
      let
        waiApp = Api.servantApp nodeBackend

        printStatus = do
          putStrLn $ "NODE_URL: " ++ show nodeUrl
          putStrLn $ "gRPC authentication token: " ++ show grpcAdminToken
          putStrLn $ "Server started: http://localhost:" ++ show serverPort
          when (isJust secure) $ putStrLn "Using TLS to connect to the node."

        run l = W.defaultSettings
                  & W.setBeforeMainLoop printStatus
                  & W.setPort serverPort
                  & W.setLogger l
                  & W.runSettings

      _ <- forkIO $ withStdoutLogger $ \lg -> run lg $ middlewares waiApp

      pure ()

-- Middlewares
--
-- The following functions can be composed together and wrapped around a Wai app to
-- add specific middleware behavior. For example, a common full stack might look like;
--
--    middlewares = compression . staticFiles "public" . allowCsrf . corsified
--    runApp      = run port $ middlewares httpApp

-- | @x-csrf-token@ allowance.
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
allowCsrf :: Middleware
allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]

-- | CORS middleware configured with 'appCorsResourcePolicy'.
corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

-- | Cors resource policy to be used with 'corsified' middleware.
--
-- This policy will set the following:
--
-- * RequestHeaders: @Content-Type, Authorization, Origin@
-- * MethodsAllowed: @OPTIONS, GET, PUT, POST@
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
  , corsRequestHeaders = ["Authorization", "Content-Type", "Origin"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }
