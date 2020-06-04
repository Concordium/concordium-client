module Server (module Server, addHeaders) where

import           Control.Concurrent (forkIO)
import           Control.Monad.Except
import           Data.ByteString (ByteString)
import           Data.Function ((&))
import           Data.List.Split
import qualified Data.Text as T
import           Network.Wai (Application, Middleware)
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.AddHeaders (addHeaders)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors)
import qualified Network.Wai.Middleware.ForceSSL as M (forceSSL)
import           Network.Wai.Middleware.Gzip (gzip, def)
import           Network.Wai.Middleware.HttpAuth (basicAuth)
import           Network.Wai.Middleware.Static (staticPolicy, policy, Policy)
import           System.FilePath ((</>))
import           Text.Read (readMaybe)

import qualified Config
import           Concordium.Client.GRPC
import           Concordium.Client.Config (getDefaultDataDir, getDefaultBaseConfigDir)
import qualified Api


type Middlewares = (Network.Wai.Application -> Network.Wai.Application)

{- Fork a thread and boot the http server as a Wai app on Warp -}
runHttp :: Middlewares -> IO ()
runHttp middlewares = do

  serverPort <- Config.lookupEnv "PORT" 8081
  env  <- Config.lookupEnv "ENV" Config.Development

  nodeUrl <- Config.lookupEnvText "NODE_URL" "localhost:11100"
  pgUrl <- Config.lookupEnvText "PG_URL" "host=localhost port=5432 user=concordium dbname=concordium password=concordium"
  idUrl <- Config.lookupEnvText "SIMPLEID_URL" "http://localhost:8000"

  cfgDir <- T.unpack <$> (Config.lookupEnvText "CFG_DIR" . T.pack =<< getDefaultBaseConfigDir)
  dataDir <- T.unpack <$> (Config.lookupEnvText "DATA_DIR" . T.pack =<< getDefaultDataDir)

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

    grpcConfig = GrpcConfig { host = nodeHost, port = nodePort, target = Nothing, retryNum = 5, timeout = Nothing }

  runExceptT (mkGrpcClient grpcConfig Nothing) >>= \case
    Left err -> fail (show err) -- cannot connect to grpc server
    Right nodeBackend -> do
      let
        waiApp = Api.servantApp nodeBackend pgUrl idUrl cfgDir dataDir

        printStatus = do
          putStrLn $ "NODE_URL: " ++ show nodeUrl
          putStrLn $ "PG_URL: " ++ show pgUrl
          putStrLn $ "SIMPLEID_URL: " ++ show idUrl
          putStrLn $ "Environment: " ++ show env
          putStrLn $ "Server started: http://localhost:" ++ show serverPort
          putStrLn $ "Config directory: " ++ cfgDir
          putStrLn $ "Data directory: " ++ dataDir

        run = W.defaultSettings
                  & W.setBeforeMainLoop printStatus
                  & W.setPort serverPort
                  & W.runSettings

      _ <- forkIO $ run $ Config.logger env . middlewares $ waiApp

      pure ()


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
addBaseWithIndex base fallback = policy
  ( \req -> case req of
    "" -> Just (base </> fallback)
    _  -> Just (base </> req)
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
