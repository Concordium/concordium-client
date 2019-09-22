module Server (module Server, addHeaders) where

import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)
import Data.Function ((&))

import qualified Network.Wai.Handler.Warp as W
import System.FilePath                   ((</>))
import Network.Wai                       (Application, Middleware)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors       (CorsResourcePolicy(..), cors)
import Network.Wai.Middleware.HttpAuth   (basicAuth)
import Network.Wai.Middleware.Static     (staticPolicy, policy, Policy)
import Network.Wai.Middleware.Gzip       (gzip, def)
import qualified Network.Wai.Middleware.ForceSSL as M (forceSSL)


import qualified Config

type Middlewares = (Network.Wai.Application -> Network.Wai.Application)

{- Fork a thread and boot the http server as a Wai app on Warp -}
runHttp :: Network.Wai.Application -> Middlewares -> IO ()
runHttp = boot



boot :: Network.Wai.Application -> Middlewares -> IO ()
boot waiApp middlewares = do
  port <- Config.lookupEnv "PORT" 8081
  env  <- Config.lookupEnv "ENV" Config.Development

  nodeUrl <- Config.lookupEnv "NODE_URL" "127.0.0.1:1112"
  esUrl <- Config.lookupEnv "ES_URL" "http://127.0.0.1:8888"
  simpleIdUrl <- Config.lookupEnv "SIMPLEID_URL" "http://127.0.0.1:8000"

  let
    printStatus = do
      putStrLn $ "NODE_URL: " ++ show nodeUrl
      putStrLn $ "ES_URL: " ++ show esUrl
      putStrLn $ "SIMPLEID_URL: " ++ show simpleIdUrl
      putStrLn $ "Environment: " ++ show env
      putStrLn $ "Server started: http://localhost:" ++ show port

    run = W.defaultSettings
              & W.setBeforeMainLoop printStatus
              & W.setPort port
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
