{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api where

import Network.Wai                   (Application)
import Control.Monad.Managed         (liftIO)
import Data.Text                     (Text)
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


data Routes r = Routes
    { sendTransaction :: r :-
        "v1" :> "sendTransaction" :> ReqBody '[JSON] TransactionJSON
                                  :> Post '[JSON] Text
    , typecheckContract :: r :-
        "v1" :> "typecheckContract" :> ReqBody '[JSON] Text
                                    :> Post '[JSON] Text
    }
  deriving (Generic)


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

      let nid = 1234 -- @TODO what is the network ID actually for...? What should it be in wallet context?

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
