{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Api where

import Network.Wai              (Application)
import Control.Monad.Managed    (liftIO)
import Data.Monoid              ((<>))
import qualified Data.Text as T (pack, splitOn, unpack, intercalate)
import Data.Text                (Text)
import Text.Read                (readMaybe)
import Data.List                (find)
import System.Exit              (ExitCode(..))
import System.Process
import Servant
import Servant.API.Generic
import Servant.Server.Generic


-- import           Concordium.Client.Commands
-- import           Concordium.Client.Runner
-- import           Control.Monad.Reader
-- import           Options.Applicative


data Routes r = Routes
    { sendTransaction :: r :-
        "v1" :> "sendTransaction" :> Get '[JSON] Int
    }
  deriving (Generic)


api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)


servantApp :: Application
servantApp = genericServe routesAsServer
 where
  routesAsServer = Routes {..} :: Routes AsServer

  sendTransaction :: Handler Int
  sendTransaction = do
    -- @TODO change this to a POST and integrate with actual sendTransaction code
    pure 42
