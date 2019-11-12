module Config where

import Text.Read                            (readMaybe)
import qualified System.Environment as E    (lookupEnv)
import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Data.Maybe                           (fromMaybe)
import Data.Text                            (Text, pack, unpack)


data Environment =
    Development
  | Test
  | Staging
  | Production
  deriving (Eq, Show, Read)


lookupEnv :: Read a => String -> a -> IO a
lookupEnv name defaultVal = do
  param <- E.lookupEnv name
  pure $ case param of
    Nothing -> defaultVal
    Just a  -> fromMaybe defaultVal (readMaybe a)


lookupEnvText :: Text -> Text -> IO Text
lookupEnvText name defaultVal = do
  param <- E.lookupEnv (unpack name)
  pure $ maybe defaultVal pack param


logger :: Environment -> Middleware
logger Test        = id
logger Development = logStdoutDev
logger Staging     = logStdoutDev
logger Production  = logStdout
