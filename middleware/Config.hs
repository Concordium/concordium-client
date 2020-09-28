module Config where

import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, unpack)
import qualified System.Environment as E (lookupEnv)
import           Text.Read (readMaybe)

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

lookupEnvTextWithoutDefault :: Text  -> IO (Maybe Text)
lookupEnvTextWithoutDefault name = do
  param <- E.lookupEnv (unpack name)
  pure $ pack <$> param
