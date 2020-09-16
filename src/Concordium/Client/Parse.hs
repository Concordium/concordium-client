module Concordium.Client.Parse
  ( parseTransactionHash
  , parseBlockHash
  , parseTime
  , parseCredExpiry
  , parseExpiry
  ) where

import Concordium.Crypto.SHA256
import Concordium.Types

import Control.Monad.Except
import Data.Text
import Data.Time hiding (parseTime)
import Data.Word
import Text.Read
import Text.Printf

parseTransactionHash :: Text -> Maybe TransactionHashV0
parseTransactionHash = readMaybe . unpack

parseBlockHash :: Text -> Maybe Hash
parseBlockHash = readMaybe . unpack

data DurationUnit = Second | Minute | Hour

type TimeFormat = String

-- |Parse time from a string using the provided format.
-- This is a simple convenience wrapper around the more general function parseTimeM.
parseTime :: (MonadFail m) => TimeFormat -> String -> m UTCTime
parseTime = parseTimeM False defaultTimeLocale

-- |Parse credential expiry time formatted as "<year (4 digits)><month (2 digits)>"
parseCredExpiry :: (MonadFail m) => String -> m UTCTime
parseCredExpiry = parseTime "%0Y%0m"

-- |Parse expiry time given as absolute Unix epoch or a duration string
-- relative to the provided "now" time.
parseExpiry :: (MonadError String m) => TransactionExpiryTime -> Text -> m TransactionExpiryTime
parseExpiry now input = do
  (t, u) <- parseDuration input
  let e = TransactionTime t
  return $ case u of
            Nothing -> e
            Just s -> now + e*multiplier s
  where multiplier = \case
          Second -> 1
          Minute -> 60
          Hour -> 3600

-- |Parse a string into an integer and an optional duration unit.
parseDuration :: (MonadError String m) => Text -> m (Word64, Maybe DurationUnit)
parseDuration t =
  case reads $ unpack t of
    [(n, r)] -> do
      unit <- case toLower $ strip $ pack r of
                "" -> return Nothing
                "s" -> return $ Just Second
                "m" -> return $ Just Minute
                "h" -> return $ Just Hour
                _ -> throwError $ printf "unsupported suffix '%s'" r
      return (n, unit)
    _ -> throwError "non-numeric prefix"
