{-# LANGUAGE FlexibleContexts #-}

module Concordium.Client.Parse
  ( parseTransactionHash
  , parseBlockHash
  , parseTime
  , parseCredExpiry
  , parseTimestamp ) where

import Concordium.Crypto.SHA256
import Concordium.Types

import Control.Monad.Except
import Data.Text
import Data.Time hiding (parseTime)
import Text.Printf

parseHash :: Text -> Maybe Hash
parseHash t =
  case reads $ unpack t :: [(Hash, String)] of
    [(h, "")] -> Just h
    _ -> Nothing

parseTransactionHash :: Text -> Maybe Hash
parseTransactionHash = parseHash

parseBlockHash :: Text -> Maybe Hash
parseBlockHash = parseHash

data DurationUnit = Second | Minute | Hour

type TimeFormat = String

-- Parse time from a string using the provided format.
-- This is a simple convenience wrapper around the more general function parseTimeM.
parseTime :: (MonadFail m) => TimeFormat -> String -> m UTCTime
parseTime = parseTimeM False defaultTimeLocale

-- Parse credential expiry time formatted as "<year (4 digits)><month (2 digits)>"
parseCredExpiry :: (MonadFail m) => String -> m UTCTime
parseCredExpiry = parseTime "%0Y%0m"

parseTimestamp :: (MonadError String m) => Timestamp -> Text -> m Timestamp
parseTimestamp now input  = do
  (t, u) <- parseDuration input
  return $ case u of
    Nothing -> t
    Just s -> now + t*multiplier s
  where multiplier = \case
          Second -> 1
          Minute -> 60
          Hour -> 3600

parseDuration :: (MonadError String m) => Text -> m (Timestamp, Maybe DurationUnit)
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
