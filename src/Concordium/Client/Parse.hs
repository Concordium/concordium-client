{-# LANGUAGE FlexibleContexts #-}

module Concordium.Client.Parse
  ( parseTransactionHash
  , parseBlockHash
  , parseTime
  , parseCredExpiry
  , parseTimestamp
  , parseSignKey
  , parseVerifyKey ) where

import qualified Concordium.Crypto.ByteStringHelpers as BSH
import qualified Concordium.Crypto.Ed25519Signature as Ed25519
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

parseTimestamp :: (MonadError String m) => TransactionExpiryTime -> Text -> m TransactionExpiryTime
parseTimestamp now input = do
  (t, u) <- parseDuration input
  return $ case u of
    Nothing -> t
    Just s -> now + t*multiplier s
  where multiplier = \case
          Second -> 1
          Minute -> 60
          Hour -> 3600

parseDuration :: (MonadError String m) => Text -> m (TransactionExpiryTime, Maybe DurationUnit)
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

parseSignKey :: (MonadError String m) => Text -> m Ed25519.SignKey
parseSignKey k = case BSH.deserializeBase16 k of
                   Nothing -> throwError $ printf "invalid sign key '%s' (should be base-16 string of length 64)" k
                   Just v -> return v


parseVerifyKey :: (MonadError String m) => Text -> m Ed25519.VerifyKey
parseVerifyKey k = case BSH.deserializeBase16 k of
                     Nothing -> throwError $ printf "invalid verify key '%s' (should be base-16 string of length 64)" k
                     Just v -> return v
