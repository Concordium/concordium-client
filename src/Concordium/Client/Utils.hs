{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Utils where

import Control.Monad.Except
import Concordium.Types
import Concordium.Crypto.ByteStringHelpers
import qualified Concordium.ID.Types as IDTypes
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import Text.Read

-- | In the 'Left' case of an 'Either', transform the error using the given function and
-- "rethrow" it in the current 'MonadError'.
embedErr :: MonadError e m => Either e' a -> (e' -> e) -> m a
embedErr (Left x) f = throwError (f x)
embedErr (Right a) _ = return a

-- | In the 'Left' case of an 'Either', transform the error using the given function and
-- "rethrow" it in the current 'MonadError'.
failWith :: MonadError e m => m (Either e' a) -> (e' -> e) -> m a
failWith act f = act >>= flip embedErr f

-- |Like 'failWith', but use MonadFail and just fail with the given message
-- without tranforming it.
failOnError :: MonadFail m => m (Either String a) -> m a
failOnError act = act >>=
    \case Left err -> fail err
          Right x -> return x

-- | 'embedErr' for IO.
embedErrIO :: Either e' a -> (e' -> String) -> IO a
embedErrIO (Left x) f = throwError $ userError (f x)
embedErrIO (Right x) _ = return x

-- | Like 'embedErrIOM but where the value is also an IO action.
embedErrIOM :: IO (Either e' a) -> (e' -> String) -> IO a
embedErrIOM action f = do
  v <- action
  v `embedErrIO` f

-- |Try to parse an amount from a string, and if failing, try to inform the user
-- what the expected format is. This is intended to be used by the options
-- parsers.
amountFromStringInform :: String -> Either String Amount
amountFromStringInform s =
  case amountFromString s of
    Just a -> Right a
    Nothing -> Left $ "Invalid GTU amount '" ++ s ++ "'. Amounts must be of the form n[.m] where m, if present,\n must have at least one and at most 6 digits."


-- |Try to parse a KeyIndex from a string, and if failing, try to inform the user
-- what the expected format is. This is intended to be used by the options
-- parsers.
indexFromStringInform :: String -> Either String IDTypes.KeyIndex
indexFromStringInform s =
   case readMaybe s :: Maybe Integer of
    Just a -> if a >= 0 && a <= 255 then Right (IDTypes.KeyIndex (fromIntegral a)) else Left errString
    Nothing -> Left errString
  where errString = "Invalid KeyIndex. A KeyIndex must be an integer between 0 and 255 inclusive."


-- |Try to parse a credential id from string, and if failing, try to inform the user
-- what the expected format is. This is intended to be used by the options
-- parsers.
credIdFromStringInform :: String -> Either String IDTypes.CredentialRegistrationID
credIdFromStringInform s =
  case deserializeBase16 (Text.pack s) of
    Just a -> Right a
    Nothing -> Left $ "Invalid credential registration ID."


-- |Try to parse `Energy` from a string, and, if failing, inform the user
-- what the expected format and bounds are.
-- This is intended to be used by the options parsers.
energyFromStringInform :: String -> Either String Energy
energyFromStringInform s =
  -- Reading negative numbers directly to Energy (i.e. Word64) silently underflows, so this approach is necessary.
  case readMaybe s :: Maybe Integer of
    Just a -> if a >= nrgMinBound && a <= nrgMaxBound
              then Right . fromIntegral $ a
              else Left errMsg
    Nothing -> Left errMsg
  where errMsg = [i|Invalid energy '#{s}'. Energy must be an integer between #{nrgMinBound} and #{nrgMaxBound}, both inclusive.|]
        nrgMinBound = 0
        nrgMaxBound = fromIntegral (maxBound :: Energy)

-- |Try to parse the signature threshold from string with a meaningful error message if unsuccessful.
thresholdFromStringInform :: String -> Either String IDTypes.SignatureThreshold
thresholdFromStringInform s =
  case readMaybe s :: Maybe Integer of
    Just a -> if a >= 1 && a <= 255 then Right (IDTypes.SignatureThreshold (fromIntegral a)) else Left errString
    Nothing -> Left errString
  where errString = "Invalid signature threshold. A signature threshold must be an integer between 1 and 255 inclusive."

-- |Try to parse the account threshold from string with a meaningful error message if unsuccessful.
accountThresholdFromStringInform :: String -> Either String IDTypes.AccountThreshold
accountThresholdFromStringInform s =
  case readMaybe s :: Maybe Integer of
    Just a -> if a >= 1 && a <= 255 then Right (IDTypes.AccountThreshold (fromIntegral a)) else Left errString
    Nothing -> Left errString
  where errString = "Invalid account threshold. A signature threshold must be an integer between 1 and 255 inclusive."


-- |Try to parse the credential index
credentialIndexFromStringInform :: String -> Either String IDTypes.CredentialIndex
credentialIndexFromStringInform s =
  case readMaybe s :: Maybe Integer of
    Just a | a >= 0 && a <= 255 -> Right (IDTypes.CredentialIndex (fromIntegral a))
           | a >= 0 -> Left "Credential index must be less than or equal to 255."
           | otherwise -> Left "Credential index must be non-negative."
    Nothing -> Left "Credential index must be an integer between 0 and 255 (inclusive)."

