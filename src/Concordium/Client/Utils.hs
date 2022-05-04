{-# LANGUAGE QuasiQuotes #-}
module Concordium.Client.Utils where

import Control.Monad.Except
import Concordium.Types
import Concordium.Crypto.ByteStringHelpers
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Wasm as Wasm
import Data.String.Interpolate (i, iii)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy.Text
import qualified Data.Text.Lazy.Encoding as Lazy.Text
import Data.Word (Word64)
import Text.Read
import qualified Data.Char as Char
import Data.Maybe (mapMaybe)
import Data.Aeson as AE

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
    Nothing -> Left $ "Invalid CCD amount '" ++ s ++ "'. Amounts must be of the form n[.m] where m, if present,\n must have at least one and at most 6 digits."

amountFractionFromStringInform :: String -> Either String AmountFraction
amountFractionFromStringInform s =
  case AE.decode (Lazy.Text.encodeUtf8 $ Lazy.Text.pack s) of
    Just a -> Right a
    Nothing -> Left $ "Invalid decimal fraction '" ++ s ++ "'. A decimal fractions must be a decimal number n.m between 0 and 1 (both inclusive), and with at most 5 digits of precision."

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

-- |Try to parse an account alias counter from a string, and, if failing, inform
-- the user what the expected format and bounds are. This is intended to be used
-- by the options parsers.
aliasFromStringInform :: String -> Either String Word
aliasFromStringInform s =
  -- Reading negative numbers directly to Word silently underflows, so this approach is necessary.
  case readMaybe s :: Maybe Integer of
    Just a -> if a >= aliasMinBound && a <= aliasMaxBound
              then Right . fromIntegral $ a
              else Left errMsg
    Nothing -> Left errMsg
  where errMsg = [i|Invalid alias number '#{s}'. Alias must be an integer between #{aliasMinBound} and #{aliasMaxBound}, both inclusive.|]
        aliasMinBound = 0
        aliasMaxBound = 2^(8 * (IDTypes.accountAddressSize - accountAddressPrefixSize)) - 1


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

-- |Try to parse a WasmVersion.
contractVersionFromStringInform :: String -> Either String Wasm.WasmVersion
contractVersionFromStringInform s =
  case s of
    "v0" -> Right Wasm.V0
    "V0" -> Right Wasm.V0
    "0" -> Right Wasm.V0
    "v1" -> Right Wasm.V1
    "V1" -> Right Wasm.V1
    "1" -> Right Wasm.V1
    _ -> Left "Contract version must be one of [v0, V0, 0, v1, V1, 1]."

-- Time Units and durations

secInMs :: Word64
secInMs = 1000

minInMs :: Word64
minInMs = 60 * secInMs

hrInMs :: Word64
hrInMs = 60 * minInMs

dayInMs :: Word64
dayInMs = 24 * hrInMs

-- |Convert a duration in milliseconds into text with a list of duration measures separated by a space.
-- A measure is a non-negative integer followed by the unit (with no whitespace in between).
-- The support units are: days (d), hours (h), minutes (m), seconds (s), milliseconds (ms).
-- Measures that are 0 are omitted from the output (see example, where 'd' and 'ms' are omitted).
-- Example: 5022000 -> "1h 23m 42s".
durationToText :: Word64 -> Text.Text
durationToText t = Text.intercalate " " . mapMaybe showTimeUnit $
                  [(d, "d"), (h, "h"), (m, "m"), (s, "s"), (ms, "ms")]
  where
    (d, rem0) = quotRem t dayInMs
    (h, rem1) = quotRem rem0 hrInMs
    (m, rem2) = quotRem rem1 minInMs
    (s, ms)   = quotRem rem2 secInMs

    -- Show the time unit if it is non-zero, otherwise return Nothing.
    showTimeUnit :: (Word64, Text.Text) -> Maybe Text.Text
    showTimeUnit (value, unit) =
        if value == 0
        then Nothing
        else Just [i|#{value}#{unit}|]

-- |Parse a string containing a list of duration measures separated by
-- spaces. A measure is a non-negative integer followed by a unit (no whitespace is allowed in between).
-- Every measure is accumulated into a duration. The string is allowed to contain
-- any number of measures with the same unit in no particular order.
-- The support units are: days (d), hours (h), minutes (m), seconds (s), milliseconds (ms).
-- Example: "1d 2h 3m 2d 1h" -> Right 270180000
textToDuration :: Text.Text -> Either String Word64
textToDuration t = mapM measureToMs measures >>= Right . sum
  where measures :: [String]
        measures = map Text.unpack . Text.split (== ' ') $ t

        measureToMs :: String -> Either String Word64
        measureToMs m = do
          let (digits, unit) = span Char.isDigit m

          value <- word64FromString digits
          unit' <- case unit of
            "ms" -> Right 1
            "s"  -> Right secInMs
            "m"  -> Right minInMs
            "h"  -> Right hrInMs
            "d"  -> Right dayInMs
            _    -> Left invalidMeasureErrorMsg
          Right $ value * unit'

          where
            invalidMeasureErrorMsg = [iii|"Invalid measure of time '#{m}'.
                                          Should be a non-negative integer followed by a unit (d, h, m, s, ms)."|]

            -- Reading negative numbers directly to Word64 silently underflows, so this approach is necessary.
            word64FromString :: String -> Either String Word64
            word64FromString s = case readMaybe s :: Maybe Integer of
                                  Nothing -> Left invalidMeasureErrorMsg
                                  Just x -> if x >= word64MinBound && x <= word64MaxBound
                                              then Right . fromIntegral $ x
                                              else Left invalidMeasureErrorMsg
              where word64MinBound = 0
                    word64MaxBound = fromIntegral (maxBound :: Word64)

applyAlias :: Maybe Word -> AccountAddress -> AccountAddress
applyAlias Nothing addr = addr
applyAlias (Just alias) addr = createAlias addr alias
