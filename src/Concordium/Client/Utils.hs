module Concordium.Client.Utils where

import Control.Monad.Except
import Concordium.Types

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

