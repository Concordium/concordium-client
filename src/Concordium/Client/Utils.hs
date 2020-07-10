module Concordium.Client.Utils where

import Control.Monad.Except

-- | In the 'Left' case of an 'Either', transform the error using the given function and
-- "rethrow" it in the current 'MonadError'.
embedErr :: MonadError e m => Either e' a -> (e' -> e) -> m a
embedErr (Left x) f = throwError (f x)
embedErr (Right a) _ = return a

-- | 'embedErr' for IO.
embedErrIO :: Either e' a -> (e' -> String) -> IO a
embedErrIO (Left x) f = throwError $ userError (f x)
embedErrIO (Right x) _ = return x

-- | Like 'embedErrIOM but where the value is also an IO action.
embedErrIOM :: IO (Either e' a) -> (e' -> String) -> IO a
embedErrIOM action f = do
  v <- action
  v `embedErrIO` f
