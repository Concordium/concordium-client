module Concordium.Client.Utils where

import Control.Monad.Except

-- | In the 'Left' case of an 'Either', transform the error using the given function and
-- "rethrow" it in the current 'MonadError'.
embedErr :: MonadError e m => Either e' a -> (e' -> e) -> m a
embedErr (Left x) f = throwError (f x)
embedErr (Right a) _ = return a
