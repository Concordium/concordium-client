{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SimpleClientTests.QueryTransaction where

import Concordium.Client.Cli

import Control.Monad.State.Strict

newtype TestTransactionStatusQuery a = TestTransactionStatusQuery { _runQuery :: State ([TransactionStatusResult], Int) a }
  deriving (Functor, Applicative, Monad, MonadState ([TransactionStatusResult], Int))

instance TransactionStatusQuery TestTransactionStatusQuery where
  queryTransactionStatus _ = do
    (rss, c) <- get
    case rss of
      [] -> error "unexpected call to 'queryTransactionStatus'"
      (r:rs) -> do
        put (rs, c)
        return r
  wait _ = modify' (\(v, c) -> (v, c+1))

runQuery :: TestTransactionStatusQuery a -> ([TransactionStatusResult], Int) -> (a, ([TransactionStatusResult], Int))
runQuery c s = runState (_runQuery c) s
