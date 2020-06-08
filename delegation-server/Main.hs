{-# LANGUAGE Rank2Types #-}

module Main where

import Concordium.DelegationServer.Server
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stdout, utf8)

main :: IO ()
main = do
  stdoutSetup
  putStrLn "[server] Booting..."
  -- Boot the http server
  let middlewares = allowCsrf . corsified
  runHttp middlewares
  -- FIXME: Refactor server start so that the following hack is not needed.
  forever $ threadDelay 100000

-- | Force LineBuffering for consistent output behavior
stdoutSetup :: IO ()
stdoutSetup = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
