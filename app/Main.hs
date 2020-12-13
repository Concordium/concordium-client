module Main where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Data.Maybe
import           Options.Applicative
import           System.Environment
import           System.IO
ccPrefs :: ParserPrefs
ccPrefs = defaultPrefs { prefColumns = 110,
                         prefShowHelpOnEmpty = True}

-- |Set the encoding on the given handle in such a way that unsupported
-- characters are replaced with a replacement character.
-- See: https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html#v:mkTextEncoding
setReplacement :: Handle -> IO ()
setReplacement h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce -> do
      newEncoding <- mkTextEncoding ((takeWhile (/= '/') $ show ce) ++ "//TRANSLIT")
      hSetEncoding h newEncoding

main :: IO ()
main = do
  mapM_ setReplacement [stdout, stderr]
  showAllOpts <- lookupEnv "SHOW_ALL_OPTS"
  process =<< customExecParser ccPrefs (optsParser $ isJust showAllOpts)
