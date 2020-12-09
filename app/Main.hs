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

-- |makeSafe handles encoding errors when cli does not accept all unicode characters
makeSafe :: Handle -> IO ()                         
makeSafe h = do
  ce' <- hGetEncoding h
  case ce' of
    Nothing -> return ()
    Just ce -> mkTextEncoding ((takeWhile (/= '/') $ show ce) ++ "//TRANSLIT") >>=
      hSetEncoding h

main :: IO ()
main = do
  mapM_ makeSafe [stdout, stderr]
  showAllOpts <- lookupEnv "SHOW_ALL_OPTS"
  process =<< customExecParser ccPrefs (optsParser $ isJust showAllOpts)
