module Main where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Data.Maybe
import           Options.Applicative
import           System.Environment

ccPrefs :: ParserPrefs
ccPrefs = defaultPrefs { prefColumns = 110,
                         prefShowHelpOnEmpty = True}

main :: IO ()
main = do
  showAllOpts <- lookupEnv "SHOW_ALL_OPTS"
  process =<< customExecParser ccPrefs (optsParser $ isJust showAllOpts)
