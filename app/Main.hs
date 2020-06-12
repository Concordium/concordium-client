module Main where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Options.Applicative

ccPrefs :: ParserPrefs
ccPrefs = defaultPrefs { prefColumns = 110,
                         prefShowHelpOnEmpty = True}

main :: IO ()
main = process =<< customExecParser ccPrefs optsParser
