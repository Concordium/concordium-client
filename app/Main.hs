module Main where

import           Concordium.Client.Commands
import           Concordium.Client.Runner
import           Control.Monad.Reader
import           Options.Applicative

main :: IO ()
main = process =<< execParser optsParser
