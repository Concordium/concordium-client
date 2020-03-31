module Concordium.Client.Utils where

import Data.Char

firstLower :: String -> String
firstLower [] = []
firstLower (c:cs) = toLower c : cs
