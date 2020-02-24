module Concordium.Client.Output where

import Concordium.Client.Cli

import Control.Monad
import Data.List
import Data.Time
import Text.Printf

getFormattedLocalTimeOfDay :: IO String
getFormattedLocalTimeOfDay = do
  t <- getLocalTimeOfDay
  return $ showFormattedTimeOfDay t

getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
  tz <- getCurrentTimeZone
  t <- getCurrentTime
  return $ localTimeOfDay $ utcToLocalTime tz t

showFormattedTimeOfDay :: TimeOfDay -> String
showFormattedTimeOfDay = formatTime defaultTimeLocale "%T"

printPresentBlockList :: [TransactionStatusResultItem] -> IO ()
printPresentBlockList rs = case Prelude.length rs of
  0 -> return ()
  1 -> do
    let r = rs !! 0
    printf "Transaction is present in block %s with status \"%s\".\n" (show $ tsriBlockHash r) (tsriResult r)
  n -> do
    printf "Transaction is present in %d blocks:\n" n
    forM_ (Prelude.zip rs [1..]) $ \(r, i) ->
      printf "%d: %s with status %s\n" (i::Int) (show $ tsriBlockHash r) (tsriResult r)

printTransactionStatus :: TransactionStatusResult -> IO ()
printTransactionStatus status =
  case tsrState status of
    Pending -> putStrLn "Transaction is pending."
    Absent -> putStrLn "Transaction is absent."
    Committed -> do
      printf "Transaction is committed%s.\n" costsFragment
      printBlocks
    Finalized -> do
      printf "Transaction is finalized%s.\n" costsFragment
      printBlocks
  where
    res = tsrResults status
    printBlocks = printPresentBlockList res
    costs = let cs = Prelude.map tsriExecutionCost res
                es = Prelude.map tsriExecutionEnergyCost res
            in Prelude.zip cs es
    costsFragment = case nub costs of
                      [] -> "" :: String
                      [(c, e)] -> printf " with cost %s GTU (%s NRG)" (show c) (show e)
                      cs -> printf " with ambiguous cost %s (debug: %s)" (show cs) (show res)
