module Concordium.Client.Output where

import Concordium.Client.Cli
import Concordium.Types

import Control.Monad.Writer
import Data.Functor
import Data.List
import Data.Text (Text)
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

data TransactionOutcome = TransactionOutcome
                          { toStatus :: Text
                          , toGtuCost :: Amount
                          , toNrgCost :: Energy }
                        deriving (Eq)

type Printer = Writer [String] ()

runPrinter :: Printer -> IO ()
runPrinter p = mapM_ putStrLn $ execWriter p

transactionOutcome :: TransactionStatusResultItem -> TransactionOutcome
transactionOutcome t = TransactionOutcome
                       { toStatus = tsriResult t
                       , toGtuCost = tsriExecutionCost t
                       , toNrgCost = tsriExecutionEnergyCost t }

printTransactionStatus :: TransactionStatusResult -> Printer
printTransactionStatus status =
  case tsrState status of
    Pending -> tell ["Transaction is pending."]
    Absent -> tell ["Transaction is absent."]
    Committed -> do
      case Prelude.map (\t -> (tsriBlockHash t, transactionOutcome t)) $ tsrResults status of
        [] ->
          -- No blocks.
          tell ["Transaction is committed (no block information received)."]
        [(hash, outcome)] ->
          -- Single block.
          tell [printf
                "Transaction is committed into block %s with %s."
                (show hash)
                (showOutcomeFragment outcome)]
        blocks ->
          -- Multiple blocks.
          case nub $ Prelude.map snd blocks of
            [outcome] -> do
              -- Single outcome.
              tell [printf
                     "Transaction is committed into the following %d blocks with %s:"
                     (length blocks)
                     (showOutcomeFragment outcome)]
              tell $ blocks <&> \(hash, _) ->
                                  printf "- %s" (show hash)
            _ -> do
              -- Multiple outcomes.
              tell [printf
                     "Transaction is committed into the following %d blocks:"
                     (length blocks)]
              tell $ blocks <&> \(hash, outcome) ->
                                  printf
                                    "- %s with %s."
                                    (show hash)
                                    (showOutcomeFragment outcome)
    Finalized -> do
      case tsrResults status of
        [] ->
          -- No blocks.
          tell ["Transaction is finalized (no block information received)."]
        [r] ->
          -- Single block.
          tell [printf
                 "Transaction is finalized into block %s with %s."
                 (show $ tsriBlockHash r)
                 (showOutcomeFragment $ transactionOutcome r)]
        _ ->
          -- Multiple blocks.
          tell ["Transaction is finalized into multiple blocks - this is very unexpected!"]
  where
    showCostFragment :: Amount -> Energy -> String
    showCostFragment gtu nrg = printf "%s GTU (%s NRG)" (show gtu) (show nrg)
    showOutcomeFragment :: TransactionOutcome -> String
    showOutcomeFragment outcome = printf
                                    "status \"%s\" and cost %s"
                                    (toStatus outcome)
                                    (showCostFragment (toGtuCost outcome) (toNrgCost outcome))
