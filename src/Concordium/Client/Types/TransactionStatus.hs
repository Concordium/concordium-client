{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Concordium.Client.Types.TransactionStatus where

import Control.Monad (foldM)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Map.Strict as Map

import Concordium.Types
import qualified Concordium.Types.Queries as Queries
import Concordium.Utils

data TransactionState = Received | Committed | Finalized | Absent deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions{constructorTagModifier = firstLower} ''TransactionState)

type TransactionBlockResults = Map.Map BlockHash Queries.SupplementedTransactionSummary

data TransactionStatusResult = TransactionStatusResult
    { tsrState :: !TransactionState,
      tsrResults :: !TransactionBlockResults -- TODO Rename to "blocks".
    }
    deriving (Eq, Show)

-- | Convert a @TransactionStatus@ instance into a @TransactionStatusResult@ instance.
--  Returns a @Left@ wrapping an error message if either the transaction summary contained in the
--  input is @Nothing@, or the input is of variant @Committed@ or @Finalized@. Returns a @Right@
--  wrapping the corresponding @TransactionStatusResult@ otherwise.
transactionStatusToTransactionStatusResult :: Queries.SupplementedTransactionStatus -> Either String TransactionStatusResult
transactionStatusToTransactionStatusResult tStatus = do
    (tsrState, tsrResults) <- do
        case tStatus of
            Queries.Received ->
                return (Received, Map.empty)
            Queries.Committed bhToSummMap -> do
                bhToSummlist <-
                    foldM
                        ( \acc (bh, summM) -> do
                            case summM of
                                Nothing ->
                                    err bh
                                Just summ ->
                                    Right $ acc <> [(bh, summ)]
                        )
                        []
                        (Map.toList bhToSummMap)
                return (Committed, Map.fromList bhToSummlist)
            Queries.Finalized bh Nothing -> err bh
            Queries.Finalized bh (Just summ) ->
                return (Finalized, Map.fromList [(bh, summ)])
    return TransactionStatusResult{..}
  where
    err bh = Left $ "Transaction summary missing for blockhash '" <> show bh <> "'."

instance FromJSON (TransactionStatusResult) where
    parseJSON Null = return TransactionStatusResult{tsrState = Absent, tsrResults = Map.empty}
    parseJSON v = flip (withObject "Transaction status") v $ \obj -> do
        tsrState <- obj .: "status"
        tsrResults <- obj .:? "outcomes" .!= Map.empty
        return $ TransactionStatusResult{..}

instance ToJSON (TransactionStatusResult) where
    toJSON TransactionStatusResult{..} =
        object $ ("status" .= tsrState) : mapObject
      where
        mapObject = ["outcomes" .= tsrResults | not (Map.null tsrResults)]
