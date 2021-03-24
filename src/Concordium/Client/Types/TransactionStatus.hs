{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.Client.Types.TransactionStatus where

import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.TH

import Concordium.Types.Execution
import Concordium.Types
import Concordium.Utils

data TransactionState = Received | Committed | Finalized | Absent deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions{constructorTagModifier = firstLower} ''TransactionState)

type TransactionBlockResults' a = Map.Map BlockHash (TransactionSummary' a)

type TransactionBlockResults = TransactionBlockResults' ValidResult

data TransactionStatusResult' a = TransactionStatusResult
  { tsrState :: !TransactionState
  , tsrResults :: !(TransactionBlockResults' a) } -- TODO Rename to "blocks".
  deriving (Eq, Show)

type TransactionStatusResult = TransactionStatusResult' ValidResult

instance FromJSON a => FromJSON (TransactionStatusResult' a) where
  parseJSON Null = return TransactionStatusResult{tsrState = Absent, tsrResults = Map.empty}
  parseJSON v = flip (withObject "Transaction status") v $ \obj -> do
    tsrState <- obj .: "status"
    tsrResults <- obj .:? "outcomes" .!= Map.empty
    return $ TransactionStatusResult {..}

instance ToJSON a => ToJSON (TransactionStatusResult' a) where
  toJSON TransactionStatusResult{..} =
    object $ ("status" .= tsrState):mapObject
    where mapObject = ["outcomes" .= tsrResults | Map.null tsrResults]
