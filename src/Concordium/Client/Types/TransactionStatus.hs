{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.Client.Types.TransactionStatus where

import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.Aeson.TH
import Data.Char

import Concordium.Types.Execution
import Concordium.Types

data TransactionState = Received | Committed | Finalized | Absent deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''TransactionState)


type TransactionBlockResults' a = HM.HashMap BlockHash (Maybe (TransactionSummary' a))

type TransactionBlockResults = TransactionBlockResults' ValidResult

data TransactionStatusResult' a = TransactionStatusResult
  { tsrState :: !TransactionState
  , tsrResults :: !(TransactionBlockResults' a) } -- TODO Rename to "blocks".
  deriving (Eq, Show)

type TransactionStatusResult = TransactionStatusResult' ValidResult

instance FromJSON a => FromJSON (TransactionStatusResult' a) where
  parseJSON Null = return TransactionStatusResult{tsrState = Absent, tsrResults = HM.empty}
  parseJSON v = flip (withObject "Transaction status") v $ \obj -> do
    tsrState <- obj .: "status"
    tsrResults <- obj .:? "outcomes" .!= HM.empty
    return $ TransactionStatusResult {..}

instance ToJSON a => ToJSON (TransactionStatusResult' a) where
  toJSON TransactionStatusResult{..} =
    object $ ("status" .= tsrState):mapObject
    where mapObject =
            case HM.null tsrResults of
              True -> []
              False -> ["outcomes" .= tsrResults]
