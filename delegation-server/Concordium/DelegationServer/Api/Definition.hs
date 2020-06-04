{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Request and response types for the different API endpoints. The expected flow
-- is:
--
-- 1. Requesting a delegation using @RequestDelegationRequest@.
-- 2a. Get back a transaction hash if a delegating account was available.
-- 2b. Get back the number of seconds that the user must wait to get a delegation.
-- 3. Query the status of the delegation using @GetDelegationStatusRequest@.
-- 4. Finally receiving a @RequestedDelegationFinished@ when the delegation has
-- finished.
module Concordium.DelegationServer.Api.Definition where

import Concordium.Client.Types.Transaction ()
import Concordium.Types (BakerId)
import Data.Aeson
import Data.Text (Text)
import Data.Word
import GHC.Generics
import Servant

-- * Requesting a delegation

-- | API endpoint that will queue a delegation request and if possible delegate
--  immediately.
type RequestDelegation = "requestDelegation" :> ReqBody '[JSON] RequestDelegationRequest :> Post '[JSON] RequestDelegationResponse

-- | Request for delegation
newtype RequestDelegationRequest
  = RequestDelegationRequest
      { -- | Baker Id which the request is done for.
        delegateTo :: BakerId
      }
  deriving (Generic)

instance FromJSON RequestDelegationRequest where
  parseJSON = withObject "RequestDelegation" $
    \v ->
      RequestDelegationRequest
        <$> v .: "delegateTo"

-- | Response to a delegation request. It will be either a
-- @RequestedDelegationPending@ when all the delegating accounts are busy and
-- the request was queued or a @RequestedDelegationDone@ if the request is done
-- immediately because there were available accounts.
data RequestDelegationResponse
  = RequestDelegationAccepted
      { expectedWaitingTime :: Word64
      }
  | RequestDelegationNotAccepted
  deriving (Generic, ToJSON)

-- * Querying for a delegation

type GetDelegationStatus = "getDelegationStatus" :> ReqBody '[JSON] GetDelegationStatusRequest :> Post '[JSON] GetDelegationStatusResponse

-- | Request information about a queued/done/finished delegation.
newtype GetDelegationStatusRequest
  = GetDelegationStatusRequest
      { -- | Baker Id to which the delegation should be done.
        delegateTo :: BakerId
      }
  deriving (Generic)

instance FromJSON GetDelegationStatusRequest where
  parseJSON = withObject "GetDelegationStatusRequest" $
    \v ->
      GetDelegationStatusRequest
        <$> v .: "delegateTo"

-- | Response to @GetDelegationStatusRequest@. Modeled after the alternatives in the flow in the @Logic@ module.
data GetDelegationStatusResponse
  = DelegationAbsent
      { estimatedWaitingTime :: Word64
      }
  | DelegationInQueue
      { estimatedWaitingTime :: Word64
      }
  | DelegationPending
      { txHash :: Text,
        estimatedStartTime :: Maybe Word64
      }
  | DelegationAssigned
      { txHash :: Text,
        estimatedEndTime :: Maybe Word64
      }
  | DelegationFinished
      { expiredAt :: Word64,
        estimatedWaitingTime :: Word64
      }
  deriving (Generic, ToJSON)

-- * Querying for all delegations

-- | API endpoint that will return a list of the current delegations that are
--  active at the time of the request together with their expiry times.
type GetDelegations = "getDelegations" :> Get '[JSON] CurrentDelegations

newtype CurrentDelegations = CurrentDelegations [Maybe BakerId]
  deriving (Generic)
  deriving newtype (ToJSON)

-- * API

type DelegationAPI = "v1" :> (GetDelegations :<|> RequestDelegation :<|> GetDelegationStatus)

api :: Proxy DelegationAPI
api = Proxy
