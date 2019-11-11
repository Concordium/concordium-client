{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api.Messages where

import Network.Wai                   (Application)
import Control.Monad.Managed         (liftIO)
import Data.Aeson                    (encode, decode')
import Data.Aeson.Types              (ToJSON, FromJSON, parseJSON, typeMismatch, (.:), Value(..))
import Data.Text                     (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.List.Split
import Data.Map
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import System.Directory
import System.Exit
import System.Environment
import System.IO.Error
import System.Process
import System.Random
import System.IO.Unsafe
import Text.Read (readMaybe)
import Lens.Simple

import           Concordium.Client.Runner
import           Concordium.Client.GRPC
import           Concordium.Client.Runner.Helper
import           Concordium.Client.Types.Transaction
import           Concordium.Client.Commands          as COM
import qualified Acorn.Parser.Runner                 as PR
import qualified Concordium.Types                    as Types
import qualified Concordium.Types.Transactions       as Types
import           Concordium.Crypto.SignatureScheme   (SchemeId (..), VerifyKey, KeyPair(..), correspondingVerifyKey)
import           Concordium.Crypto.Ed25519Signature  (randomKeyPair, deriveVerifyKey)
import qualified Concordium.ID.Account
import qualified Concordium.ID.Types
import qualified Proto.Concordium_Fields             as CF
import Control.Monad

import qualified Config
import SimpleIdClientApi
import EsApi

data BetaIdProvisionRequest =
  BetaIdProvisionRequest
    { attributes :: [(Text,Text)]
    , accountKeys :: Maybe AccountKeyPair
    }
  deriving (FromJSON, Generic, Show)

-- The BetaIdProvisionResponse is just what the SimpleIdClient returns for Identity Object provisioning
type BetaIdProvisionResponse = IdObjectResponse

data BetaAccountProvisionRequest =
  BetaAccountProvisionRequest
    { ipIdentity :: Int
    , preIdentityObject :: PreIdentityObject
    , privateData :: PrivateData
    , signature :: Text
    , revealedItems :: [Text]
    }
  deriving (ToJSON, FromJSON, Generic, Show)


data BetaAccountProvisionResponse =
  BetaAccountProvisionResponse
    { accountKeys :: AccountKeyPair
    , spio :: IdCredential
    , address :: Text
    }
  deriving (ToJSON, Generic, Show)


data BetaGtuDropResponse =
  BetaGtuDropResponse
    { transactionId :: Types.TransactionHash
    }
  deriving (ToJSON, Generic, Show)

data TransferRequest =
  TransferRequest
    { keypair :: KeyPair
    , to :: Types.Address
    , amount :: Types.Amount
    }
  deriving (FromJSON, Generic, Show)

data TransferResponse =
  TransferResponse
    { transactionId :: Types.TransactionHash
    }
  deriving (ToJSON, Generic, Show)

data GetNodeStateResponse =
  GetNodeStateResponse
    { name :: Text
    , id :: Text
    , version :: Text
    , running :: Bool
    , runningSince :: Int
    , sent :: Int
    , received :: Int
    , isBaking :: Bool
    , isInBakingCommittee :: Bool
    , isFinalizing :: Bool
    , isInFinalizingCommittee :: Bool
    , signatureVerifyKey :: Text
    , selectionVerifyKey :: Text
    , timestamp :: Int
    }
  deriving (FromJSON, ToJSON, Generic, Show)


data SetNodeStateRequest =
  SetNodeStateRequest
    { name :: Maybe Text
    , id :: Maybe Text
    , version :: Maybe Text
    , running :: Maybe Bool
    , runningSince :: Maybe Int
    , sent :: Maybe Int
    , received :: Maybe Int
    , isBaking :: Maybe Bool
    , isInBakingCommittee :: Maybe Bool
    , isFinalizing :: Maybe Bool
    , isInFinalizingCommittee :: Maybe Bool
    }
  deriving (FromJSON, ToJSON, Generic, Show)


data SetNodeStateResponse =
  SetNodeStateResponse
    { success :: Bool }
  deriving (FromJSON, ToJSON, Generic, Show)

data ReplayTransactionsRequest = ReplayTransactionsRequest  { adminToken :: Text }
  deriving (FromJSON, ToJSON, Generic, Show)

data ReplayTransactionsResponse = ReplayTransactionsResponse { success :: Bool }
  deriving (FromJSON, ToJSON, Generic, Show)
