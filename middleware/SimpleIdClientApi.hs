{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleIdClientApi where

import           Data.Aeson (Value(..), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (ToJSON(..), FromJSON)
import qualified Data.HashMap.Strict as DMS (fromList)
import           Data.Map
import           Data.Word
import           Data.Text (Text)
import qualified Data.Text as Text
import           Servant.API.Generic
import qualified Data.HashMap.Strict as Map

import           Http
import           Concordium.Crypto.SignatureScheme (KeyPair(..))
import           Concordium.ID.Types (CredentialDeploymentInformation(..))
import qualified Concordium.Client.Types.Transaction as Types
import qualified Concordium.ID.Types as IDTypes


-- API requests


-- getIdentityProviders :: IO [IdentityProvider]
-- getIdentityProviders =
--     getJsonRequest "localhost:8000/ips"


postIdObjectRequest :: Text -> IdObjectRequest -> IO IdObjectResponse
postIdObjectRequest idUrl =
    postJsonRequest (Text.unpack idUrl ++ "/identity_object")


postIdCredentialRequest :: Text -> IdCredentialRequest -> IO IdCredentialResponse
postIdCredentialRequest idUrl =
    postJsonRequest (Text.unpack idUrl ++ "/generate_credential")


-- Data types


data IdObjectRequest =
  IdObjectRequest
    { ipIdentity :: Int
    , attributes :: Attributes
    , anonymityRevokers :: [Int]
    , threshold :: Int
    }
  deriving (Generic, Show, ToJSON)


data Attributes =
  Attributes
    { chosenAttributes :: Map Text Text
    , createdAt :: Text
    , validTo :: Text
    , maxAccounts :: Int
    }
  deriving (Generic, Show, ToJSON, FromJSON)


sampleIdObjectRequest :: IdObjectRequest
sampleIdObjectRequest =
  IdObjectRequest
    { ipIdentity = 0
    , attributes =
        Attributes
          { chosenAttributes =
              fromList
                [ ("dob", "19800101")
                , ("countryOfResidence", "386")
                ]
                , createdAt = "203001"
                , validTo = "203101"
                , maxAccounts = 30
          }
    , anonymityRevokers = [0,1,2]
    , threshold = 2
    }


data IdObjectResponse =
  IdObjectResponse
    { ipIdentity :: Int
    , identityObject :: IdentityObject
    , idUseData :: IdUseData
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdentityObject =
  IdentityObject
    { attributeList :: Attributes
    , preIdentityObject :: PreIdentityObject
    , signature :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data PreIdentityObject =
  PreIdentityObject
    { idCredPub :: Text
    , idCredSecCommitment :: Text
    , ipArData :: [IpArData]
    , choiceArData :: ChoiceArData
    , pokSecCred :: Text
    , proofCommitmentsToIdCredSecSame :: Text
    , prfKeyCommitmentWithIP :: Text
    , prfKeySharingCoeffCommitments :: [Text]
    , proofCommitmentsSame :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data ChoiceArData =
  ChoiceArData
    { arIdentities :: [Int]
    , threshold :: Int
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IpArData =
  IpArData
    { arIdentity :: Int
    , encPrfKeyShare :: Text
    , prfKeyShareNumber :: Int
    , proofComEncEq :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdUseData =
  IdUseData
    { aci :: IdUseDataAci
    , randomness :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdUseDataAci =
  IdUseDataAci
    { credentialHolderInformation :: CredentialHolderInformation
    , prfKey :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data CredentialHolderInformation =
  CredentialHolderInformation
    { idCredSecret :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

-- Identical to IdObjectResponse with addition of revealedItems and accountNumber fields.
data IdCredentialRequest =
  IdCredentialRequest
    { ipIdentity :: Int
    , identityObject :: IdentityObject
    , idUseData :: IdUseData
    , revealedAttributes :: Map Text Text
    , accountNumber :: Word8
    }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToJSON CredentialDeploymentInformation where
  toJSON CredentialDeploymentInformation{..} = Object $
    (case toJSON cdiValues of
      Object o -> o
      _ -> error "Should not happen")
    <> DMS.fromList ["proof" .= cdiProofs]

data IdCredentialResponse =
  IdCredentialResponse
    { accountAddress :: IDTypes.AccountAddress
    , accountData :: AccountDataKeys
    , credential :: CredentialDeploymentInformation
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data AccountDataKeys =
  AccountDataKeys
    { keys :: Types.KeyMap
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdCredRevealedItem =
  IdCredRevealedItem
    { index :: Int
    , value :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)
