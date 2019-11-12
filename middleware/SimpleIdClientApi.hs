{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleIdClientApi where

import Data.Text                 (Text)
import Data.Aeson.Types          (ToJSON(..), FromJSON)
import Data.Aeson                (Value(..), (.=))
import Servant.API.Generic
import Data.Map
import qualified Data.HashMap.Strict as DMS (fromList)
import qualified Data.Text as Text

import Http
import Concordium.Crypto.SignatureScheme (KeyPair(..))
import Concordium.ID.Types (CredentialDeploymentInformation(..))


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
    , name :: Text
    , attributes :: Map Text Text
    , anonymityRevokers :: [Int]
    , threshold :: Int
    }
  deriving (Generic, Show, ToJSON)

sampleIdObjectRequest :: IdObjectRequest
sampleIdObjectRequest =
  IdObjectRequest
    { ipIdentity = 0
    , name = "Ales"
    , attributes = fromList
        [ ("birthYear", "2013")
        , ("creationTime", "1341324324")
        , ("expiryDate", "1910822399")
        , ("maxAccount", "30")
        , ("residenceCountryCode", "386")
        , ("variant", "0")
        ]
    , anonymityRevokers = [0,1,2]
    , threshold = 2
    }


data IdObjectResponse =
  IdObjectResponse
    { ipIdentity :: Int
    , preIdentityObject :: PreIdentityObject
    , privateData :: PrivateData
    , signature :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data PreIdentityObject =
  PreIdentityObject
    { accountHolderName :: Text
    , attributeList :: Map Text Text
    , idCredPubIp :: Text
    , idCredPub :: Text
    , idCredSecCommitment :: Text
    , sndIdCredSecCommitment :: Text
    , ipArData :: [IpArData]
    , choiceArData :: [Int]
    , revocationThreshold :: Int
    , pokSecCred :: Text
    , sndPokSecCred :: Text
    , proofCommitmentsToIdCredSecSame :: Text
    , prfKeyCommitmentWithID :: Text
    , prfKeySharingCoeffCommitments :: [Text]
    , proofCommitmentsSame :: Text
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

data PrivateData =
  PrivateData
    { aci :: PrivateDataAci
    , pioRandomness :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data PrivateDataAci =
  PrivateDataAci
    { attributes :: Map Text Text
    , credentialHolderInformation :: CredentialHolderInformation
    , prfKey :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data CredentialHolderInformation =
  CredentialHolderInformation
    { idCredPublic :: Text
    , idCredSecret :: Text
    , name :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

-- Identical to IdObjectResponse with addition of revealedItems and accountNumber fields.
data IdCredentialRequest =
  IdCredentialRequest
    { ipIdentity :: Int
    , preIdentityObject :: PreIdentityObject
    , privateData :: PrivateData
    , signature :: Text
    , revealedItems :: [Text]
    , accountNumber :: Int
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
    { accountKeyPair :: KeyPair
    , credential :: CredentialDeploymentInformation
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdCredRevealedItem =
  IdCredRevealedItem
    { index :: Int
    , value :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)
