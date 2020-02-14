{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module SimpleIdClientApi where

import           Data.Aeson (Value(..), (.=))
import           Data.Aeson.Types (ToJSON(..), FromJSON)
import qualified Data.HashMap.Strict as DMS (fromList)
import           Data.Map
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
    , name :: Text
    , attributes :: Attributes
    , anonymityRevokers :: [Int]
    , threshold :: Int
    }
  deriving (Generic, Show, ToJSON)


data Attributes =
  Attributes
    { chosenAttributes :: Map Text Text
    , expiryDate :: Int
    }
  deriving (Generic, Show, ToJSON, FromJSON)


sampleIdObjectRequest :: IdObjectRequest
sampleIdObjectRequest =
  IdObjectRequest
    { ipIdentity = 0
    , name = "Ales"
    , attributes =
        Attributes
          { chosenAttributes =
              fromList
                [ ("DateOfBirth", "1234")
                , ("MaxAccount", "30")
                , ("CountryOfResidence", "386")
                ]
          , expiryDate = 1893456000 -- 2030/01/01
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
    { accountHolderName :: Text
    , idCredPub :: Text
    , idCredSecCommitment :: Text
    , ipArData :: [IpArData]
    , choiceArData :: ChoiceArData
    , pokSecCred :: Text
    , sndPokSecCred :: Text
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
    , name :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

-- Identical to IdObjectResponse with addition of revealedItems and accountNumber fields.
data IdCredentialRequest =
  IdCredentialRequest
    { ipIdentity :: Int
    , identityObject :: IdentityObject
    , idUseData :: IdUseData
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
