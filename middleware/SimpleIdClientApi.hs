{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SimpleIdClientApi where

import Data.Text                 (Text)
import Data.Aeson.Types          (ToJSON, FromJSON)
import Data.Aeson                (eitherDecode)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Servant.API.Generic
import Data.Map
import qualified Data.Text as Text

import Concordium.Crypto.SignatureScheme (SignKey (..), VerifyKey (..))


-- API requests


-- getIdentityProviders :: IO [IdentityProvider]
-- getIdentityProviders =
--     getJsonRequest "localhost:8000/ips"


postIdObjectRequest :: Text -> IdObjectRequest -> IO IdObjectResponse
postIdObjectRequest idUrl idObjectRequest = do
    postJsonRequest (Text.unpack idUrl ++ "/identity_object") idObjectRequest


postIdCredentialRequest :: Text -> IdCredentialRequest -> IO IdCredentialResponse
postIdCredentialRequest idUrl idCredentialRequest = do
    postJsonRequest (Text.unpack idUrl ++ "/generate_credential") idCredentialRequest


-- API Helpers

getJsonRequest ::  (FromJSON response) => String -> IO response
getJsonRequest url = do
  req <- setHeaders <$> parseUrlThrow url
  jsonRequest req


postJsonRequest ::  (ToJSON request, FromJSON response) => String -> request -> IO response
postJsonRequest url requestObject = do
  req <- setRequestBodyJSON requestObject . setHeaders . setPost <$> parseUrlThrow url
  jsonRequest req


jsonRequest :: FromJSON response => Request -> IO response
jsonRequest req = do
  manager <- newManager tlsManagerSettings -- @TODO put the tlsManger into reader
  res     <- Network.HTTP.Conduit.httpLbs req manager

  case eitherDecode (responseBody res) of
    Left  err -> do
      putStrLn "There was an error decoding this response:"
      putStrLn $ show $ responseBody res
      error err
    Right result -> pure result


setHeaders :: Request -> Request
setHeaders h = h { requestHeaders = [("Content-Type", "application/json")] }


setPost :: Request -> Request
setPost h = h { method = "POST" }


-- Data types

data IdObjectRequest =
  IdObjectRequest
    { ipIdentity :: Int
    , name :: Text
    , attributes :: Map Text Text
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
    , idCredSecCommitment :: Text
    , ipArData :: IpArData
    , pokSecCred :: Text
    , prfKeyCommitmentWithAR :: Text
    , prfKeyCommitmentWithID :: Text
    , proofCommitmentsSame :: Text
    , proofEncryptionPrf :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IpArData =
  IpArData
    { arDescription :: Text
    , arIdentity :: Int
    , prfKeyEncryption :: Text
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
    , idCredPublicIP :: Text
    , idCredSecret :: Text
    , name :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

-- {
--     "ipIdentity": 0,
--     "preIdentityObject": {
--         "accountHolderName": "Aleš",
--         "attributeList": {
--             "age": 31,
--             "business": true,
--             "citizenship": 38,
--             "expiryDate": 1910822399,
--             "maxAccount": 200,
--             "variant": 1
--         },
--         "idCredPubIp": "929b9f83a095f9c65e5b90a1da71ba1643aa52294b2df48671a82b126fb75dddf1661759e68069b7a87cd19347be627c",
--         "idCredSecCommitment": "9215eec41a14350ed9eae438984c2f719c9f5fe12d4d4301e63e791f6994163a4c7046c265635cf9c411429d52c2402d",
--         "ipArData": {
--             "arDescription": "anonymity_revoker-0",
--             "arIdentity": 0,
--             "prfKeyEncryption": "ab486d46e24db272839683fd13ef9041148af7b7fc64d1a37781b03ac7a7c9563b951399e6a1052fd046d67773a2349098a2f45c4bf18dc4eec64fe40ee831d919b47a5558796b24b5a2431fbe892202df82646e6717699a712671ef980f36bf"
--         },
--         "pokSecCred": "63994139028fd5843cfbae94fcf6cf97141faf9ccb6f0302ae3a7e59c15818bb00000001a4cfb12d6d33908d2690215294f01d919f67723cfeb0cc6ae28fa893e52fccfc9c95c05f98a7a890f9d088e3d6ff80e9b38e7b8c9d888c9d5b88c27db1f7ed53d03991a28aff6ce2bf9369b606aef78151c5b1f5726450479ba4bedd690aa70d000000010f7d2404e0d58b9fed2075609d679abdda7409419c273aac5c3738132debd57d00000001185f717fd80205ba4da4ae57fdc22e382cc1508ef3fcdf9a4eb94d8e72023a66",
--         "prfKeyCommitmentWithAR": "af21201056c7e287357f133653baf86c03bd463d29da81a1bad639ee1cd24e0e2ce6f36ae8599d8ab187ad02e864751a",
--         "prfKeyCommitmentWithID": "ad0ee0900b20a2e29d282e30e1d349be944f53e56235d8579b37b39ed765ef8ffd37a6d184610072796fb9f767b6e696",
--         "proofCommitmentsSame": "615434a03267e06eb01fab70f593227765fb394d3e7b8f7cfee4574eac3aaebaae7d9aab0bfc33aead60a258e30c7ce73de77337749f2c294aa3e2eb82ecd263dec0cefd0bbfed92ba82f17092acf18fa1c0aeff1479f65e0dac9f9fe99faefc0d8e9bf24ff2f5923795ed6496f2821cbcce5b5fb1dd46cff10f1066a746ad6c189f31d99f21edf69065295812bbd2634d382e3d04599c202cff80d4b76ee7716d6dfa0de8727e548d1b2df8750f96e46abe4ffe3879a2b91d62a9c873e4e0781c51a11413435885179bcd3b268f890dfa86dc246bd3d3054933540f44b9b14a",
--         "proofEncryptionPrf": "32898c955f2f46dcecf0b9074548fa272e60f799eb8f465af23c11b0c723031eaab136c8ed896d0b0a6dd7f92f80bda3f1c3caf712ec83d60dc7640bea5ea47787461d576d8bdedf8265fbba0fae2c2a85a865a9331e8df128c620e3fc04f9690afa38b4f29cee322221b408e1e58dd89882d4b71dc42ed7d51661330da2213699c8d5408faf710b2273584f88f5ced4d4eee5e2a10af85b2ad469f3ba2711130abc379d21709606ac4cee2ce5b59e2325510828277c62d81e0a57a0edddbff409129ac06980cc4bf28d2b3387b064e01a751cbf1bc613231488fc17ec2a6411c96f059a6e33d03ce6d756f158a7bd1e25d91e6cbe1d5e4057500fdecbaba773cfc40b5562cae78d022793277a22d4cd"
--     },
--     "privateData": {
--         "ACI": {
--             "attributes": {
--                 "age": 31,
--                 "business": true,
--                 "citizenship": 38,
--                 "expiryDate": 1910822399,
--                 "maxAccount": 200,
--                 "variant": 1
--             },
--             "credentialHolderInformation": {
--                 "idCredPublic": "929b9f83a095f9c65e5b90a1da71ba1643aa52294b2df48671a82b126fb75dddf1661759e68069b7a87cd19347be627c",
--                 "idCredPublicIP": "929b9f83a095f9c65e5b90a1da71ba1643aa52294b2df48671a82b126fb75dddf1661759e68069b7a87cd19347be627c",
--                 "idCredSecret": "0cb96a1746ce08db10a240b416ec749c408d52aedd3eae8c7bb93fa2b754c095",
--                 "name": "Aleš"
--             },
--             "prfKey": "14010d50621a10b13e24e8ea6b19802202f9bf163bcdfa77500fc8244a8a60c6"
--         },
--         "PIORandomness": "50099c7315144100da2da8f64e53041bdccfd0a72a050a4b77ef48a958b206e5"
--     },
--     "signature": "86828e8d017f29fd7cf89756fd8db924c51d8ebe2a03a663bb3d44db7e93f0a28e86e01cdea9203b2ac77b80c9a21165aca393256cfd2938a1d6ffee241d41de2b56cd58043bbc68a88e9406e666e1f5077c24a05082dd7c14c75561de8ffc14"
-- }


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

-- {
--     "ipIdentity": 0,
--     "preIdentityObject": {
--         "accountHolderName": "Aleš",
--         "attributeList": {
--             "age": 31,
--             "business": true,
--             "citizenship": 38,
--             "expiryDate": 1910822399,
--             "maxAccount": 200,
--             "variant": 1
--         },
--         "idCredPubIp": "929b9f83a095f9c65e5b90a1da71ba1643aa52294b2df48671a82b126fb75dddf1661759e68069b7a87cd19347be627c",
--         "idCredSecCommitment": "9215eec41a14350ed9eae438984c2f719c9f5fe12d4d4301e63e791f6994163a4c7046c265635cf9c411429d52c2402d",
--         "ipArData": {
--             "arDescription": "anonymity_revoker-0",
--             "arIdentity": 0,
--             "prfKeyEncryption": "ab486d46e24db272839683fd13ef9041148af7b7fc64d1a37781b03ac7a7c9563b951399e6a1052fd046d67773a2349098a2f45c4bf18dc4eec64fe40ee831d919b47a5558796b24b5a2431fbe892202df82646e6717699a712671ef980f36bf"
--         },
--         "pokSecCred": "63994139028fd5843cfbae94fcf6cf97141faf9ccb6f0302ae3a7e59c15818bb00000001a4cfb12d6d33908d2690215294f01d919f67723cfeb0cc6ae28fa893e52fccfc9c95c05f98a7a890f9d088e3d6ff80e9b38e7b8c9d888c9d5b88c27db1f7ed53d03991a28aff6ce2bf9369b606aef78151c5b1f5726450479ba4bedd690aa70d000000010f7d2404e0d58b9fed2075609d679abdda7409419c273aac5c3738132debd57d00000001185f717fd80205ba4da4ae57fdc22e382cc1508ef3fcdf9a4eb94d8e72023a66",
--         "prfKeyCommitmentWithAR": "af21201056c7e287357f133653baf86c03bd463d29da81a1bad639ee1cd24e0e2ce6f36ae8599d8ab187ad02e864751a",
--         "prfKeyCommitmentWithID": "ad0ee0900b20a2e29d282e30e1d349be944f53e56235d8579b37b39ed765ef8ffd37a6d184610072796fb9f767b6e696",
--         "proofCommitmentsSame": "615434a03267e06eb01fab70f593227765fb394d3e7b8f7cfee4574eac3aaebaae7d9aab0bfc33aead60a258e30c7ce73de77337749f2c294aa3e2eb82ecd263dec0cefd0bbfed92ba82f17092acf18fa1c0aeff1479f65e0dac9f9fe99faefc0d8e9bf24ff2f5923795ed6496f2821cbcce5b5fb1dd46cff10f1066a746ad6c189f31d99f21edf69065295812bbd2634d382e3d04599c202cff80d4b76ee7716d6dfa0de8727e548d1b2df8750f96e46abe4ffe3879a2b91d62a9c873e4e0781c51a11413435885179bcd3b268f890dfa86dc246bd3d3054933540f44b9b14a",
--         "proofEncryptionPrf": "32898c955f2f46dcecf0b9074548fa272e60f799eb8f465af23c11b0c723031eaab136c8ed896d0b0a6dd7f92f80bda3f1c3caf712ec83d60dc7640bea5ea47787461d576d8bdedf8265fbba0fae2c2a85a865a9331e8df128c620e3fc04f9690afa38b4f29cee322221b408e1e58dd89882d4b71dc42ed7d51661330da2213699c8d5408faf710b2273584f88f5ced4d4eee5e2a10af85b2ad469f3ba2711130abc379d21709606ac4cee2ce5b59e2325510828277c62d81e0a57a0edddbff409129ac06980cc4bf28d2b3387b064e01a751cbf1bc613231488fc17ec2a6411c96f059a6e33d03ce6d756f158a7bd1e25d91e6cbe1d5e4057500fdecbaba773cfc40b5562cae78d022793277a22d4cd"
--     },
--     "privateData": {
--         "ACI": {
--             "attributes": {
--                 "age": 31,
--                 "business": true,
--                 "citizenship": 38,
--                 "expiryDate": 1910822399,
--                 "maxAccount": 200,
--                 "variant": 1
--             },
--             "credentialHolderInformation": {
--                 "idCredPublic": "929b9f83a095f9c65e5b90a1da71ba1643aa52294b2df48671a82b126fb75dddf1661759e68069b7a87cd19347be627c",
--                 "idCredPublicIP": "929b9f83a095f9c65e5b90a1da71ba1643aa52294b2df48671a82b126fb75dddf1661759e68069b7a87cd19347be627c",
--                 "idCredSecret": "0cb96a1746ce08db10a240b416ec749c408d52aedd3eae8c7bb93fa2b754c095",
--                 "name": "Aleš"
--             },
--             "prfKey": "14010d50621a10b13e24e8ea6b19802202f9bf163bcdfa77500fc8244a8a60c6"
--         },
--         "PIORandomness": "50099c7315144100da2da8f64e53041bdccfd0a72a050a4b77ef48a958b206e5"
--     },
--     "signature": "86828e8d017f29fd7cf89756fd8db924c51d8ebe2a03a663bb3d44db7e93f0a28e86e01cdea9203b2ac77b80c9a21165aca393256cfd2938a1d6ffee241d41de2b56cd58043bbc68a88e9406e666e1f5077c24a05082dd7c14c75561de8ffc14",
--
--     "revealedItems": ["age", "business"],
--     "accountNumber": 3
-- }



data IdCredentialResponse =
  IdCredentialResponse
    { accountKeyPair :: AccountKeyPair
    , credential :: IdCredential
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data AccountKeyPair =
  AccountKeyPair
    { signKey :: SignKey
    , verifyKey :: VerifyKey
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdCredential =
  IdCredential
    { arData:: IdCredArData
    , ipIdentity :: Int
    , policy :: IdCredPolicy
    , proofs :: Text
    , regId :: Text
    , schemeId :: Text
    , verifyKey :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdCredArData =
  IdCredArData
    { arIdentity :: Int
    , idCredPubEnc :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdCredPolicy =
  IdCredPolicy
    { expiry :: Int
    , revealedItems :: [IdCredRevealedItem]
    , variant :: Int
    }
  deriving (Generic, Show, FromJSON, ToJSON)

data IdCredRevealedItem =
  IdCredRevealedItem
    { index :: Int
    , value :: Text
    }
  deriving (Generic, Show, FromJSON, ToJSON)

-- {
--     "accountKeyPair": {
--         "signKey": "8f2c00e86615f69967a5abce5066b1c27abcf17397a6eca8faa01ac9f3b60056",
--         "verifyKey": "4b26d799f1f717ad6426bf0d127b4f90c9f468e864cde7b1eac7761c23bc5c41"
--     },
--     "credential": {
--         "arData": {
--             "arIdentity": 0,
--             "idCredPubEnc": "89845c730e826ec9d8f04561909eacd1962718879dbebc85716aec71f7013d9b7d3f4c77e730c380069dc3cd3c8090b5ab97db4b814ceb090ad16b3eca4ea8a5e76eb28c9555aeafddff86f497dfa7e26c3cc3e61b01382f3e550935d63ea8a1"
--         },
--         "ipIdentity": 0,
--         "policy": {
--             "expiry": 1910822399,
--             "revealedItems": [
--                 {
--                     "index": 1,
--                     "value": "001f"
--                 },
--                 {
--                     "index": 3,
--                     "value": "0001"
--                 }
--             ],
--             "variant": 1
--         },
--         "proofs": "a2b2dfe21bcda687e4c5c77df9612d00961929d4f9588033fad78a21545e00254b977e9de50ec04f0492ad97d07d68a3b4b5596ecfdab916efbaed8e92fc84befe6140653a34189d5deddc119c3ab4e1cae52a8e44b6737ae418e945ba212fb7945b368474a6560031e1954a861bfd593ed86a6c8faa1612d9bfb23d41305a0cdb20bb76c6cc4e30483a7cde014c8bb0aab36693ac243df9d1dcdb2f4864f168f4ac8b283276d67f346069f51a7ac64fddacfdb1f776e4e1fb4a01541216f18782f0a9e7238fc805d0b4856c17ce27edc172ab3909b644465c0da40d34fb8a0e94e58a65a1d5fcb7136c3f84b7ffa60600068c3e059b1aab519335c93a5679ee1c4a66ac75f8cf7e551f8528a075d0db29db55f2d57bfa58a828c92a34e5a3144606aca60d0cd82be5080a6032fdf9a3546efa98322135cb35ae085e1af7eb8f1c195259ddbcf8fa6a6235e513e1d4896736813d587ba7c0dd46b49e1ed7534a015299ccda683a70208861a759424dcf1e4a123e18bcb0beb71d5632283b458a944da1b9280d88800e4922618582f446f03eda4fecddaff1f5422d27465e2a5ad346254d73577f17ce9592bf13189141a60f85b12387ca7db0098d2df775e60202ae5f821d525151cf35bd86234d21f3983748a17fc753b43e0c2e6e3cc43b486bf5b33d0bf489ced863d48c13278c3c5188939a50e674a6e9ee0eb9ece9a8a56a323d6a6da32ea935f2fdd940efb1a7585c56b6498c695e1eaef1aeef2517793de793f9f03e98268fb7a92e939a89a38966a1bc4027b94609bddfcb87ffd18c701e24ec65836d67bbf8de55d5fdef2f72faad1bdf4a84f039841e98f5a1e5091a3294b4b89a89637decb6ef09b22b261ad1738fc747040004fc424a91f0740f22e8948b8aed30bce8411f8add6ec3052d4591acbbcbaf2121056036f1582fe53b2c89f86219fcc557551a3dbe8c182814c3d258f71f1ec531420e40f07b20111dc3460c6d47f6d190d3199ee9cf0d03d162279c5e4762329631921b182460d6290851b9f9c1ff57803168ce23b6bef3eae05336ba0d37c899bb3746f0542694b91a5810190746167dc8a3341cb32f9d3aee43e9b1d570b4e867f9934b45c84538bf53a8e378fb59c2fba41b2768d0790ffe3bab8d0dec4fa9e936e15acf91c3b41c82f21945936b4a3e612842158b318406d28d41efd3a6fe62470ea2657666809129949d35080c2207b3df15f716d9d85415b38e65aee39c58bd8611b36a114b2f6477f104d3f992485e211cea80a45f468bd7af3cab6f1cae5daf4e377affff2200000008b4a941b70872761e2118635baee548bdce0f5e02dd27d0095b8ca10343372255d01969f7f145782d4470466eb71f4fe6b455fa1df3237ee5943ec787113179f921b1e1cb7ff3dd763a3951cb560ea287008526bf0b137b57dd388fa288f530bea003c9e66b9af96ea5a5ac36f68d1fee8f2d41b1009ccad1baf7d2ef2c0ad4a83b965164bbffd7bc2da0cc173a9ef729a0cf50650378c3806e5f39e81be16c5a892956bad0475bda747200ae5c7d791a73cc870912d9f205628c67f4d768c877a37a69b5d62a845669b94ec024a28d68dc268c4d8100510b1bd0abaceb19a1a9ff24d211c0784975b4f84d0b8f85acef87bbad820f3f4d81b320a2d717b75810ddfd606394d9f4ff3f38b98a4b34332c16a66659f7ab1a37d57cb53428ab6942b264fc1090d8ffbacbae8847436123aef16493d9a14c4d7f760b41ae54e498506b128d419454c7d2dc77b816bfa5dc8299d5d7b4c17ac43790fbb9cf88ab5dd718f96c5ba8e37e2fee45e62005fa2b66e5c4ab9117fcd1b6571439f0a3094c93286d6389b457a1660172501826604c031b38616667b2bc0b24c7d121a7bd39c20000000814a0f148d113bf5947abdda6b0de5932ac3b4f80b9db095d2efe658f8afb6ecf5655f2076a0adfa5eaa89ccf7fd8e8bdfcbbcb8ffd7216ee648464d0da4d3046588fdfea84306752d0fd2fdee84988cb4a7037e4762c56299acbb61fe358d2574b3b1f8941ca0072e4ffff0b4bb4069396744c5561ba5bd5c58cf949d064c5d1150a9df6cc056ab5042e30f9eda708243100e0aa49e31b10a5190a150409436d10579d5503127dcde3e0d69a278193a1e75f80b0be6390481677caa0542b008309d2ce0550ffe7acae59c544db298719219a60f04893c0f3c5e1a91344887f174983e7a5d1f389cd694cbf2a1848e45b3436eacfc619a88394a8cba64b8080ba000000086e5aa5200022db6b193240486c2bd3e5f180a0e2360cc9b42a8c33af07d4655c2a7226f4db187f2d0a0d9bf32d56ed1aa8efc9d5f5b1b0ab0c71e4514d15b0f1506c337f0fc6c0075c37bb1700bbeb28b69e850bf4daab182b5fa520cb3b0d5872210976b7401badfb31eea8a0ff19c1d9589e43bfa31a25f48e9889df875dee1cdcf3457656eaeaffdf4c00ad4270ffea6e75fa9989faa615dd92b83a262a701f723c0021b7cf1c8269117abdb47ba9b1ddbf47aeb8d31e068e52828203e3db223fc8793677681791ed862ae794838e77fc9d8bbf8db70606e0cedefd67c3f86d191a087792ad61021e6c0533b0950736533321ec06706e2cbe0984b11502ab43e740579d15bd51a8d279e1e00ca6d6734634ecc18b01b6115377a793233541b63b8427e135ca662160c44321b794fa62038f0e38ed5b761dfc82ac84a0b23ce2797e05e5866d5ed15a4f08314af078a773c245cf8f47ca11618432e78bef2aa81f7661d2e2e26ccd3d18f4edb07ed395012f463a6dc08ba16cda04279ecc31b7fe3c07bdbbdf06dcc9e8ea0e3cd3eec61e25a943914f2a00d858ef1ccca145fc95cbafc0b7758e91afec45fe91755ab3feadb739d5e972496f8b8538a9fde7893e461b19799d807da9fece54bbbf497a67cc6fd06cae82ede1a92907b602aa0ed7e0f7099dcc1391100af74f092ba3e59476470333051cefb18df93f4b4682134e32b9ae67d27b54567546175b0948d77e015a5902553862b1d89022c4eaf42c8030aa6485eb9a7837adabd290e0a2163dda5b861cc3e35550381b850c254b2a0787c63481e3643028fe93b68516f7ae881acea37be6716a1073a95cf9735640197c416e0a1f78d6bdb4bf1b7ccbdeddf0a1fcdd03b1a7aa5064a51c0a452051fb7faa5f92f2ad63923afd3536d659c431820f68cd8dea495e2f6bd00cdd9463feee35e24a807dfea5210effc875d6818172cc576c08ee78a04c38936417e73e8313a388e3b1dcbe3e85efa4790bdf5e42977fe2ee402147fb4c1424712706bc7c7e9bc562b6673727a0354626f25d070d76cc874ee8a49352c927aa7f8e3d00ac2c192d3db9bd84b1fa9027ffd28a510142a7e16e39fa5de1f495c5199e0c078c8b00c6ac0f6465fd672e6264fdaf14d7b4d67483e840f07dde10a31a08471d319655a4893421ae50e5bc14e803fffc5cd844145aa212484ea06a54b48a7e000200014092bc8dfea90a60465643eb9e9baf4197c02c0960ec3e8c761fa80a83596ebf0003552a9ccba17b5e66edea6a6b6706a359e1144e423fb0a583ddd65dda1ade137c",
--         "regId": "980c490366bf61a9efc1783861f8edca94cfeb0aa6beb79d87de1f33ba40174c3fe8294102b3b1766a5c54e231d08cde",
--         "schemeId": "Ed25519",
--         "verifyKey": "4b26d799f1f717ad6426bf0d127b4f90c9f468e864cde7b1eac7761c23bc5c41"
--     }
-- }
