{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module SimpleIdClientMock where

import Data.Text                     (Text)
import Data.Aeson.Types              (ToJSON, FromJSON)
import qualified Data.Text as T
import NeatInterpolation
import Servant.API.Generic

{-

These are mock implementations of a possible API to simple_id_client mocked out
until we have an agreed approach for subsuming crypto APIs into the middleware.

See https://gitlab.com/Concordium/notes-wiki/wikis/ID-interactions
and https://docs.google.com/drawings/d/1adf5KVgqxgKs_9ptKj4UaZg2K3EzPuZpvMAivz3-Mww/edit

-}


-- Generate Credential Holder Information (CHI)
-- Equivalent:
-- simple_id_client start-ip --chi mario-chi.json --private mario-aci.json --public mario-pio.json
createChi :: Text -> IO Text
createChi name =
  -- @TODO inject the name when we start using proper values everywhere
  pure [text|
    {
      "idCredPublic": "81e4ebdd9d840ca076243f47415b55e955e8f92a63fcfe3e0d10c2a5366c7013255cf2bc0b7f24d83c14ce4dd64a82cc",
      "idCredPublicIP": "81e4ebdd9d840ca076243f47415b55e955e8f92a63fcfe3e0d10c2a5366c7013255cf2bc0b7f24d83c14ce4dd64a82cc",
      "idCredSecret": "3eee83ca68b98150eac930ddd539b06c3c13028c3c716491d157013c43fa6b21",
      "name": "Mario"
    }
  |]


data IdAttributesScheme = V1 Text Text Text  | V2 deriving (Generic, Show, FromJSON)


-- Generate Account Creation Information (ACI) and Pre-Identity Object (PIO)
-- Equivalent:
-- simple_id_client start-ip --chi mario-chi.json --private mario-aci.json --public mario-pio.json
createAciPio :: IdAttributesScheme -> Text -> IO (Text,Text)
createAciPio scheme chi = do
  let
    aci =
      [text|
        {
          "ACI": {
            "attributes": {
              "age": 30,
              "expiryDate": 1911686399,
              "maxAccount": 10,
              "variant": 0
            },
            "credentialHolderInformation": {
              "idCredPublic": "81e4ebdd9d840ca076243f47415b55e955e8f92a63fcfe3e0d10c2a5366c7013255cf2bc0b7f24d83c14ce4dd64a82cc",
              "idCredPublicIP": "81e4ebdd9d840ca076243f47415b55e955e8f92a63fcfe3e0d10c2a5366c7013255cf2bc0b7f24d83c14ce4dd64a82cc",
              "idCredSecret": "3eee83ca68b98150eac930ddd539b06c3c13028c3c716491d157013c43fa6b21",
              "name": "Mario"
            },
            "prfKey": "37acb874c4b3e8c7c8f378ee7b11b7d21a2f1a6fa590ea0b5af69a1a9438d0de"
          },
          "randomness": "0c7277a97a22dbed51ef61ed007150be54382def92f1a80a0a307e449744cfb3"
        }
      |]

    pio =
      [text|
        {
          "accountHolderName": "Mario",
          "attributeList": {
            "age": 30,
            "expiryDate": 1911686399,
            "maxAccount": 10,
            "variant": 0
          },
          "idCredPubIp": "81e4ebdd9d840ca076243f47415b55e955e8f92a63fcfe3e0d10c2a5366c7013255cf2bc0b7f24d83c14ce4dd64a82cc",
          "idCredSecCommitment": "b8ab69bfa03b4f3ccabbec2153a9f5b7e88ba914bfabae0bfaacbc09eecc1078656c396238e82d4aca14c0b6c7b40bdf",
          "ipArData": {
            "arName": "anonymity_revoker-5",
            "prfKeyEncryption": "8f3452fb54b2db5e79d8743547f7eb90176426475c6b6eefe5cfa9e975e9153ba04e542c1d2eaab6663777dd3774a8eba5bf6f148babbf2c2aba46f066ef1e71a6da177519175315a5a00975f06b7162b6b8a2f5144859fcdfc7db7361f1d136"
          },
          "pokSecCred": "5a14c16ecce7095a7357df591b96d1d7a05ef4d997bdde5c6fd12b8faca46be900000001b89fae3b3eff4cfed7d1ffbbdf5ceb23c4075f70edb171fc982a4212253450831ed41ecf7eacb6e3b0b0e2716114fad283ab10ee1e1e43e7b8981885ed5782eb155657343f431e24d0505fec93954ae1d1ddda1377baf63caa6a9a2929a9a166000000015751d23c1ee8738409108790065fca03b10ae852ac62e30e483214e971b3970400000001201faa1359c4d9daa936607e63082b963df7b5f111fe2e11e13806e70fc62af1",
          "prfKeyCommitmentWithAR": "8b7e2c242d2be08fef8e9f41431c1f73c9dbe7665201341ff12e50977d1ac218cbeb970198ce1f9c1e13c77687c96ddb",
          "prfKeyCommitmentWithID": "832ff7c477e083ad58a06b23a10afbd0c96594b0d145e3385981550feb5ebca10190eac136f699e0dfc409a38bd0e78c",
          "proofCommitmentsSame": "164dadb163842732f6db681373a80367edd2d7b9b82a11a7749f4f46b8ee9555a3b6bc80e7a432a9158476e2fb35510f0ab1654d4ed55f4eb7f0d4ad2410c2223cbd01b409be7d42a147263296134f0b84f69536172470964bb9e7157ba37bcc96c51d873801d0673ea8b575d1396a2ea48baca959a8f07d1f4ca51ccdc3728e691a5c0e137d30d0750e5c0ca5a2da958d852610d2805b08d0e0c22e0cc6259615f0ba08e66802bcd496cbfc55f481bf79eb15f087df86712d5e8fd5b7e39da01b84d842d9b56668e214d3050c2b696d507fbdc6c9b488459b2daad08550bf05",
          "proofEncryptionPrf": "5e2f9fcce5020f673e59245e0a68ff9b58b89c5617b88f5c74d492d8bae7cb46b0e5058a12849d24afa72a0591a7bea84080b4e8fcbe1531073ab012c0fb7a2846f27897d0318a6bb88f4d6ded758c50b4b90cfef3d2caaa994eb54d865b4fd80e25bdfcda54361a1e3452ae1ac3f61f0d7fc3bda5e126205f97e88d723b09ad8c307165630592912f439e9a1ba05918e199575678632b902e71206416a347ea2cd3c4cbf9c8c5a8c06c413141e54cf415259ef961fae4d672d81b2b597ca6d5c5dc9ad12f27e73459ff41d13f192c262971420afd8e6c94134186f9b4bc25a72598af3f7eb000e7dd9ddda64989831a470a46da3d0ba9e1f3ce3a1c46dd7150961e7ce39cdc2a3941f91efaece345b0"
        }
      |]

  pure (aci, pio)


-- simple_id_client ip-sign-pio --pio mario-pio.json --ip-data database/identity_provider-5.json --out mario-spio.json
signPio :: Text -> Text -> IO Text
signPio pio identityProviderId =
  -- identityProvider <- providerByKey identityProviderId
  pure [text|
    {
      "ipInfo": {
        "arElgamalGenerator": "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb",
        "arName": "anonymity_revoker-5",
        "arPublicKey": "b22fa02ecffcc731160a5d1892651aa0aeca76f7cf227fb8b749c8389d09ff67fdccadd1ca67137df93200323a620db7",
        "ipIdentity": "identity_provider-5",
        "ipVerifyKey": "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb80000000aa4927c0af7af0767f70ba45eaa20524e58a0d6ea83668e0c9a31d6ac42f209b50f2d81533fc14ce749413825a67893a096789bb6e66a689bb69187cb5cc2ce606ce526b3492660b55865bbecf4afed7a8a08b7a600a3576cabcad811f7e75e83af4b221f718beb7cff52dc998a6b675f3bb7f1c7eb26721351383481fcd66283b4fcb951439708b2e42d1c8d485e2c32a5a817568f7d280bc79800bc69d383148bccc2d55bccc16526d5e19903bd09e066928b5bc10db7cd6f8e330c58f8ec86b2ad28601c23a948815057552d364fca2cb5394831b17a88315fd3dd142849b20b58082263f0ca03298444bce59baff9805efb703330c75ed6817c1788e7650322a9d6de33c5f1943d7cf4b13b24b2ba21234ae1594469f96a3d15c34cfc962d8ce0985ac6b864a3c8f71f7221f0abee208994e4da4773460e2d7a22fa2cc520501f4e8e7c20387867e898b185e7c89c9120d8eb7bc7401d284028de1988f24976f15098e60dca68a72d488ececa9d5d94a063239bafbfb526ab1fd2a81bf5bb853754ace403d6d7640f3be210409ce6c08f991c4f82bcc0943ee3bb3a46bd099f3afe17ba1fdf27a6f77f6134b3676a9750de4609d715db6961229f8c84a5fb945edcaf91242c8a27c16011e445e112521a1ef6bb359daaf69aa8c4236f915f0000000a8ba497730c47a93c130018b2e72c4771b807b48ed63415f4b289be2ba9ab5e52e4895a155733a82af3559fc90f6dadc21722887aeb69655db3a4ae5ad729556d1faa65a7fdaa724db0fc383e759e6d4aecfb645cd4b775d9d61eda1d389a620da25a3aa3f9efac361355f4b406542c674d930359442850922c8a63a1574a117e97c6e77e64436977f2c18a3d255c40820c318816d1cb974308e70077fdfaf40912b07e6f32f3d973d1d16bb3c3099adfdfb87806d85c4ec6b54da39d20eeeba8ac53f84412d9352aa05b523eb940a0b515391858efe568f856fd427447641af1cd659f5b15e91c4c4b80768446c5f23b1845957e07a1dacf590f6eb8ec4116bf1625fb40ea4c3fd3b65424ae0862dd2643f9d08d2e7f580f96ff0fb169174810ac2b31ad05bcfd65d1f8e1c7546dcc2bb9ceb84746afcbc399892547f125adf77115257f821ab1a11c7472bb29cf2118174f9ae6ed0bd5c89a644f5443e60c63a3de08e9701e1d68d1bd70d0f0163add7a907123febc6a922814ad220f58332ab8c8b4388daa310bdb7fe0d8f7516eef85a00b5f3edf45ffe49c0b586bc8ff7cbff2a2b3cfc2e7d1f2fec3c898881a1d190a33ab20ec77032a750cde5dad0862454f6d65a7949e88b561d14861a94839f252c0b0a99e8091f8dc71c0cca3e6058f6a8968f13a34ba8072301feb45c086ccfb1f33989cac531061e6534c583ec14aff5d7502af9f09703cbc2e3b57b66905f30d01c375ed8ae44c6ed28bfa5ab2e426cb63a6798c7641848c4118f0e86a78670197644f8aaef20e704c3c854e7aaf2b2e8a7780e91610126e29fcce6b3401de1c563b64e5ac44dcd4432c1f872d89833ded9c8e7abf2447acaa3133317f0cbbba3829fe4e4441cd576c6c68474172c446f775763a0e9cf894b026920fdd39b49a76942dbb38cb7381e73c40f0eb9128be3161137f5d85373633208b5c78a1ccbe0d587463ce8570639a61d0183f0bf48654673597b3a117cd8bbf4b77f301c569a76373e88595ea7b0422a3b6cbb8c20339cb5a67887a71b3ccec6090416b75cb8a90201d286e3409451b66e3bbb62e9c378eb09f8da4cc49e19e4d3a828cc033bd042440bb72cfbf3b1961491f91365162d75cdbc42db381adbdcadaf509ae87c5c68eec21a70ef62573fe561beaa7e0582219a570407b5fe3f0dd63d95a8577405541f5d2bb58b04ed86d07eba15c12f0ed699d3628467f33a9cda5b426370c715cbde49fd92e86494c91fd266765d2099432507a6ce36e059230a7d60407b71eda7ce929398120bcedeff9600cb9be6245a3e301bd382f774b0e631a8a92df8affe944b17c826760cdbfadb482392ca754f8c463b2e1ac741d98a08538be092b13b833914e0a37fb87548668e76bf202bfc4f197c0da3c9e3910e4a515339ea5a89a363831fe4f7982ee5b5bcebdaa0ed549915016a2c1438452d91c3719a1f157e8c8683d7508de0ba54b04"
      },
      "preIdentityObject": {
        "accountHolderName": "Mario",
        "attributeList": {
          "age": 30,
          "expiryDate": 1911686399,
          "maxAccount": 10,
          "variant": 0
        },
        "idCredPubIp": "81e4ebdd9d840ca076243f47415b55e955e8f92a63fcfe3e0d10c2a5366c7013255cf2bc0b7f24d83c14ce4dd64a82cc",
        "idCredSecCommitment": "b8ab69bfa03b4f3ccabbec2153a9f5b7e88ba914bfabae0bfaacbc09eecc1078656c396238e82d4aca14c0b6c7b40bdf",
        "ipArData": {
          "arName": "anonymity_revoker-5",
          "prfKeyEncryption": "8f3452fb54b2db5e79d8743547f7eb90176426475c6b6eefe5cfa9e975e9153ba04e542c1d2eaab6663777dd3774a8eba5bf6f148babbf2c2aba46f066ef1e71a6da177519175315a5a00975f06b7162b6b8a2f5144859fcdfc7db7361f1d136"
        },
        "pokSecCred": "5a14c16ecce7095a7357df591b96d1d7a05ef4d997bdde5c6fd12b8faca46be900000001b89fae3b3eff4cfed7d1ffbbdf5ceb23c4075f70edb171fc982a4212253450831ed41ecf7eacb6e3b0b0e2716114fad283ab10ee1e1e43e7b8981885ed5782eb155657343f431e24d0505fec93954ae1d1ddda1377baf63caa6a9a2929a9a166000000015751d23c1ee8738409108790065fca03b10ae852ac62e30e483214e971b3970400000001201faa1359c4d9daa936607e63082b963df7b5f111fe2e11e13806e70fc62af1",
        "prfKeyCommitmentWithAR": "8b7e2c242d2be08fef8e9f41431c1f73c9dbe7665201341ff12e50977d1ac218cbeb970198ce1f9c1e13c77687c96ddb",
        "prfKeyCommitmentWithID": "832ff7c477e083ad58a06b23a10afbd0c96594b0d145e3385981550feb5ebca10190eac136f699e0dfc409a38bd0e78c",
        "proofCommitmentsSame": "164dadb163842732f6db681373a80367edd2d7b9b82a11a7749f4f46b8ee9555a3b6bc80e7a432a9158476e2fb35510f0ab1654d4ed55f4eb7f0d4ad2410c2223cbd01b409be7d42a147263296134f0b84f69536172470964bb9e7157ba37bcc96c51d873801d0673ea8b575d1396a2ea48baca959a8f07d1f4ca51ccdc3728e691a5c0e137d30d0750e5c0ca5a2da958d852610d2805b08d0e0c22e0cc6259615f0ba08e66802bcd496cbfc55f481bf79eb15f087df86712d5e8fd5b7e39da01b84d842d9b56668e214d3050c2b696d507fbdc6c9b488459b2daad08550bf05",
        "proofEncryptionPrf": "5e2f9fcce5020f673e59245e0a68ff9b58b89c5617b88f5c74d492d8bae7cb46b0e5058a12849d24afa72a0591a7bea84080b4e8fcbe1531073ab012c0fb7a2846f27897d0318a6bb88f4d6ded758c50b4b90cfef3d2caaa994eb54d865b4fd80e25bdfcda54361a1e3452ae1ac3f61f0d7fc3bda5e126205f97e88d723b09ad8c307165630592912f439e9a1ba05918e199575678632b902e71206416a347ea2cd3c4cbf9c8c5a8c06c413141e54cf415259ef961fae4d672d81b2b597ca6d5c5dc9ad12f27e73459ff41d13f192c262971420afd8e6c94134186f9b4bc25a72598af3f7eb000e7dd9ddda64989831a470a46da3d0ba9e1f3ce3a1c46dd7150961e7ce39cdc2a3941f91efaece345b0"
      },
      "signature": "868eb423cf15c89eaf8388c5a9ceddcc3bae02ce74dc96e11510f5671392f6f5ff195ec97807fe1cb01809c88272234498d7ea7bae75d3b1c46b4210262ee81c5b38f56ed1dcf44cead22ab98058573dfeb2d81dbe44149f1247aad9df8882b4"
    }
  |]


data AccountTransaction =
  AccountTransaction
    { transactionId :: Text
    , transactionType :: TransactionType
    , amount :: Int
    , fromAddress :: Text
    , toAddress :: Text
    , blockNumber :: Int
    , slotNumber :: Int
    }
  deriving (ToJSON, Generic, Show)


data TransactionType
  = Sent
  | Received
  | Shielded
  | Unshielded
  deriving (ToJSON, Generic, Show)


accountTransactions :: Text -> [AccountTransaction]
accountTransactions address =
  [ AccountTransaction { transactionId = "abcd", transactionType = Received, amount = 123, fromAddress = "x", toAddress = "y", blockNumber = 123, slotNumber = 321 }
  , AccountTransaction { transactionId = "efgh", transactionType = Sent, amount = 123, fromAddress = "x", toAddress = "y", blockNumber = 123, slotNumber = 321 }
  , AccountTransaction { transactionId = "ijkl", transactionType = Shielded, amount = 123, fromAddress = "x", toAddress = "y", blockNumber = 123, slotNumber = 321 }
  , AccountTransaction { transactionId = "mnop", transactionType = Unshielded, amount = 123, fromAddress = "x", toAddress = "y", blockNumber = 123, slotNumber = 321 }
  ]
