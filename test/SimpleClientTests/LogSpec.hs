module SimpleClientTests.LogSpec where

import Concordium.Client.Cli

import Test.Hspec

logSpec :: Spec
logSpec = describe "log" $ do
  prettyMsgSpec

prettyMsgSpec :: Spec
prettyMsgSpec = describe "prettyMsg" $ do
  specify "empty" $
    prettyMsg "" `shouldBe` ""
  specify "non-empty" $
    prettyMsg "sentence" `shouldBe` "Sentence."
