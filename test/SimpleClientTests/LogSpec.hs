module SimpleClientTests.LogSpec where

import Concordium.Client.Cli

import Test.Hspec

logSpec :: Spec
logSpec = describe "log" $ do
    prettyMsgSpec

prettyMsgSpec :: Spec
prettyMsgSpec = describe "prettyMsg" $ do
    specify "empty without punctuation" $
        prettyMsg "" "" `shouldBe` ""
    specify "empty with punctuation" $
        prettyMsg "" "" `shouldBe` ""
    specify "non-empty without punctuation" $
        prettyMsg "" "sentence" `shouldBe` "Sentence"
    specify "non-empty with punctuation" $
        prettyMsg "." "sentence" `shouldBe` "Sentence."
