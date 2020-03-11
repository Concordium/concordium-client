module SimpleClientTests.LogSpec where

import Concordium.Client.Cli

import Test.Hspec

logSpec :: Spec
logSpec = describe "log" $ do
  prettyMsgSpec
  prettyLinesSpec

prettyMsgSpec :: Spec
prettyMsgSpec = describe "prettyMsg" $ do
  specify "empty" $
    prettyMsg "" `shouldBe` ""
  specify "non-empty" $
    prettyMsg "sentence" `shouldBe` "Sentence."

prettyLinesSpec :: Spec
prettyLinesSpec = describe "prettyLinesSpec" $ do
  specify "empty" $
    prettyLines 1 [] `shouldBe` []
  specify "single message not fitting line" $
    prettyLines 2 ["msg"] `shouldBe` ["msg"]
  specify "single message fitting line" $
    prettyLines 3 ["msg"] `shouldBe` ["msg"]
  specify "two messages not fitting line" $
    prettyLines 8 ["msg1", "msg2"] `shouldBe` ["msg1", "msg2"]
  specify "two messages fitting one line" $
    prettyLines 9 ["msg1", "msg2"] `shouldBe` ["msg1 msg2"]
  specify "two messages fitting line followed by one not fitting line" $
    prettyLines 13 ["msg1", "msg2", "msg3"]`shouldBe` ["msg1 msg2", "msg3"]
  specify "three messages fitting line" $
    prettyLines 14 ["msg1", "msg2", "msg3"]`shouldBe` ["msg1 msg2 msg3"]
  specify "one long message followed by two fitting line" $
    prettyLines 9 ["message", "msg1", "msg2"] `shouldBe` ["message", "msg1 msg2"]
  specify "one long message followed by two fitting line followed by one more long" $
    prettyLines 9 ["message", "msg1", "msg2", "message"] `shouldBe` ["message", "msg1 msg2", "message"]
  specify "four messages fitting two and two" $
    prettyLines 12 ["message", "msg1", "msg2", "message"] `shouldBe` ["message msg1", "msg2 message"]
