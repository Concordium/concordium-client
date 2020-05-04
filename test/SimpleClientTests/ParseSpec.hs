module SimpleClientTests.ParseSpec where

import Concordium.Client.Parse
import Concordium.Types

import Test.Hspec

parseSpec :: Spec
parseSpec = describe "parse" $ do
  parseExpirySpec

parseExpirySpec :: Spec
parseExpirySpec = describe "expiry" $ do
  specify "empty" $ p "" `shouldBe` Left "non-numeric prefix"
  specify "space" $ p " " `shouldBe` Left "non-numeric prefix"
  specify "literal number" $ p "1" `shouldBe` Right 1
  specify "literal number with spaces" $ p " 2 " `shouldBe` Right 2
  specify "number of seconds" $ p "3s" `shouldBe` Right 4
  specify "number of seconds with spaces" $ p " 4 s " `shouldBe` Right 5
  specify "number of minutes" $ p "4m" `shouldBe` Right 241
  specify "number of hours" $ p "5h" `shouldBe` Right 18001
  specify "invalid suffix" $ p "7x" `shouldBe` Left "unsupported suffix 'x'"
  specify "missing number" $ p "s" `shouldBe` Left "non-numeric prefix"
  specify "missing number with spaces" $ p " s " `shouldBe` Left "non-numeric prefix"
  specify "swapped number and suffix" $ p "s7" `shouldBe` Left "non-numeric prefix"
  specify "multiple components" $ p "1m7s" `shouldBe` Left "unsupported suffix 'm7s'"
  where p = parseExpiry 1
