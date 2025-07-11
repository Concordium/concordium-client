module SimpleClientTests.TokenAmountSpec where

import Test.Hspec

import Concordium.Types.Tokens

import Concordium.Client.Utils

tokenAmountSpec :: Spec
tokenAmountSpec = describe "TokenAmount" $ do
    it "Parse 123456" $ preTokenAmountFromString "123456" `shouldBe` Just (PreTokenAmount 123456 0)
    it "Parse 123456.789" $ preTokenAmountFromString "123456.789" `shouldBe` Just (PreTokenAmount 123456789 3)
    it "Parse 123.000" $ preTokenAmountFromString "123.000" `shouldBe` Just (PreTokenAmount 123000 3)
    it "Parse 0.002" $ preTokenAmountFromString "0.002" `shouldBe` Just (PreTokenAmount 2 3)
    it "Parse .02" $ preTokenAmountFromString ".02" `shouldBe` Just (PreTokenAmount 2 2)
    it "Parse 18446744073709551615" $
        preTokenAmountFromString "18446744073709551615" `shouldBe` Just (PreTokenAmount 18446744073709551615 0)
    it "Parse 18446744073709551616" $ preTokenAmountFromString "18446744073709551616" `shouldBe` Nothing
    it "Parse 0.0[*254]1" $ preTokenAmountFromString ("0." ++ replicate 254 '0' ++ "1") `shouldBe` Just (PreTokenAmount 1 255)
    it "Parse 0.0[*255]1" $ preTokenAmountFromString ("0." ++ replicate 255 '0' ++ "1") `shouldBe` Nothing

    it "Normalize 123456 -> 3" $
        normalizeTokenAmount (TokenId "test-token") 3 (PreTokenAmount 123456 0)
            `shouldBe` Right (TokenAmount (TokenRawAmount 123456000) 3)
    it "Normalize 123456.789 -> 3" $
        normalizeTokenAmount (TokenId "test-token") 3 (PreTokenAmount 123456789 3)
            `shouldBe` Right (TokenAmount (TokenRawAmount 123456789) 3)
    it "Normalize 123.000 -> 0" $
        normalizeTokenAmount (TokenId "test-token") 0 (PreTokenAmount 123000 3)
            `shouldBe` Left "Token amount 123.000 has 3 decimals, but test-token only supports 0."
    it "1844674407370955162 -> 1" $
        normalizeTokenAmount (TokenId "test-token") 1 (PreTokenAmount 1844674407370955162 0)
            `shouldBe` Left "Token amount 1844674407370955162 exceeds the maximum representable token amount for test-token."
