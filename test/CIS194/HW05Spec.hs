module CIS194.HW05Spec where

import Test.Hspec
    ( Spec

    , describe
    , it
    , shouldBe
    )
import CIS194.HW05
    ( Mod5(MkMod)
    )
import CIS194.Ring
    ( addId
    , parse
    )
import CIS194.Parser
    ( parseRing
    )

spec :: Spec
spec =
  describe "CIS194 Homework 5" $ do
    describe "Exercise 1" $ do
      it "parses a literal Integer" $ do
        parse "3" `shouldBe` Just (3 :: Integer, "")

      it "parses an Integer expression" $ do
        parseRing "1 + 2 * 5" `shouldBe` Just (11 :: Integer)

      it "has addative identity for Integer" $ do
        addId `shouldBe` (0 :: Integer)

    describe "Exercise 2" $ do
      it "parses a literal" $ do
        parse "3" `shouldBe` Just (MkMod 3, "")

      it "parses a + Ring" $ do
        parseRing "3 + 4" `shouldBe` Just (MkMod 2)

      it "wraps a + operation" $ do
        parseRing "3 + 4" `shouldBe` Just (MkMod 2)

      it "wraps a + operation to 0" $ do
        parseRing "1 + 4" `shouldBe` Just (MkMod 0)

      it "has addative inverse" $ do
        addId `shouldBe` (MkMod 0)

      it "parses an experssion with multiple operations" $ do
        parseRing "1 + 2 * 5" `shouldBe` Just (MkMod 1)
