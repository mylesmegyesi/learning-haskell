module CIS194.HW01Spec where

import Test.Hspec (describe, it, shouldBe)
import CIS194.HW01

spec =
  describe "CIS 194 Homework 1" $ do

    describe "Exercise 1" $ do

      describe "lastDigit" $ do
        it "returns the last (base 10) digit of a number" $ do
          lastDigit 123 `shouldBe` 3

        it "just returns the number if the number only has one (base 10) digit" $ do
          lastDigit 0 `shouldBe` 0

      describe "dropLastDigit" $ do
        it "returns the the number with last (base 10) digit removed" $ do
          dropLastDigit 123 `shouldBe` 12

        it "just returns the number if the number only has one (base 10) digit" $ do
          dropLastDigit 0 `shouldBe` 0
