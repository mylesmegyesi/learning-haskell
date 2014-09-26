module CIS194.HW01Spec where

import Test.Hspec (describe, it, shouldBe)
import CIS194.HW01
    ( lastDigit
    , dropLastDigit
    , toDigits
    , doubleEveryOther
    , sumDigits
    , validate
    )

spec =
  describe "CIS 194 Homework 1" $ do

    describe "Exercise 1" $ do

      describe "lastDigit" $ do
        it "returns the last (base 10) digit of a number" $ do
          lastDigit 123 `shouldBe` 3
          lastDigit 10 `shouldBe` 0

        it "just returns the number if the number only has one (base 10) digit" $ do
          lastDigit 3 `shouldBe` 3
          lastDigit 0 `shouldBe` 0

      describe "dropLastDigit" $ do
        it "returns the the number with last (base 10) digit removed" $ do
          dropLastDigit 123 `shouldBe` 12
          dropLastDigit 10 `shouldBe` 1

        it "just returns 0 if the number only has one (base 10) digit" $ do
          dropLastDigit 3 `shouldBe` 0
          dropLastDigit 0 `shouldBe` 0

      describe "toDigits" $ do
        it "returns an list of the digits" $ do
          toDigits 1234 `shouldBe` [1,2,3,4]

        it "returns an empty list for 0" $ do
          toDigits 0 `shouldBe` []

        it "returns an empty list for a negative number" $ do
          toDigits (-17) `shouldBe` []

      describe "doubleEveryOther" $ do
        it "doubles the odd indices in an even length list" $ do
          doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]

        it "doubles the even indices in an odd length list" $ do
          doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

      describe "sumDigits" $ do
        it "calculates the sum of all digits" $ do
          sumDigits [16,7,12,5] `shouldBe` 22

      describe "validate" $ do
        it "approves valid credit card numbers" $
          validate 4012888888881881 `shouldBe` True

        it "rejects invalid credit card numbers" $
          validate 4012888888881882 `shouldBe` False
