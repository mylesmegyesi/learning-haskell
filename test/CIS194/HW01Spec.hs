module CIS194.HW01Spec where

import Test.Hspec (describe, it, shouldBe)
import CIS194.HW01
    ( lastDigit
    , dropLastDigit
    , toDigits
    , doubleEveryOther
    , sumDigits
    , validate
    , hanoi
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

    describe "Exercise 2" $ do
      describe "toDigits" $ do
        it "returns an list of the digits" $ do
          toDigits 1234 `shouldBe` [1,2,3,4]

        it "returns an empty list for 0" $ do
          toDigits 0 `shouldBe` []

        it "returns an empty list for a negative number" $ do
          toDigits (-17) `shouldBe` []

    describe "Exercise 3" $ do
      describe "doubleEveryOther" $ do
        it "doubles the odd indices in an even length list" $ do
          doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]

        it "doubles the even indices in an odd length list" $ do
          doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

    describe "Exercise 4" $ do
      describe "sumDigits" $ do
        it "calculates the sum of all digits" $ do
          sumDigits [16,7,12,5] `shouldBe` 22

    describe "Exercise 5" $ do
      describe "validate" $ do
        it "approves valid credit card numbers" $
          validate 4012888888881881 `shouldBe` True

        it "rejects invalid credit card numbers" $
          validate 4012888888881882 `shouldBe` False

    describe "Exercise 6" $ do
      describe "hanoi" $ do
        it "no moves for 0 blocks" $ do
          hanoi 0 "a" "b" "c" `shouldBe` []

        it "moves directly for one block" $ do
          hanoi 1 "a" "b" "c" `shouldBe` [("a","b")]
          hanoi 1 "a" "c" "b" `shouldBe` [("a","c")]
          hanoi 1 "b" "a" "c" `shouldBe` [("b","a")]
          hanoi 1 "b" "c" "a" `shouldBe` [("b","c")]
          hanoi 1 "c" "a" "b" `shouldBe` [("c","a")]
          hanoi 1 "c" "b" "a" `shouldBe` [("c","b")]

        it "moves 2 blocks from a to b" $ do
          hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]

        it "moves 3 blocks from a to b" $ do
          hanoi 3 "a" "b" "c" `shouldBe` [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]

        it "moves 15 blocks from a to b" $ do
          length (hanoi 15 "a" "b" "c") `shouldBe` 32767
