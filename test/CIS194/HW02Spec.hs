module CIS194.HW02Spec where

import Test.Hspec (describe, it, shouldBe)
import CIS194.HW02
    ( formableBy
    , wordsFrom
    , wordFitsTemplate
    , wordsFittingTemplate
    , scrabbleValueWord
    , bestWords
    , scrabbleValueTemplate
    )

spec =
  describe "CIS 194 Homework 2" $ do
    describe "Exercise 1" $ do
      describe "formableBy" $ do
        it "returns True if the word can be formed by the letters in the Hand" $ do
          formableBy "fun" ['x','n','i','f','u','e','l'] `shouldBe` True
          formableBy "haskell" ['k','l','e','h','a','l','s'] `shouldBe` True

        it "a letter in the Hand can only be used once" $ do
          formableBy "haskell" ['k','l','e','h','a','y','s'] `shouldBe` False

    describe "Exercise 2" $ do
      describe "wordsFrom" $ do
        it "gives a list of all valid words formable from a certain hand" $ do
          wordsFrom ['a','b','c','d'] `shouldBe` ["ab","ad","ba","bad","cab","cad","dab"]
          wordsFrom ['h','e','l','l','o'] `shouldBe` ["eh","el","ell","he","hell","hello","helo","ho","hoe","hole","lo","oe","oh","ole" ]

    describe "Exercise 3" $ do
      describe "wordFitsTemplate" $ do
        it "returns True if the given word can be formed with the given hand and fits in the template" $ do
          wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" == True

        it "returns False if the given word cannot be formed with the given hand" $ do
          wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" == True

        it "returns False if the given word can be formed but doesn't fit into the template" $ do
          wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" == False

        it "returns True if the given word equals the template" $ do
          wordFitsTemplate "let" ['x','x'] "let" == True

    describe "Exercise 4" $ do
      describe "wordsFittingTemplate" $ do
        it "returns all words that fit the template" $ do
          wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] `shouldBe` ["acre","bare","carb","care","carl","earl"]

    describe "Exercise 5" $ do
      describe "scrabbleValueWord" $ do
        it "returns the score of the given word" $ do
          scrabbleValueWord "care" `shouldBe` 6
          scrabbleValueWord "quiz" `shouldBe` 22

    describe "Exercise 6" $ do
      describe "bestWords" $ do
        it "returns the highest scoring word in a given list of words" $ do
          bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']) `shouldBe` ["carb"]

        it "returns multiple words when they have the same value" $ do
          bestWords ["cat", "rat", "bat"] `shouldBe` ["cat","bat"]

        it "returns nothing when given an empty list" $ do
          bestWords [] `shouldBe` []

    describe "Exercise 7" $ do
      describe "scrabbleValueTemplate" $ do
        it "calcuates the value of a word when the word is tripled by the template" $ do
          scrabbleValueTemplate "?e??3" "peace" `shouldBe` 27

        it "calcuates the value of a word when a letter is doubled and the word is doubled" $ do
          scrabbleValueTemplate "De?2?" "peace" `shouldBe` 24

        it "calcuates the value of a word when a letter is tripled" $ do
            scrabbleValueTemplate "??Tce" "peace" `shouldBe` 11
