module CIS194.HW04Spec where

import Test.Hspec
    ( Spec

    , describe
    , context
    , errorCall
    , it
    , shouldBe
    , shouldThrow
    )
import CIS194.HW04
    ( ex1
    , ex2_1
    , ex2_2
    , ex3
    , ex4_1
    , ex4_2
    , ex5_1
    , ex5_2
    , ex5_3
    , ex7
    , ex8_1
    , ex8_2
    , ex8_3
    , ex9_1
    , ex9_2
    , ex10
    , ex11_1
    , ex11_2
    , ex12_1
    , ex12_2
    , insertBST
    , allCaps
    , dropTrailingWhitespace
    , firstLetters
    , asList
    )
import CIS194.BST as BST

spec :: Spec
spec =
  describe "CIS194 Homework 4" $ do
    describe "Exercise 1" $ do
      -- only one function can satisfy this type signature
      it "must return the second argument" $ do
        ex1 "who cares" True `shouldBe` True
        ex1 True "now i care" `shouldBe` "now i care"

    describe "Exercise 2" $ do
      -- only two functions can satisfy this type signature
      it "can return the first argument" $ do
        ex2_1 True False `shouldBe` True
        ex2_2 True False `shouldBe` False

      it "can return the second argument" $ do
        ex2_1 True False `shouldBe` True
        ex2_2 True False `shouldBe` False

    describe "Exercise 3" $ do
      -- only one function can satisfy this type signature
      it "must return the second argument" $ do
        ex3 1 True `shouldBe` True

    describe "Exercise 4" $ do
      -- only two functions can satisfy this type signature
      it "can return the second argument" $ do
        ex4_1 True "a" "b" `shouldBe` "a"

      it "can return the third argument" $ do
        ex4_2 True "a" "b" `shouldBe` "b"

    describe "Exercise 5" $ do
      -- only three functions can satisfy this type signature
      it "can return the argument" $ do
        ex5_1 True `shouldBe` True
        ex5_1 False `shouldBe` False

      it "can always return True" $ do
        ex5_2 True `shouldBe` True
        ex5_2 False `shouldBe` True

      it "can always return False" $ do
        ex5_3 True `shouldBe` False
        ex5_3 False `shouldBe` False

    describe "Exercise 6" $ do
      it "no function can satisfy this type signature" $ do
        True `shouldBe` True

    describe "Exercise 7" $ do
      -- only one function can satisfy this type signature
      it "must call the given lamba with the second argument" $ do
        ex7 (\ x -> x) 'a' `shouldBe` 'a'
        ex7 (\ _ -> 'b') 'a' `shouldBe` 'b'

    describe "Exercise 8" $ do
      -- there are an infinite number of implemenations
      it "can blindly return an empty list" $ do
        ex8_1 ['a', 'b'] `shouldBe` []
        ex8_1 ["a", "b"] `shouldBe` []

      describe "can pick any one of the elements and return it wrapped in a list" $ do
        it "for a list of length n, there could be n implementations that satisfy this type signature in this manner. Since the length of the list is only bounded by the size of memory, so are the number of implementations for this type signature." $ do
          ex8_2 ['a', 'b'] `shouldBe` ['a']
          ex8_3 ["a", "b"] `shouldBe` ["b"]

    describe "Exercise 9" $ do
      -- this is the type signature of map
      it "can blindy return an empty list" $ do
        ex9_1 (\x -> [x]) ['a', 'b'] `shouldBe` []

      it "can call the given function on each element in the given list" $ do
        ex9_2 (\x -> [x]) ['a', 'b'] `shouldBe` ["a", "b"]

    describe "Exercise 10" $ do
      -- this is the type signature of Data.Maybe.fromJust
      it "can return the value if it is in a Just" $ do
        ex10 (Just 'a') `shouldBe` 'a'

      it "must throw an error if the value is a Nothing" $ do
        ex10 Nothing `shouldThrow` errorCall "Maybe.fromJust: Nothing"

    describe "Exercise 11" $ do
      -- there are two implementations that satisfy the type signature
      it "blindy returns Nothing" $ do
        ex11_1 'a' `shouldBe` Nothing

      it "wraps the value in a Just" $ do
        ex11_2 'a' `shouldBe` (Just 'a')

    describe "Exercise 12" $ do
      -- there are two implementations that satisfy the type signature
      it "blindy returns Nothing" $ do
        ex12_1 (Just 'a') `shouldBe` Nothing
        ex12_1 (Nothing::Maybe Char) `shouldBe` Nothing

      it "can just return the given Maybe" $ do
        ex12_2 (Just 'a') `shouldBe` (Just 'a')
        ex12_2 (Nothing::Maybe Char) `shouldBe` Nothing

    describe "Exercise 13" $ do
      context "when inserting a value into an empty tree" $ do
        it "returns a Node with two Leafs" $ do
          insertBST (\_ _ -> EQ) 'a' BST.Leaf `shouldBe` (BST.Node BST.Leaf 'a' BST.Leaf)

      context "when inserting x into a non-empty tree whose root node has a value greater than x" $ do
        it "inserts the value into the left sub-tree" $ do
          let root = (BST.Node BST.Leaf 'b' BST.Leaf) in
              insertBST compare 'a' root `shouldBe` (BST.Node (BST.Node BST.Leaf 'a' BST.Leaf) 'b' BST.Leaf)

      context "when inserting x into a non-empty tree whose root node has a value less than x" $ do
        it "inserts the value into the right sub-tree" $ do
          let root = (BST.Node BST.Leaf 'a' BST.Leaf) in
              insertBST compare 'b' root `shouldBe` (BST.Node BST.Leaf 'a' (BST.Node BST.Leaf 'b' BST.Leaf))

      context "when inserting x into a non-empty tree whose root node has a value equal to x" $ do
        it "inserts the value into the left sub-tree" $ do
          let root = (BST.Node BST.Leaf 'a' BST.Leaf) in
              insertBST compare 'a' root `shouldBe` (BST.Node (BST.Node BST.Leaf 'a' BST.Leaf) 'a' BST.Leaf)

      it "inserts the value deep into the tree" $ do
        let root = (BST.Node (BST.Node BST.Leaf 'a' BST.Leaf) 'c' BST.Leaf) in
            insertBST compare 'b' root `shouldBe` (BST.Node (BST.Node BST.Leaf 'a' (BST.Node BST.Leaf 'b' BST.Leaf)) 'c' BST.Leaf)

    describe "Exercise 14" $ do
      it "returns True if all the words in the list are capitalized" $ do
        allCaps ["Hi","There"] `shouldBe` True

      it "returns True if there are no Strings" $ do
        allCaps [] `shouldBe` True

      it "returns False if a words is not capitalized" $ do
        allCaps ["Hi","there"] `shouldBe` False

      it "returns False for an empty string" $ do
        allCaps ["","There"] `shouldBe` False

    describe "Exercise 15" $ do
      context "when the given string has trailing spaces" $ do
        it "returns the string with the trailing spaces removed" $ do
          dropTrailingWhitespace "foo  " `shouldBe` "foo"

      context "when the string is empty" $ do
        it "returns an empty string" $ do
          dropTrailingWhitespace "" `shouldBe` ""

      context "when the string has no trailing spaces" $ do
        it "returns the same string" $ do
          dropTrailingWhitespace "foo" `shouldBe` "foo"

    describe "Exercise 16" $ do
      context "all given strings have at least on character" $ do
        it "returns the first letter of each string" $ do
          firstLetters ["foo", "bar"] `shouldBe` ['f', 'b']

      context "when one of the given strings is empty" $ do
        it "skips strings that are empty" $ do
          firstLetters ["alpha",""] `shouldBe` ['a']
          firstLetters ["",""] `shouldBe` []

      context "when there are no strings" $ do
        it "returns an empty list" $ do
          firstLetters [] `shouldBe` []
        it "skips strings that are empty" $ do
          firstLetters ["alpha",""] `shouldBe` ['a']

    describe "Exercise 17" $ do
      it "renders a list of strings" $ do
        asList ["alpha","beta","gamma"] `shouldBe` "[alpha,beta,gamma]"

      it "renders empty brackets when the list is empty" $ do
        asList [] `shouldBe` "[]"

      it "renders a string in brackets when the list has one item" $ do
        asList ["lonely"] `shouldBe` "[lonely]"
