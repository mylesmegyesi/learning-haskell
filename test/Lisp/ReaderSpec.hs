module Lisp.ReaderSpec where

import Test.Hspec (Spec, describe, it, shouldBe, pendingWith)
import Lisp.Reader
    (
      ASTNode(IntegerNode, StringNode, ListNode)
    , readString
    )

spec :: Spec
spec =
  describe "Lisp.Reader" $ do
    describe "reading integers" $ do
      it "reads a single positive digit" $ do
        readString "1" `shouldBe` [(IntegerNode True "1")]
        readString "2" `shouldBe` [(IntegerNode True "2")]
        readString "0" `shouldBe` [(IntegerNode True "0")]

      it "reads multiple positive digits" $ do
        readString "22" `shouldBe` [(IntegerNode True "22")]

      it "reads a single negative integer" $ do
        readString "-1" `shouldBe` [(IntegerNode False "1")]

      it "reads multiple negative integer" $ do
        readString "-22" `shouldBe` [(IntegerNode False "22")]

      it "ignores leading whitespace" $ do
        readString " 1" `shouldBe` [(IntegerNode True "1")]

      it "ignores trailing whitespace of a positive number" $ do
        readString "22 " `shouldBe` [(IntegerNode True "22")]

      it "ignores trailing whitespace of a negative number" $ do
        readString "-22 " `shouldBe` [(IntegerNode False "22")]

    describe "reading strings" $ do
      it "reads a string" $ do
        readString "\"howdy\"" `shouldBe` [(StringNode "howdy")]

      it "ignores leading whitespace" $ do
        readString " \"howdy\"" `shouldBe` [(StringNode "howdy")]

      it "ignores trailing whitespace" $ do
        readString "\"howdy\" " `shouldBe` [(StringNode "howdy")]

      it "does not allow quotes in quotes" $
        pendingWith "test error case"

    describe "reading lists" $ do
      it "reads an empty list" $ do
        readString "()" `shouldBe` [(ListNode [])]

      it "ignores leading spaces" $ do
        readString " ()" `shouldBe` [(ListNode [])]

      it "ignores trailing spaces" $ do
        readString "  ()  " `shouldBe` [(ListNode [])]

      it "reads a list with one integer" $ do
        readString "(1)" `shouldBe` [(ListNode [(IntegerNode True "1")])]

      it "ignores spaces leading the first value" $ do
        readString "(  1) " `shouldBe` [(ListNode [(IntegerNode True "1")])]

      it "ignores spaces trailing the first value" $ do
        readString "(   1   ) " `shouldBe` [(ListNode [(IntegerNode True "1")])]

      it "reads a list with two integers" $ do
        readString "(1 2)" `shouldBe` [(ListNode [ (IntegerNode True "1")
                                                 , (IntegerNode True "2")
                                                 ])]

      it "ignores spaces trailing the first value" $ do
        readString "( 1   2   ) " `shouldBe` [(ListNode [ (IntegerNode True "1")
                                                        , (IntegerNode True "2")
                                                        ])]

      it "handles commas between elements of the list" $ do
        readString "(1, 2)" `shouldBe` [(ListNode [ (IntegerNode True "1")
                                                  , (IntegerNode True "2")
                                                  ])]

      it "does not allow leading commas before elements in a list" $ do
        pendingWith "readString \"(,1)\""

      it "does not allow a trailing commas after the last element in a list" $ do
        pendingWith "readString \"(1,)\""

      it "does not allow multiple commas between elements in a list" $ do
        pendingWith "readString \"(1 ,, 1)\""
