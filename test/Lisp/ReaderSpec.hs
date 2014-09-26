module Lisp.ReaderSpec where

import Test.Hspec (describe, it, shouldBe, shouldThrow, anyException)
import Lisp.Reader (read, ASTNode(IntegerNode, StringNode))
import Prelude hiding (read)

spec =
  describe "Lisp.Reader" $ do

    it "reads an integer" $ do
      read "1" `shouldBe` (IntegerNode "1")
      read "2" `shouldBe` (IntegerNode "2")
      read "0" `shouldBe` (IntegerNode "0")
      read "-1" `shouldBe` (IntegerNode "-1")
      read "22" `shouldBe` (IntegerNode "22")
      read "-22" `shouldBe` (IntegerNode "-22")
      read "-22" `shouldBe` (IntegerNode "-22")

    it "reads a string" $ do
      read "\"howdy\"" `shouldBe` (StringNode "howdy")

    it "ignores leading whitespace" $ do
      read " 1" `shouldBe` (IntegerNode "1")

    it "ignores trailing whitespace" $ do
      read "1 " `shouldBe` (IntegerNode "1")
      read "-22 " `shouldBe` (IntegerNode "-22")
