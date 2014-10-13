module Lisp.InterpreterSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Lisp.Interpreter (eval)

spec :: Spec
spec =
  describe "Lisp.Interpreter" $ do

    it "evaluates integers" $ do
      eval "1" `shouldBe` (1 :: Int)
      -- eval "2" `shouldBe` 2
      -- eval "0" `shouldBe` 0
      -- eval "-1" `shouldBe` -1

    -- it "parses a vector" $ do
    --   eval "[1 1]" `shouldBe` [1, 1]
