module LearnYouAHaskell.SolveRPNSpec (spec) where

import Test.Hspec (describe, it, shouldBe)
import LearnYouAHaskell.SolveRPN (solveRPN)

spec =
  describe "solveRPN" $ do
    it "adds two integers" $
      solveRPN "1 2 +" `shouldBe` 3

    it "subtracts two integers" $
      solveRPN "1 2 -" `shouldBe` -1
