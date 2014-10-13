module LearnYouAHaskell.SolveRPNSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import LearnYouAHaskell.SolveRPN (solveRPN)

spec :: Spec
spec =
  describe "solveRPN" $ do
    it "adds two integers" $
      solveRPN "1 2 +" `shouldBe` (3::Integer)

    it "subtracts two integers" $
      solveRPN "1 2 -" `shouldBe` (-1::Integer)
