module LearnYouAHaskell.SolveRPN (solveRPN) where

import System.Environment (getArgs)

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl evalString [] . words
  where evalString (x:y:ys) "-"  = (y-x):ys
        evalString (x:y:ys) "+"  = (x+y):ys
        evalString (x:y:ys) "*"  = (x*y):ys
        evalString stack    item = read item:stack

main = do
  (expression:args) <- getArgs
  print $ solveRPN expression
