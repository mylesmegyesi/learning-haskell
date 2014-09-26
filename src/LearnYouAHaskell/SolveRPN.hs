module LearnYouAHaskell.SolveRPN (solveRPN) where

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl evalString [] . words
  where evalString (x:y:ys) "-"  = (y-x):ys
        evalString (x:y:ys) "+"  = (x+y):ys
        evalString (x:y:ys) "*"  = (x*y):ys
        evalString stack    item = read item:stack
