module Lisp.Interpreter (eval) where

class Eval a where
  evalThing :: String -> a

instance Eval Int where
  evalThing = read

instance Eval [a] where
  evalThing thing = []

eval :: (Eval a) => String -> a
eval = evalThing
