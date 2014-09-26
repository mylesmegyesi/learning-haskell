module Lisp.Reader (
  read
  , ASTNode(..)
  ) where

import Prelude hiding (read)
import Data.Char (isDigit)

data ASTNode = IntegerNode String | StringNode String deriving (Show, Eq)

readInt :: String -> ASTNode
readInt [] = (IntegerNode [])
readInt (first:rest)
  | (isDigit first) || (first == '-') =
    let (IntegerNode i) = readInt rest
    in
      (IntegerNode (first:i))
  | otherwise = (IntegerNode [])

read :: String -> ASTNode
read arg@(first:rest)
    | (first == ' ') = read rest
    | (first == '"') = (StringNode (init rest))
    | (isDigit first) || (first == '-') = readInt arg
