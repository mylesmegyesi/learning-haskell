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
    | (isDigit first) || (first == '-') = IntegerNode (first:restAsInt)
    | otherwise = IntegerNode []
  where (IntegerNode restAsInt) = readInt rest

read :: String -> ASTNode
read [] = (IntegerNode "")
read arg@(first:rest)
    | (first == ' ') = read rest
    | (first == '"') = (StringNode (init rest))
    | (isDigit first) || (first == '-') = readInt arg
    | otherwise = read []
