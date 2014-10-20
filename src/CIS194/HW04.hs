{-
Name: Myles Megyesi
Collaborators: none
Notes: I'm not in the class bro, I just like Haskell
-}

module CIS194.HW04
    ( ex1
    , ex2_1
    , ex2_2
    , ex3
    , ex4_1
    , ex4_2
    , ex5_1
    , ex5_2
    , ex5_3
    , ex7
    , ex8_1
    , ex8_2
    , ex8_3
    , ex9_1
    , ex9_2
    , ex10
    , ex11_1
    , ex11_2
    , ex12_1
    , ex12_2
    , insertBST
    , allCaps
    , dropTrailingWhitespace
    , firstLetters
    , asList
    ) where

import Data.Char
    ( isSpace
    , isUpper
    )
import Data.Maybe (fromJust)
import Data.List
    ( intercalate
    )
import CIS194.BST as BST

ex1 :: a -> b -> b
ex1 _ b = b

ex2_1 :: a -> a -> a
ex2_1 one _ = one

ex2_2 :: a -> a -> a
ex2_2 _ two = two

ex3 :: Int -> a -> a
ex3 _ ret = ret

ex4_1 :: Bool -> a -> a -> a
ex4_1 _ two _ = two

ex4_2 :: Bool -> a -> a -> a
ex4_2 _ _ three = three

ex5_1 :: Bool -> Bool
ex5_1 x = x

ex5_2 :: Bool -> Bool
ex5_2 _ = True

ex5_3 :: Bool -> Bool
ex5_3 _ = False

-- no implementation can satisfy this type signature
-- ex6 :: (a -> a) -> a

ex7 :: (a -> a) -> a -> a
ex7 f a = f a

ex8_1 :: [a] -> [a]
ex8_1 _ = []

ex8_2 :: [a] -> [a]
ex8_2 [] = []
ex8_2 (x:_) = [x]

ex8_3 :: [a] -> [a]
ex8_3 [] = []
ex8_3 [x] = [x]
ex8_3 (_:y:_) = [y]

ex9_1 :: (a -> b) -> [a] -> [b]
ex9_1 _ _ = []

ex9_2 :: (a -> b) -> [a] -> [b]
ex9_2 = map

ex10 :: Maybe a -> a
ex10 = fromJust

ex11_1 :: a -> Maybe a
ex11_1 _ = Nothing

ex11_2 :: a -> Maybe a
ex11_2 = Just

ex12_1 :: Maybe a -> Maybe a
ex12_1 _ = Nothing

ex12_2 :: Maybe a -> Maybe a
ex12_2 = id

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x BST.Leaf = (BST.Node BST.Leaf x BST.Leaf)
insertBST comp x (BST.Node left y right) =
    case comp x y of
        LT -> (BST.Node (insertBST comp x left) y right)
        GT -> (BST.Node left y (insertBST comp x right))
        EQ -> (BST.Node (insertBST comp x left) y right)

isFirstUpper :: String -> Bool
isFirstUpper [] = False
isFirstUpper (x:_) = isUpper x

allCaps :: [String] -> Bool
allCaps = all isFirstUpper

dropLeadingWhitespace :: String -> String
dropLeadingWhitespace [] = []
dropLeadingWhitespace s@(x:xs)
    | isSpace x = dropLeadingWhitespace xs
    | otherwise = s

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropLeadingWhitespace . reverse

firstLetterAcc :: String -> [Char] -> [Char]
firstLetterAcc [] acc = acc
firstLetterAcc (x:_) acc = x:acc

firstLetters :: [String] -> [Char]
firstLetters = foldr firstLetterAcc []

asList :: [String] -> String
asList xs = "[" ++ (intercalate "," xs) ++ "]"
