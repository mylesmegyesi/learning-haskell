{-
Name: Myles Megyesi @mylesmegyesi
Collaborators: none
Notes: I'm not in the class bro
-}

module CIS194.HW01
    (  lastDigit
    , dropLastDigit
    , toDigits
    , doubleEveryOther
    , sumDigits
    , validate
    ) where         -- We'll learn more about this later

base :: Integer
base = 10

lastDigit :: Integer -> Integer
lastDigit i = i `mod` base

dropLastDigit :: Integer -> Integer
dropLastDigit i = (i - (i `mod` base)) `quot` base

toDigitsReversed :: Integer -> [Integer]
toDigitsReversed i
  | i > 0 = (lastDigit i):(toDigitsReversed $ dropLastDigit i)
  | otherwise =  []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsReversed

recDoubleEveryOther :: [Integer] -> [Integer]
recDoubleEveryOther [] = []
recDoubleEveryOther [x] = [x]
recDoubleEveryOther (x:y:xs) = x:(y * 2):(recDoubleEveryOther xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . recDoubleEveryOther . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

isModZero :: Integer -> Integer -> Bool
isModZero b i = (i `mod` b) == 0

validate :: Integer -> Bool
validate = isModZero base . sumDigits . doubleEveryOther . toDigits
