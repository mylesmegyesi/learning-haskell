{-
Name: Myles Megyesi
Collaborators: none
Notes: I'm not in the class bro, I just like Haskell
-}

module CIS194.HW02
    ( formableBy
    , wordsFrom
    , wordFitsTemplate
    , wordsFittingTemplate
    , scrabbleValueWord
    , bestWords
    , scrabbleValueTemplate
    ) where

import CIS194.Words (allWords, scrabbleValue)
import Data.List (delete, elem, foldl, map, maximum)

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

type Word = String

-- Write your code below:
formableBy :: Word -> Hand -> Bool
formableBy [] _ = True
formableBy _ [] = False
formableBy (x:xs) hand =
    if (elem x hand) then
        (formableBy xs (delete x hand))
    else
        False

wordsFrom :: Hand -> [Word]
wordsFrom hand = filter (`formableBy` hand) allWords

compareTemplateToWord :: Word -> Template -> Word -> (Bool, Word)
compareTemplateToWord acc [] [] = (True, reverse acc)
compareTemplateToWord acc (x:xs) (y:ys)
    | x == '?' = compareTemplateToWord (y:acc) xs ys
    | x == y = compareTemplateToWord acc xs ys
    | otherwise = (False, [])

wordFitsTemplate :: Template -> Hand -> Word -> Bool
wordFitsTemplate template hand word
    | (length word) /= (length template) = False
    | otherwise =
        case (compareTemplateToWord [] template word) of
            (True, word) -> formableBy word hand
            (False, _) -> False

wordsFittingTemplate :: Template -> Hand -> [Word]
wordsFittingTemplate template hand = filter (wordFitsTemplate template hand) allWords

scrabbleValueWord :: Word -> Int
scrabbleValueWord = sum . map scrabbleValue

scoreWord :: Word -> (Int, Word)
scoreWord word = (scrabbleValueWord word, word)

keepMatching :: Int -> (Int, Word) -> Bool
keepMatching score1 (score2, _) = score1 == score2

bestWords :: [Word] -> [Word]
bestWords words =
    let wordScores = map scoreWord words
        bestScore = maximum $ map fst wordScores
        bestPairs = filter (keepMatching bestScore) wordScores
    in
        map snd bestPairs

calculateMultiplier :: Int -> Char -> Int
calculateMultiplier m '2' = m * 2
calculateMultiplier m '3' = m * 3
calculateMultiplier m l = m

scoreLetterByTemplate :: Char -> Char -> Int
scoreLetterByTemplate 'D' l = 2 * (scrabbleValue l)
scoreLetterByTemplate 'T' l = 3 * (scrabbleValue l)
scoreLetterByTemplate _ l = (scrabbleValue l)

scoreWordByTemplate :: STemplate -> Word -> Int
scoreWordByTemplate [] [] = 0
scoreWordByTemplate (t:ts) (l:ls) =
    (scoreLetterByTemplate t l) + (scoreWordByTemplate ts ls)

scrabbleValueTemplate :: STemplate -> Word -> Int
scrabbleValueTemplate [] [] = 0
scrabbleValueTemplate template word =
    let multiplier = foldl calculateMultiplier 1 template
        wordValue = scoreWordByTemplate template word
    in
        multiplier * wordValue
