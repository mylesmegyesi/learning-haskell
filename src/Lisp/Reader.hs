{-# LANGUAGE FlexibleContexts #-}

module Lisp.Reader
    ( ASTNode(IntegerNode, StringNode, ListNode)
    , readString
    ) where

import Control.Applicative
     ( (<|>)
     )

import Text.ParserCombinators.Parsec
    ( Parser
    , char
    , digit
    , many
    , many1
    , noneOf
    , parse
    , optionMaybe
    , sepBy
    , spaces
    )

import Text.Parsec.Prim
    ( Stream
    , ParsecT
    )

data ASTNode = IntegerNode Bool String
             | StringNode String
             | ListNode [ASTNode]
             deriving (Show, Eq)

parseInt :: Parser ASTNode
parseInt = do isNegative <- optionMaybe (char '-')
              result <- many1 digit
              case isNegative of
                  Just _ -> return (IntegerNode False result)
                  Nothing -> return (IntegerNode True result)

parseString :: Parser ASTNode
parseString = do _ <- char '"'
                 str <- many $ noneOf ['"']
                 _ <- char '"'
                 return (StringNode str)

elementSeparator :: Stream s m Char => ParsecT s u m ()
elementSeparator = do spaces
                      _ <- optionMaybe (char ',')
                      spaces
                      return ()

parseList :: Parser ASTNode
parseList = do _ <- char '('
               exprs <- (parseExpr `sepBy` elementSeparator)
               _ <- char ')'
               return (ListNode exprs)

parseExpr :: Parser ASTNode
parseExpr = do spaces
               result <- parseInt <|> parseString <|> parseList
               spaces
               return result

lispParser :: Parser [ASTNode]
lispParser = do results <- many1 parseExpr
                spaces
                return results

readString :: String -> [ASTNode]
readString s =
    case parse lispParser "" s of
        Left err -> error $ (show err)
        Right exprs -> exprs
