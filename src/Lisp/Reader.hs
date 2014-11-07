{-# LANGUAGE FlexibleContexts #-}

module Lisp.Reader
    ( ASTNode( IntegerNode
             , FloatNode
             , ListNode
             , StringNode
             , VectorNode
             )
    , readString
    ) where
import Text.Parsec
    ( (<|>)
    , Stream
    , ParsecT
    )
import Text.ParserCombinators.Parsec
    ( Parser
    , char
    , digit
    , many
    , many1
    , noneOf
    , parse
    , oneOf
    , optionMaybe
    , sepBy
    , spaces
    )

data ASTNode = IntegerNode Bool String
             | FloatNode Bool String String
             | ListNode [ASTNode]
             | StringNode String
             | VectorNode [ASTNode]
             deriving (Eq, Show)

isPositive :: Maybe Char -> Bool
isPositive Nothing = True
isPositive (Just '+') = True
isPositive (Just _) = False

parseInt :: Parser ASTNode
parseInt = do maybeSign <- optionMaybe (oneOf "-+")
              result <- many1 digit
              return (IntegerNode (isPositive maybeSign) result)

parseFloat :: Parser ASTNode
parseFloat = do maybeSign <- optionMaybe (oneOf "-+")
                intPart <- many1 digit
                _ <- char '.'
                decimalPart <- many1 digit
                return (FloatNode (isPositive maybeSign) intPart decimalPart)

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

parseVector :: Parser ASTNode
parseVector = do _ <- char '['
                 exprs <- (parseExpr `sepBy` elementSeparator)
                 _ <- char ']'
                 return (VectorNode exprs)

parseExpr :: Parser ASTNode
parseExpr = do spaces
               result <- parseInt <|> parseFloat <|> parseString <|> parseList <|> parseVector
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
