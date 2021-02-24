module Parser where

import Text.ParserCombinators.Parsec
import Data.Char

import Expression

-- Receives a string, parses it, and returns an expression
parseExpression :: String -> Either ParseError Expression
parseExpression s = do
  e <- parse anyExpressionParser "" s
  return e

-- Parser any possible expression
anyExpressionParser :: GenParser Char st Expression
anyExpressionParser = numberParser <|> symbolParser <|> sexprParser

-- Parses an S-expr
sexprParser :: GenParser Char st Expression
sexprParser = do
  _ <- spaces
  _ <- char '('
  es <- many1 (try $ spaces >> anyExpressionParser)
  _ <- spaces
  _ <- char ')' >> spaces
  return $ SExpr es

-- Parses a symbol
symbolParser :: GenParser Char st Expression
symbolParser = Symbol <$> do
  c <- symbolChar
  cs <- many (symbolChar <|> digit)
  return $ c:cs
    where symbolChar = letter <|> oneOf "!#$%&*+-/:<=>?@\\^_`~"

-- Parses a number
numberParser :: GenParser Char st Expression
numberParser = do
  num <- read <$> many1 digit
  return $ Number num
