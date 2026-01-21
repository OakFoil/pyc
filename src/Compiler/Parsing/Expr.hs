module Compiler.Parsing.Expr (expr, predefinedVars) where

import Compiler.Expr
import Compiler.Parsing.Lexer
import Compiler.Parsing.Types
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char

lambda :: Parser Expr
lambda = do
  keyword "lambda"
  lambdaArguments <- variable `sepBy` symbol ","
  symbol ":"
  lambdaBody <- expr
  return $ Lam lambdaArguments lambdaBody

app :: Parser Expr
app = do
  function <- term
  symbol "("
  arguments <- expr `sepBy` symbol ","
  symbol ")"
  return $ function :@ arguments

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

term :: Parser Expr
term = choice [parens expr, lambda, Integer <$> integer, Var <$> variable]

expr :: Parser Expr
expr = makeExprParser (try app <|> term) operators

predefinedVars :: [String]
predefinedVars = ["+", "-", "*", "/", ".", "$"]

operators :: [[Operator Parser Expr]]
operators =
  [ [Prefix $ Negate <$ string "-"],
    [binaryR "."],
    [binaryL "*", binaryL "/"],
    [binaryL "+", binaryL "-"],
    [binaryR "$"]
  ]
  where
    binaryL, binaryR :: String -> Operator Parser Expr
    binaryL op = InfixL $ (\a b -> Var op :@ [a, b]) <$ symbol op
    binaryR op = InfixR $ (\a b -> Var op :@ [a, b]) <$ symbol op
