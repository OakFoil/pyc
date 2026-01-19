module Compiler.Parsing.Parser (file, predefinedVars) where

import Compiler.Expr
import Compiler.Parsing.Lexer
import Compiler.Parsing.Types
import Control.Monad.Combinators.Expr
import Data.List (intercalate)
import Text.Megaparsec
import Text.Megaparsec.Char

file :: Parser [Stmt]
file = do
  space
  importStmts <- many $ nonIndented importStmt <* space
  otherStmts <- many $ nonIndented (defineStmt <|> (TopLevelExpr <$> expr)) <* space
  eof
  return $ importStmts ++ otherStmts

importStmt :: Parser Stmt
importStmt = do
  keyword "from"
  packageName <- variable
  keyword "import"
  importedFile <- variable `sepBy1` symbol "."
  let path = intercalate "/" importedFile
  return $ Import packageName path

defineStmt :: Parser Stmt
defineStmt = do
  name <- variable
  symbol "="
  Define name <$> expr

lambda :: Parser Expr
lambda = do
  keyword "lambda"
  varNames <- some variable
  symbol ":"
  parsedExpr <- expr
  return $ foldr Lam parsedExpr varNames

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

term :: Parser Expr
term = choice [parens expr, Integer <$> integer, Var <$> variable, lambda]

expr :: Parser Expr
expr = makeExprParser term operators

predefinedVars :: [String]
predefinedVars = ["+", "-", "*", "/", ".", "$"]

operators :: [[Operator Parser Expr]]
operators =
  [ [Prefix $ (Var "-" :@ Integer 0 :@) <$ string "-"],
    [InfixL $ (:@) <$ (symbol "" *> lookAhead (try expr))],
    [binaryR "."],
    [binaryL "*", binaryL "/"],
    [binaryL "+", binaryL "-"],
    [binaryR "$"]
  ]
  where
    binaryL, binaryR :: String -> Operator Parser Expr
    binaryL op = InfixL $ (:@) . (Var op :@) <$ symbol op
    binaryR op = InfixR $ (:@) . (Var op :@) <$ symbol op
