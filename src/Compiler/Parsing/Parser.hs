module Compiler.Parsing.Parser (file) where

import Compiler.Parsing.Expr
import Compiler.Parsing.Lexer
import Compiler.Parsing.Types
import Data.List (intercalate)
import Text.Megaparsec
import Text.Megaparsec.Char

file :: Parser [Stmt]
file = do
  space
  importStmts <- many $ nonIndented (try importStmt) <* space
  otherStmts <- many $ nonIndented (try defineStmt <|> (Expr <$> expr)) <* space
  eof
  return $ importStmts ++ otherStmts

importStmt :: Parser Stmt
importStmt = importPackage <|> importVariables

importPackage :: Parser Stmt
importPackage = do
  keyword "import"
  packageName <- variable `sepBy1` symbol "."
  let path = intercalate "/" packageName
  return $ ImportPackage path

importVariables :: Parser Stmt
importVariables = do
  keyword "from"
  packageName <- variable `sepBy1` symbol "."
  let path = intercalate "/" packageName
  keyword "import"
  importedVariables <- variable `sepBy1` symbol ","
  return $ Import path importedVariables

defineStmt :: Parser Stmt
defineStmt = do
  name <- variable
  symbol "="
  Define name <$> expr
