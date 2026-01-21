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
  importStmts <- many $ nonIndented importStmt <* space
  otherStmts <- many $ nonIndented (try defineStmt <|> (TopLevelExpr <$> expr)) <* space
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
