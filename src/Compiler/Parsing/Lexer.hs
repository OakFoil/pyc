module Compiler.Parsing.Lexer (lexeme, symbol, integer, variable, keyword, indentBlock, nonIndented, lineFold) where

import Compiler.Parsing.Types
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = empty

scn :: Parser ()
scn =
  L.space
    space1
    lineComment
    blockComment

sc :: Parser ()
sc =
  L.space
    (void $ some (char ' ' <|> char '\t'))
    lineComment
    blockComment

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

lineFold :: (Parser () -> Parser a) -> Parser a
lineFold = L.lineFold scn

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = label "integer" $ lexeme L.decimal

float :: (RealFloat a) => Parser a
float = label "float" $ lexeme L.float

variable :: Parser String
variable = label "variable" $ lexeme $ (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')

keyword :: String -> Parser String
keyword a = lexeme (string a <* notFollowedBy alphaNumChar)
