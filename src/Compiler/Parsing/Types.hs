module Compiler.Parsing.Types (Input, File (..), Stmt (..), Parser, runMyParser) where

import Compiler.Expr
import Data.Either.Extra
import Text.Megaparsec

type Input = String

newtype Error = Error {unError :: String} deriving (Show, Read, Eq, Ord)

instance ShowErrorComponent Error where
  showErrorComponent = unError

data File = File
  { fileName :: String,
    stmts :: [Stmt]
  }
  deriving (Show, Read, Eq)

data Stmt
  = Import String FilePath
  | Define String Expr
  | TopLevelExpr Expr
  deriving (Show, Read, Eq)

type Parser = Parsec Error Input

runMyParser :: Parser output -> FilePath -> Input -> Either String output
runMyParser parser fileName = mapLeft errorBundlePretty . runParser parser fileName
