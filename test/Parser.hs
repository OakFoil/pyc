module Parser (tests) where

import Compiler.Parsing.Parser
import Parser.Expr qualified as Expr
import Parser.Util
import Test.HUnit

tests :: [Assertion]
tests =
  map
    (testIfParsesWithoutError file)
    [ "from package import variable",
      "a = a",
      "lambdaA = 1"
    ]
    ++ Expr.tests
