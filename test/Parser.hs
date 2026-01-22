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
      "lambdaA = 1"
    ]
    ++ Expr.tests
