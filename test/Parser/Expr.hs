module Parser.Expr (tests) where

import Compiler.Expr
import Compiler.Parsing.Expr
import Parser.Util
import Test.HUnit

tests :: [Assertion]
tests =
  map
    (testIfParsesWithoutError expr)
    [ "1",
      "lambda a, b: a + b",
      "f(a, b, c)"
    ]
    ++ map
      (testIfParsesCorrectly expr)
      [ ("lambda: 1 + 1", Lam [] $ Var "+" :@ [Integer 1, Integer 1])
      ]
