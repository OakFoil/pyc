module Parser (tests) where

import Compiler.Expr
import Compiler.Parsing.Parser
import Compiler.Parsing.Types
import Parser.Expr qualified as Expr
import Parser.Util
import Test.HUnit

tests :: [Assertion]
tests =
  map
    (testIfParsesCorrectly file)
    [ ("import package.submodule", [ImportPackage "package/submodule"]),
      ("from package.submodule import a, b", [Import "package/submodule" ["a", "b"]]),
      ("a = a", [Define "a" $ Var "a"]),
      ("lambdaA = 1", [Define "lambdaA" $ Integer 1]),
      ("importA = 1", [Define "importA" $ Integer 1]),
      ("fromA = 1", [Define "fromA" $ Integer 1])
    ]
    ++ Expr.tests
