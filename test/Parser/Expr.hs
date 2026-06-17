{-# LANGUAGE QuasiQuotes #-}

module Parser.Expr (tests) where

import Compiler.Expr
import Compiler.Parsing.Expr
import Parser.Util
import Test.HUnit
import Text.RawString.QQ (r)

tests :: [Assertion]
tests =
  map
    (testIfParsesWithError expr)
    [ [r|"a'|]
    ]
    ++ map
      (testIfParsesCorrectly expr)
      [ ("1", Integer 1),
        ("3.14", Float 3.14),
        ("3e10", Float 3e10),
        ("3E-10", Float 3E-10),
        ( [r|'a1. \'"\t
'|],
          String "a1. '\"\t\n"
        ),
        ( [r|"a1. \"'\t
"|],
          String "a1. \"'\t\n"
        ),
        ("lambda: 1 + 1", Lam [] $ Var "+" :@ [Integer 1, Integer 1]),
        ("lambda a, b: a + b", Lam ["a", "b"] $ Var "+" :@ [Var "a", Var "b"]),
        ("f(a, b, c)", Var "f" :@ [Var "a", Var "b", Var "c"])
      ]
