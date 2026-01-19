module Compiler.Expr (Expr(..)) where

data Expr =
      Integer Integer
    | Var String
    | Expr :@ Expr
    | Lam String Expr
    deriving (Show, Read, Eq)
