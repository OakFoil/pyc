module Compiler.Expr (Expr (..)) where

data Expr
  = Integer Integer
  | Float Double
  | String String
  | Negate Expr
  | Var String
  | Expr :@ [Expr]
  | Lam [String] Expr
  deriving (Show, Read, Eq)
