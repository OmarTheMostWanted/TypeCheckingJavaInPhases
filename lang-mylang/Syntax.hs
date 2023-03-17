module Syntax where

data Type
  = NumT
  | FunT Type Type
  deriving Eq

data Expr
  = Num Int
  | Plus Expr Expr
  | App Expr Expr
  | Ident String
  | Abs String Type Expr
  deriving (Eq, Show)

instance Show Type where
  show NumT = "num"
  show (FunT ti to) = "(" ++ (show ti) ++ " -> " ++ (show to) ++ ")"

example :: Expr
example = App (Abs "x" NumT (Plus (Ident "x") (Ident "x"))) (Num 21)
