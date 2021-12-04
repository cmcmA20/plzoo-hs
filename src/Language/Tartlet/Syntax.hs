module Language.Tartlet.Syntax where

import Language.Tartlet.Name

data Expr
  = Var !Name
  | Lam !Name !Expr
  | App !Expr !Expr
  deriving (Eq, Show)
