module Syntax where

import Data.Text (Text)

newtype Name = MkName Text
  deriving (Eq, Show)

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  deriving (Eq, Show)
