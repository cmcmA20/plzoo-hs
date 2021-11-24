module Semantics where

import Data.Text (Text)

import Syntax

newtype Env a = MkEnv [(Name, a)]
  deriving (Functor, Show)

initEnv :: Env a
initEnv = MkEnv []

data Value
  = VClosure (Env Value) Name Expr
  deriving Show
