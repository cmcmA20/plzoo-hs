-- Evaluation of expressions, given as big step semantics.
module Eval where

import Control.Applicative (liftA2)

import Syntax
import Zoo

type Sem = Integer

type Ctx = ()

eval :: Cmd -> Either LangError Sem
eval (Numeral n  ) = pure n
eval (Plus    a b) = liftA2 (+) (eval a) (eval b)
eval (Minus   a b) = liftA2 (-) (eval a) (eval b)
eval (Times   a b) = liftA2 (*) (eval a) (eval b)
eval (Divide  a b) = do
  y <- eval b
  if y /= 0
     then liftA2 div (eval a) (pure y)
     else Left $ LERuntime $ locate Nothing "Division by zero"
eval (Negate  a  ) = negate <$> eval a

eval' :: Evaluator Sem Ctx Cmd
eval' _ c = (eval c, ())
