-- Evaluation of expressions, given as big step semantics.
module Eval where

import           Control.Effect.Throw

import           Syntax
import           Zoo

type Sem = Integer

type Ctx = ()

eval :: Has (Throw LangError) sig m => Cmd -> m Sem
eval (Numeral n  ) = pure n
eval (Plus    a b) = (+) <$> eval a <*> eval b
eval (Minus   a b) = (-) <$> eval a <*> eval b
eval (Times   a b) = (*) <$> eval a <*> eval b
eval (Divide  a b) = do
  y <- eval b
  if y /= 0
     then div <$> eval a <*> pure y
     else throwError $ LERuntime $ locate Nothing "Division by zero"
eval (Negate  a  ) = negate <$> eval a
