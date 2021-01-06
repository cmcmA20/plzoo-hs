-- Evaluation of expressions, given as big step semantics.
module Eval where

import Syntax
import Zoo

eval :: Exp -> Integer
eval (Numeral n  ) = n
eval (Plus    a b) = eval a + eval b
eval (Minus   a b) = eval a - eval b
eval (Times   a b) = eval a * eval b
eval (Divide  a b) =
  let y = eval b
   in if y /= 0
      then eval a `div` y
      else raiseErrorClassic EKRuntime LNowhere "Division by zero"
eval (Negate  a  ) = -(eval a)
