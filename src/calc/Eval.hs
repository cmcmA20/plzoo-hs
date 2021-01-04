-- Evaluation of expressions, given as big step semantics.
module Eval where

import Syntax

eval :: Exp -> Integer
eval (Numeral n  ) = n
eval (Plus    a b) = eval a + eval b
eval (Minus   a b) = eval a - eval b
eval (Times   a b) = eval a * eval b
eval (Divide  a b) = let y = eval b in if y /= 0 then eval a `div` y else undefined
eval (Negate  a  ) = -(eval a)
