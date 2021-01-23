-- | Evaluation of expressions
module Eval where

import           Control.Algebra
import           Control.Carrier.State.Strict
import           Control.Carrier.Throw.Either
import           Control.Monad                (when)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HM
import           Data.Text                    (Text)

import           Syntax
import           Zoo

-- | Semantics.
type Sem = Integer

-- | Context.
type Ctx = HashMap Text Sem

eval
  :: ( Has (State Ctx) sig m
     , Has (Throw LangError) sig m
     )
  => Exp
  -> m Sem

eval (Variable j) =
  gets @Ctx (HM.lookup j) >>=
    maybeThrow (LERuntime $ locate Nothing "Unknown variable")

eval (Numeral n) = pure n

eval (Plus a b) = do
  x <- eval a
  y <- eval b
  pure $ x + y

eval (Minus a b) = do
  x <- eval a
  y <- eval b
  pure $ x - y

eval (Times a b) = do
  x <- eval a
  y <- eval b
  pure $ x * y

eval (Divide a b) = do
  y <- eval b
  when (y == 0) $
    throwError $ LERuntime $ locate Nothing "Division by zero"
  x <- eval a
  pure $ x `div` y

eval (Negate a) = do
  x <- eval a
  pure $ -x


evalCmd
  :: ( Has (State Ctx) sig m
     , Has (Throw LangError) sig m
     )
  => Cmd
  -> m Sem
evalCmd (Expression e  ) = eval e
evalCmd (Definition k e) = do
  r <- eval e
  modify (HM.insert k r)
  pure r
