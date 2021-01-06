-- Evaluation of expressions
module Eval where

import           Control.Effect.Lift
import           Control.Effect.State
import           Control.Lens
import           Control.Monad (when)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           GHC.Generics (Generic)

import Syntax
import Zoo

type Sem = Integer

data Env = MkEnv
  { context    :: !(HashMap Text Sem)
  , replResult :: !(Maybe Sem) }
  deriving (Eq, Generic, Show)

eval
  :: ( Has (State Env) sig m, Has (Lift IO) sig m)
  => Exp
  -> m Integer

eval (Variable j) =
  gets @Env (^. #context . to (HM.lookup j)) >>=
    maybeRaiseError EKRuntime LNowhere "Unknown variable"

eval (Numeral n) =
  pure n

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
    raiseError EKRuntime LNowhere "Division by zero"
  x <- eval a
  pure $ x `div` y

eval (Negate a) = do
  x <- eval a
  pure $ -x
