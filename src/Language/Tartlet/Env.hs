module Language.Tartlet.Env where

import Control.Effect.Throw
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM

import Language.Tartlet.Name
import Zoo

newtype Env a = MkEnv (HashMap Name a)
  deriving (Functor, Show)

initEnv :: Env a
initEnv = MkEnv HM.empty

lookupVar :: Has (Throw LangError) sig m => Env a -> Name -> m a
lookupVar (MkEnv env) x =
  pure (HM.lookup x env) >>=
    maybeThrow (LERuntime $ locate Nothing $ "Unknown identifier " <> showName x)

extend :: Name -> a -> Env a -> Env a
extend x v (MkEnv env) = MkEnv $ HM.insert x v env
