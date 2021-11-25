module Control.Carrier.Runtime.IO
  ( module Control.Effect.Runtime
  , RuntimeIOC(..)
  ) where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Runtime
import Data.Kind (Type)
import Data.Text.IO qualified as TIO
import System.Exit (exitWith)

type CarrierKind = Type -> Type

newtype RuntimeIOC (m :: CarrierKind) (a :: Type) = MkRuntimeIOC { runRuntimeIOC :: m a }
  deriving (Applicative, Functor, Monad)

instance Has (Lift IO) sig m => Algebra (Runtime :+: sig) (RuntimeIOC m) where
  alg _   (L (RExit code)) c = c <$ sendIO (exitWith code)
  alg _   (L RRead)        c = (<$ c) <$> sendIO TIO.getLine
  alg _   (L (RWrite t))   c = c <$ sendIO (TIO.putStr t)
  alg hdl (R other)        c = MkRuntimeIOC $ alg (runRuntimeIOC . hdl) other c
