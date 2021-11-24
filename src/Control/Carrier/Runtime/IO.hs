module Control.Carrier.Runtime.IO
  ( module Control.Effect.Runtime
  , RuntimeIOC(..)
  ) where

import Control.Algebra
import Control.Effect.Runtime
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Kind (Type)
import Data.Text.IO qualified as TIO
import System.Exit qualified as S

type CarrierKind = Type -> Type

newtype RuntimeIOC (m :: CarrierKind) (a :: Type) = MkRuntimeIOC { runRuntimeIOC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Runtime :+: sig) (RuntimeIOC m) where
  alg _   (L RExit)      c = c <$ liftIO S.exitSuccess
  alg _   (L RRead)      c = (<$ c) <$> liftIO TIO.getLine
  alg _   (L (RWrite t)) c = c <$ liftIO (TIO.putStr t)
  alg hdl (R other)      c = MkRuntimeIOC $ alg (runRuntimeIOC . hdl) other c
