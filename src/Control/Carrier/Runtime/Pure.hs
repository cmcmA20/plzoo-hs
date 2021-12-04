module Control.Carrier.Runtime.Pure
  ( module Control.Effect.Runtime
  , RuntimePureC(..)
  , withoutRuntime
  ) where

import Control.Algebra
import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Carrier.Writer.Strict
import Control.Effect.Runtime
import Control.Monad (void)
import Data.Kind (Type)
import Data.Text (Text)
import System.Exit (ExitCode(..))

type CarrierKind = Type -> Type

newtype RuntimePureC (m :: CarrierKind) (a :: Type) = MkRuntimePureC { runRuntimePureC :: m a }
  deriving (Applicative, Functor, Monad)

type StdIn  = [Text]
type StdOut = Text

instance (Has (State StdIn) sig m, Has (Writer StdOut) sig m, Has (Throw ExitCode) sig m) => Algebra (Runtime :+: sig) (RuntimePureC m) where
  alg hdl si ct = MkRuntimePureC case si of
    L (RExit code) -> (<$ ct) <$> throwError code
    L RRead        -> (<$ ct) <$> do
      stdin <- get @StdIn
      case stdin of
        []     -> throwError $ ExitFailure 1
        l : ls -> put ls >> pure l
    L (RWrite t)   -> (<$ ct) <$> tell t
    R other        -> alg (runRuntimePureC . hdl) other ct

withoutRuntime :: Monad m => RuntimePureC (StateC StdIn (ThrowC ExitCode (WriterC StdOut m))) a -> m ()
withoutRuntime
  = fmap snd
  . runWriter @Text
  . void
  . runThrow @ExitCode
  . evalState @[Text] []
  . runRuntimePureC
