module Control.Carrier.Runtime.Pure
  ( module Control.Effect.Runtime
  , RuntimePureC(..)
  ) where

import           Control.Algebra
import           Control.Effect.Runtime
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Kind                  (Type)
import           Data.Text                  (Text)
import           System.Exit                (ExitCode (..))

type CarrierKind = Type -> Type

type InputStream  = [Text]
type OutputStream = [Text]

-- completely ignores temporal aspect of IO
newtype RuntimePureC (m :: CarrierKind) (a :: Type) =
  MkRuntimePureC { runRuntimePureC :: InputStream -> m ((Either ExitCode a, InputStream), OutputStream) }
  deriving (Functor, Applicative, Monad) via (ExceptT ExitCode (StateT InputStream (WriterT OutputStream m)))

-- seems clunky
instance Algebra sig m => Algebra (Runtime :+: sig) (RuntimePureC m) where
 alg _   (L (RExit code)) _  = MkRuntimePureC \is -> pure ((Left code, is), [])
 alg _   (L RRead)        ct = MkRuntimePureC \case
   []     -> pure ((Left (ExitFailure 1), []), ["Program blocked on a reading operation"]) -- an artificial stuff
   (i:is) -> pure ((Right (i <$ ct), is), [])
 alg _   (L (RWrite t))   ct = MkRuntimePureC \is -> pure ((Right ct, is), [t])
 alg hdl (R other)        ct = undefined -- FIXME
