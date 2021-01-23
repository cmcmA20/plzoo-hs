module Control.Carrier.Runtime.Pure
  ( module Control.Effect.Runtime
  , RuntimePureC(..)
  ) where

import           Control.Algebra
import           Control.Effect.Runtime
import           Data.Kind              (Type)

type CarrierKind = Type -> Type

-- TODO: make it take a Text input (representing stdin) and return a Text (representing stdout)

-- | drops any I/O
newtype RuntimePureC (m :: CarrierKind) (a :: Type) = MkRuntimePureC { runRuntimePureC :: m a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Runtime :+: sig) (RuntimePureC m) where
  alg hdl si ct = MkRuntimePureC case si of
    L RExit      -> pure ct -- FIXME feels wrong
    L RRead      -> pure ("" <$ ct)
    L (RWrite _) -> pure ct
    R other      -> alg (runRuntimePureC . hdl) other ct
