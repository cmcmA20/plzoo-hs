-- | Abstract syntax of internal expressions.
module Syntax where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Numeric.Natural

import GHC.TypeLits (Nat)

import Zoo

-- | Locally nameless but Haskell has no deptypes
data Term'' where
  TFree  :: Text -> Term''
  TBound :: Natural -> Term''
  TLam   :: Term'' -> Term''
  TApp   :: Term'' -> Term'' -> Term''
  deriving (Eq, Show)

-- newtype Term = MkTerm { unTerm :: Located Term' }
-- 
-- instance Eq Term where
--   MkTerm (MkLocated x _) == MkTerm (MkLocated y _) = x == y

-- substOut :: Term' -> Term' -> Term'
-- substOut _   (TFree 0 n ) = error "waiting for -XDependentTypes"
-- substOut _   (TFree l n ) = TFree (l - 1) n
-- substOut rep (TBound l i) = fromMaybe rep (decOut (TBound i))
-- substOut rep (TLam l t  ) = TLam $ substOut (weaken rep) t
-- substOut rep (TApp l s t) = TApp (substOut rep s) (substOut rep t)
