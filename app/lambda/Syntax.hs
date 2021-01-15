-- | Abstract syntax of internal expressions.
module Syntax where

import Data.Kind (Type)
import Data.Singletons.Decide
import Data.Singletons (sing, Sing, SingI, withSingI)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Fin
import Data.Nat

type Term' :: Nat -> Type
data Term' n where
  TFree  :: Text -> Term' n
  TBound :: Fin n -> Term' n
  TLam   :: Term' ('S n) -> Term' n
  TApp   :: Term' n -> Term' n -> Term' n
  deriving Eq

-- FIXME not optimal and may be plain wrong
instance Show (Term' n) where
  show (TFree name) = T.unpack name
  show (TBound k  ) = showFin' 0 k
    where
    showFin' :: Integer -> Fin m -> String
    showFin' n FZ     = "_" <> show n
    showFin' n (FS j) = showFin' (1 + n) j
  show (TLam body )         = "λ" <> show body
  show (TApp (TLam body) v) = "(λ" <> show body <> ") " <> show v
  show (TApp u (TApp v w) ) = show u <> " (" <> show (TApp v w) <> ")"
  show (TApp u v          ) = "(" <> show u <> ") " <> show v

weakenCtx :: forall (n :: Nat). Term' n -> Term' ('S n)
weakenCtx (TFree name) = TFree name
weakenCtx (TBound idx) = TBound (weakenBound idx)
weakenCtx (TLam body ) = TLam (weakenCtx body)
weakenCtx (TApp u v  ) = TApp (weakenCtx u) (weakenCtx v)

substOut :: forall (n :: Nat). SingI n => Term' n -> Term' ('S n) -> Term' n
substOut _   (TFree name) = TFree name
substOut rep (TBound idx) = maybe rep TBound (strengthenBoundI idx)
substOut rep (TLam body ) = TLam (substOut (weakenCtx rep) body)
substOut rep (TApp u v  ) = TApp (substOut rep u) (substOut rep v)

type SomeTerm :: Type
data SomeTerm where
  MkSomeTerm :: forall (n :: Nat). Sing n -> Term' n -> SomeTerm

mkLam :: SomeTerm -> SomeTerm
mkLam (MkSomeTerm SZ      _) = undefined -- FIXME
mkLam (MkSomeTerm (SS sn) t) = MkSomeTerm sn (TLam t)

mkApp :: SomeTerm -> SomeTerm -> SomeTerm
mkApp (MkSomeTerm sm u) (MkSomeTerm sn v) = case sm %~ sn of
  Proved    Refl -> MkSomeTerm sm (TApp u v)
  Disproved _    -> undefined -- FIXME

instance Show SomeTerm where
  show (MkSomeTerm _ t) = show t

newtype Term = MkTerm { unTerm :: Term' 'Z }
  deriving Show via Term' 'Z

weakest :: forall (n :: Nat). SingI n => Term' 'Z -> Term' n
weakest t = case sing @n of
  SZ    -> t
  SS n' -> withSingI n' (weakenCtx (weakest t))
