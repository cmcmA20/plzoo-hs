-- | Abstract syntax of internal expressions.
module Syntax where

import Data.Kind (Type)
import Data.Singletons.Decide
import Data.Singletons (sing, Sing, SingI, withSingI)
import Data.Text (Text)

import Data.Fin
import Data.Nat

type Term' :: Nat -> Type
data Term' n where
  TFree  :: Text -> Term' n
  TBound :: Fin n -> Term' n
  TLam   :: Term' ('S n) -> Term' n
  TApp   :: Term' n -> Term' n -> Term' n
  deriving (Eq, Show)

weakenTermBound :: forall (n :: Nat). Term' n -> Term' ('S n)
weakenTermBound (TFree name) = TFree name
weakenTermBound (TBound idx) = TBound (weakenBound idx)
weakenTermBound (TLam body ) = TLam (weakenTermBound body)
weakenTermBound (TApp u v  ) = TApp (weakenTermBound u) (weakenTermBound v)

substOutI :: forall (n :: Nat). SingI n => Term' n -> Term' ('S n) -> Term' n
substOutI _   (TFree name) = TFree name
substOutI rep (TBound idx) = maybe rep TBound (strengthenBoundI idx)
substOutI rep (TLam body ) = TLam (substOutI (weakenTermBound rep) body)
substOutI rep (TApp u v  ) = TApp (substOutI rep u) (substOutI rep v)

type SomeTerm :: Type
data SomeTerm where
  MkSomeTerm :: forall (n :: Nat). Sing n -> Term' n -> SomeTerm

mkLam :: SomeTerm -> SomeTerm
mkLam (MkSomeTerm SZ      _) = error "Broken SomeTerm singletons"
mkLam (MkSomeTerm (SS sn) t) = MkSomeTerm sn (TLam t)

mkApp :: SomeTerm -> SomeTerm -> SomeTerm
mkApp (MkSomeTerm sm u) (MkSomeTerm sn v) = case sm %~ sn of
  Proved    Refl -> MkSomeTerm sm (TApp u v)
  Disproved _    -> error "Broken SomeTerm singletons"

instance Show SomeTerm where
  show (MkSomeTerm _ t) = show t

newtype Term = MkTerm { unTerm :: Term' 'Z }
  deriving Show via Term' 'Z

weakestTermBoundI :: forall (n :: Nat). SingI n => Term' 'Z -> Term' n
weakestTermBoundI t = case sing @n of
  SZ    -> t
  SS n' -> withSingI n' (weakenTermBound (weakestTermBoundI t))

weakenTermIndex :: forall (n :: Nat). Sing n -> Term' n -> Term' ('S n)
weakenTermIndex _  (TFree name) = TFree name
weakenTermIndex _  (TBound idx) = TBound (FS idx)
weakenTermIndex sn (TLam body ) = TLam (weakenTermIndex (SS sn) body)
weakenTermIndex sn (TApp u v  ) = TApp (weakenTermIndex sn u) (weakenTermIndex sn v)

weakestTermIndex :: forall (n :: Nat). Sing n -> Term' 'Z -> Term' n
weakestTermIndex SZ      t = t
weakestTermIndex (SS n') t = weakenTermIndex n' (weakestTermIndex n' t)
