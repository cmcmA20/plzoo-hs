-- | Abstract syntax of internal expressions.
module Syntax where

import Data.Kind (Type)
import Data.Singletons (Sing, SingI, sing, withSingI)
import Data.Singletons.Decide
import Data.Text (Text)

import Data.Fin
import Data.Nat

-- | Helper type of lambda terms, indexed by scope (represented as a natural)
type Term' :: Nat -> Type
data Term' n where
  TFree  :: Text -> Term' n  -- ^ infinitely many names for free variables
  TBound :: Fin n -> Term' n -- ^ index of a bound variable never points out of the scope
  TLam   :: Term' ('S n) -> Term' n -- ^ scope of a lambda's body is extended
  TApp   :: Term' n -> Term' n -> Term' n -- ^ scopes of application must be same
  deriving (Eq, Show)

-- | Extends scope
weaken :: Term' n -> Term' ('S n)
weaken (TFree name) = TFree name
weaken (TBound idx) = TBound (weakenBound idx)
weaken (TLam body ) = TLam (weaken body)
weaken (TApp u v  ) = TApp (weaken u) (weaken v)

shift :: forall (n :: Nat). SingI n => Nat -> Term' n -> Term' ('S n)
shift _      (TFree name) = TFree name
shift cutOff (TBound idx) = case sing @n of
  SZ    -> error "Broken Term' singletons"
  SS sn -> if withSingI sn finToNatI idx < cutOff
    then weaken $ TBound idx
    else TBound (FS idx)
shift cutOff (TLam body ) = TLam (shift (S cutOff) body)
shift cutOff (TApp u v  ) = TApp (shift cutOff u) (shift cutOff v)

unshift :: forall (n :: Nat). SingI n => Nat -> Term' ('S n) -> Term' n
unshift _      (TFree name) = TFree name
unshift cutOff (TBound idx) = case strengthenBoundI idx of
  Nothing   -> TBound (unsafeFinPredI idx)
  Just idx' -> if finToNatI idx < cutOff
    then TBound idx'
    else TBound (unsafeFinPredI idx)
unshift cutOff (TLam body ) = TLam (unshift (S cutOff) body)
unshift cutOff (TApp u v  ) = TApp (unshift cutOff u) (unshift cutOff v)

-- | Opens a term with a replacement term at level m
open :: forall (n :: Nat). SingI n => Nat -> Term' ('S n) -> Term' ('S n) -> Term' ('S n)
open _ _   (TFree name) = TFree name
open m rep (TBound idx) =
  if finToNatI idx == m
     then rep
     else TBound idx
open m rep (TLam body ) = TLam $ open (S m) (shift Z rep) body
open m rep (TApp u v  ) = TApp (open m rep u) (open m rep v)

-- | An existential for any helper
type SomeTerm :: Type
data SomeTerm where
  MkSomeTerm :: forall (n :: Nat). Sing n -> Term' n -> SomeTerm

unsafeMkLam :: SomeTerm -> SomeTerm
unsafeMkLam (MkSomeTerm SZ      _) = error "Broken SomeTerm singletons"
unsafeMkLam (MkSomeTerm (SS sn) t) = MkSomeTerm sn (TLam t)

unsafeMkApp :: SomeTerm -> SomeTerm -> SomeTerm
unsafeMkApp (MkSomeTerm sm u) (MkSomeTerm sn v) = case sm %~ sn of
  Proved    Refl -> MkSomeTerm sm (TApp u v)
  Disproved _    -> error "Broken SomeTerm singletons"

instance Show SomeTerm where
  show (MkSomeTerm _ t) = show t

-- | Locally closed lambda term
newtype Term = MkTerm { unTerm :: Term' 'Z }
  deriving Show via Term' 'Z

weakestTermBoundI :: forall n. SingI n => Term' 'Z -> Term' n
weakestTermBoundI t = case sing @n of
  SZ    -> t
  SS n' -> withSingI n' (weaken (weakestTermBoundI t))
