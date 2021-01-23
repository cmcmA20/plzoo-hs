-- | Abstract syntax of internal expressions.
module Syntax where

import           Data.Kind              (Type)
import           Data.Singletons        (Sing, SingI, sing, withSingI)
import           Data.Singletons.Decide
import           Data.Text              (Text)

import           Data.Fin
import           Data.Nat

-- | Helper type of lambda terms, indexed by local context (represented as a natural)
type Term' :: Nat -> Type
data Term' n where
  TFree  :: Text -> Term' n  -- ^ infinitely many names for free variables
  TBound :: Fin n -> Term' n -- ^ index of a bound variable never points out of the local context
  TLam   :: Term' ('S n) -> Term' n -- ^ local context of a lambda's body is extended
  TApp   :: Term' n -> Term' n -> Term' n -- ^ local contexts of application must be same
  deriving (Eq, Show)

-- | Extends local context
weaken :: Term' n -> Term' ('S n)
weaken (TFree name) = TFree name
weaken (TBound idx) = TBound (weakenBound idx)
weaken (TLam body ) = TLam (weaken body)
weaken (TApp u v  ) = TApp (weaken u) (weaken v)

open' :: forall (n :: Nat). SingI n => Fin ('S n) -> Term' n -> Term' ('S n) -> Term' n
open' _ _   (TFree name) = TFree name
open' k rep (TBound idx) =
  if k == idx
     then rep
     else case strengthenBoundI idx of
       Nothing   -> error "Broken Term' singletons"
       Just idx' -> TBound idx'
open' k rep (TLam body ) = TLam (open' (FS k) (weaken rep) body)
open' k rep (TApp u v  ) = TApp (open' k rep u) (open' k rep v)

-- | Opens a term with a replacement term
open :: forall (n :: Nat). SingI n => Term' n -> Term' ('S n) -> Term' n
open = open' FZ

-- | Opens a term with a free variable
varOpen :: forall (n :: Nat). SingI n => Text -> Term' ('S n) -> Term' n
varOpen name = open (TFree name)

varClose' :: forall (n :: Nat). SingI n => Fin ('S n) -> Text -> Term' n -> Term' ('S n)
varClose' k old (TFree name) =
  if old == name
     then TBound k
     else TFree name
varClose' _ _   (TBound idx) = TBound (weakenBound idx)
varClose' k old (TLam body ) = TLam (varClose' (FS k) old body)
varClose' k old (TApp u v  ) = TApp (varClose' k old u) (varClose' k old v)

-- | Closes a term with respect to a free variable
varClose :: SingI n => Text -> Term' n -> Term' ('S n)
varClose = varClose' FZ

-- | Substitute a variable with a term
substitute :: SingI n => Term' n -> Text -> Term' n -> Term' n
substitute rep old = open rep . varClose old

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
