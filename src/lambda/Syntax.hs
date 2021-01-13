-- | Abstract syntax of internal expressions.
module Syntax where

import Data.Kind (Type)
import Data.Singletons.TH (genSingletons, sing, SingI, withSingI)
import Data.Text (Text)
import qualified Data.Text as T

-- import Zoo

data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Eq, Show)

genSingletons [''Nat]

data Fin :: Nat -> Type where
  FZ :: forall (n :: Nat). Fin ('S n)
  FS :: forall (n :: Nat). Fin n -> Fin ('S n)

instance Eq (Fin n) where
  FZ   == FZ    = True
  FS k == FS k' = k == k'
  _    == _     = False

instance Show (Fin n) where
  show FZ      = "FZ"
  show (FS FZ) = "FS FZ"
  show (FS k ) = "FS (" <> show k <> ")"

weakenFin :: forall (n :: Nat). Fin n -> Fin ('S n)
weakenFin FZ     = FZ
weakenFin (FS k) = FS (weakenFin k)

data Term' :: Nat -> Type where
  TFree  :: forall (n :: Nat). Text -> Term' n
  TBound :: forall (n :: Nat). Fin n -> Term' n
  TLam   :: forall (n :: Nat). Term' ('S n) -> Term' n
  TApp   :: forall (n :: Nat). Term' n -> Term' n -> Term' n
  deriving Eq

instance Show (Term' n) where
  show (TFree name) = T.unpack name
  show (TBound k  ) = showFin' 0 k
    where
    showFin' :: Integer -> Fin m -> String
    showFin' n FZ     = show n
    showFin' n (FS j) = showFin' (1 + n) j
  show (TLam body ) = "λ." <> show body
  show (TApp (TLam body) v) = "(λ." <> show body <> ") " <> show v
  show (TApp u (TApp v w) ) = show u <> " (" <> show (TApp v w) <> ")"
  show (TApp u v          ) = show u <> " " <> show v

weakenCtx :: forall (n :: Nat). Term' n -> Term' ('S n)
weakenCtx (TFree name) = TFree name
weakenCtx (TBound idx) = TBound (weakenFin idx)
weakenCtx (TLam body ) = TLam (weakenCtx body)
weakenCtx (TApp u v  ) = TApp (weakenCtx u) (weakenCtx v)

decOut :: forall (n :: Nat). SingI n => Fin ('S n) -> Maybe (Fin n)
decOut k = case sing @n of
  SZ    -> Nothing
  SS n' -> case k of
    FZ    -> Just FZ
    FS k' -> case withSingI n' decOut k' of
      Nothing -> Nothing
      Just x  -> Just (FS x)

substOut :: forall (n :: Nat). SingI n => Term' n -> Term' ('S n) -> Term' n
substOut _   (TFree name) = TFree name
substOut rep (TBound idx) = maybe rep TBound (decOut idx)
substOut rep (TLam body ) = TLam (substOut (weakenCtx rep) body)
substOut rep (TApp u v  ) = TApp (substOut rep u) (substOut rep v)

newtype Term = MkTerm { unTerm :: Term' 'Z }

-- newtype Term' = MkTerm' { unTerm' :: Located Term'' }
-- 
-- instance Eq Term' where
--   MkTerm' (MkLocated x _) == MkTerm' (MkLocated y _) = x == y
