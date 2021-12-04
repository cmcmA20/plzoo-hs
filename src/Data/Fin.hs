module Data.Fin where

import Data.Kind (Type)
import Data.Singletons (Sing, SingI, sing, withSingI)

import Data.Nat

type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

instance Eq (Fin n) where
  FZ   == FZ    = True
  FS k == FS k' = k == k'
  _    == _     = False

instance Show (Fin n) where
  show FZ      = "FZ"
  show (FS FZ) = "FS FZ"
  show (FS k ) = "FS (" <> show k <> ")"

unsafeFinPredI :: forall (n :: Nat). SingI n => Fin ('S n) -> Fin n
unsafeFinPredI k = case sing @n of
  SZ   -> error "Broken Fin singletons"
  SS _ -> case k of
    FZ    -> FZ
    FS k' -> k'

-- | index stays the same, bound is loosened
weakenBound :: forall (n :: Nat). Fin n -> Fin ('S n)
weakenBound FZ     = FZ
weakenBound (FS k) = FS (weakenBound k)

-- | index stays the same (if possible), bound is tightened
strengthenBoundI :: forall (n :: Nat). SingI n => Fin ('S n) -> Maybe (Fin n)
strengthenBoundI k = case sing @n of
  SZ    -> Nothing
  SS n' -> case k of
    FZ    -> Just FZ
    FS k' -> case withSingI n' strengthenBoundI k' of
      Nothing -> Nothing
      Just x  -> Just (FS x)

finToNatI :: forall (n :: Nat). SingI n => Fin ('S n) -> Nat
finToNatI k = case sing @n of
  SZ    -> Z
  SS n' -> case k of
    FZ    -> Z
    FS k' -> S (withSingI n' finToNatI k')

natToFin :: forall (n :: Nat). Sing n -> Nat -> Maybe (Fin ('S n))
natToFin _        Z     = Just FZ
natToFin SZ       (S _) = Nothing
natToFin (SS sn') (S j) = case natToFin sn' j of
  Nothing -> Nothing
  Just k  -> Just (FS k)
