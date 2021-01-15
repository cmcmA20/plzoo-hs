module Data.Fin where

import Data.Kind (Type)
import Data.Singletons (sing, Sing, SingI, withSingI)

import Data.Nat

type Fin :: Nat -> Type
data Fin n where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

instance Eq (Fin n) where
  FZ   == FZ    = True
  FS k == FS k' = k == k'
  _    == _     = False

-- FIXME parens
instance Show (Fin n) where
  show FZ      = "FZ"
  show (FS FZ) = "FS FZ"
  show (FS k ) = "FS (" <> show k <> ")"

weakenBound :: forall (n :: Nat). Fin n -> Fin ('S n)
weakenBound FZ     = FZ
weakenBound (FS k) = FS (weakenBound k)

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
