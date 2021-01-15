module Data.Fin where

import Data.Kind (Type)
import Data.Singletons (sing, SingI, withSingI)

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

weakenFin :: forall (n :: Nat). Fin n -> Fin ('S n)
weakenFin FZ     = FZ
weakenFin (FS k) = FS (weakenFin k)

strengthenFin :: forall (n :: Nat). SingI n => Fin ('S n) -> Maybe (Fin n)
strengthenFin k = case sing @n of
  SZ    -> Nothing
  SS n' -> case k of
    FZ    -> Just FZ
    FS k' -> case withSingI n' strengthenFin k' of
      Nothing -> Nothing
      Just x  -> Just (FS x)

finToNat :: forall (n :: Nat). SingI n => Fin ('S n) -> Nat
finToNat k = case sing @n of
  SZ    -> Z
  SS n' -> case k of
    FZ    -> Z
    FS k' -> S (withSingI n' finToNat k')

natToFin :: forall (n :: Nat). SingI n => Fin ('S n)
natToFin = case sing @n of
  SZ    -> FZ
  SS n' -> FS (withSingI n' natToFin)
