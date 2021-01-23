module Data.Nat where

import           Data.Kind              (Type)
import           Data.Maybe             (fromMaybe)
import           Data.Singletons.Decide
import           Data.Singletons.TH     (genSingletons)
import           Prelude                hiding (pred)
import           Unsafe.Coerce          (unsafeCoerce)

type Nat :: Type
data Nat where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Eq, Ord, Show)

genSingletons [''Nat]

instance SDecide Nat where
  SZ   %~ SZ   = Proved Refl
  SS _ %~ SZ   = Disproved (error "Broken Nat singletons")
  SZ   %~ SS _ = Disproved (error "Broken Nat singletons")
  SS m %~ SS n = case m %~ n of
    Proved    p -> Proved (unsafeCoerce p) -- trust me, I'm a doctor
    Disproved _ -> Disproved (error "Broken Nat singletons")

suc :: Nat -> Nat
suc = S

pred' :: Nat -> Maybe Nat
pred' Z     = Nothing
pred' (S Z) = Just Z
pred' (S n) = case pred' n of
  Nothing -> Nothing
  Just n' -> Just (S n')

pred :: Nat -> Nat
pred = fromMaybe Z . pred'

integerToNat :: Integer -> Nat
integerToNat i
  | i <  0    = Z -- FIXME something more sane?
  | otherwise = integerToNat' i
  where
  integerToNat' 0 = Z
  integerToNat' j = S (integerToNat' (j - 1))

natToInteger :: Nat -> Integer
natToInteger Z     = 0
natToInteger (S n) = 1 + natToInteger n
