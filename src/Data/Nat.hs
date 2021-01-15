module Data.Nat where

import Data.Kind (Type)
import Data.Singletons.TH (genSingletons)

type Nat :: Type
data Nat where
  Z :: Nat
  S :: Nat -> Nat
  deriving (Eq, Show)

genSingletons [''Nat]
