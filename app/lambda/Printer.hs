module Printer where

import           Data.Singletons ( sing, Sing, SingI, withSingI )
import           Data.Text ( Text )
import qualified Data.Text as T

import Data.Fin
import Data.Nat
import Syntax

showFin :: Sing n -> Fin ('S n) -> Text
showFin sn = T.pack . show . natToInteger . withSingI sn finToNatI

showTerm' :: forall (n :: Nat). SingI n => Nat -> Term' n -> Text
showTerm' _ (TFree name)        = name
showTerm' _ (TBound idx)        = case sing @n of
  SZ     -> error "Broken Term singletons"
  SS sn' -> "_" <> showFin sn' idx
showTerm' Z (TLam (TLam body))  = "λ" <> showTerm' Z (TLam body)
showTerm' n (TLam (TLam body))  = "(λ" <> showTerm' Z (TLam body) <> ")"
showTerm' Z (TLam body )        = "λ " <> showTerm' Z body
showTerm' n (TLam body )        = "(λ " <> showTerm' Z body <> ")"
showTerm' Z (TApp (TApp u v) w) = showTerm' Z u <> " " <> showTerm' (S Z) v <> " " <> showTerm' (S Z) w
showTerm' n (TApp (TApp u v) w) = "(" <> showTerm' n u <> " " <> showTerm' (S n) v <> " " <> showTerm' (S n) w <> ")"
showTerm' Z (TApp u v)          = showTerm' (S Z) u <> " " <> showTerm' (S Z) v
showTerm' n (TApp u v)          = "(" <> showTerm' (S n) u <> " " <> showTerm' (S n) v <> ")"

showTerm :: Term -> Text
showTerm (MkTerm t) = showTerm' Z t
