{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Printer where

import           Data.Text (Text)
import qualified Data.Text as T
import           Syntax

showType :: Ty -> Text
showType TyBool      = "Bool"
showType TyNat       = "Nat"
showType (TyArr u v) = showType u <> " -> " <> showType v

-- looks like GHC still can't analyze pattern completeness for GADTs
tyNatToInt:: Term 'TyNat -> Integer
tyNatToInt TZ     = 0
tyNatToInt (TS n) = 1 + tyNatToInt n -- FIXME inefficient

-- TODO refactor zoo printer to handle parens properly
showTerm :: forall ty. Term ty -> Text
showTerm TFalse = "false"
showTerm TTrue  = "true"
showTerm (TIf cond t f) = "if " <> showTerm cond <> " then " <> showTerm t <> " else " <> showTerm f
showTerm TZ = "0"
showTerm (TS tt) = T.pack $ show $ tyNatToInt (TS tt)
showTerm (TRec n z s) = "nat-rec " <> showTerm n <> " base " <> showTerm z <> " step " <> showTerm s
showTerm (TVar name) = name
showTerm (TLam name ty body) = "λ" <> name <> ":" <> showType ty <> "." <> showTerm body
showTerm (TApp fun arg) = showTerm fun <> " " <> showTerm arg
