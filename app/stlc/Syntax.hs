module Syntax where

import           Data.Kind (Type)
import           Data.Text (Text)

-- | simple types
type Ty :: Type
data Ty where
  TyBool :: Ty -- ^ booleans
  TyNat  :: Ty -- ^ naturals
  TyArr  :: Ty -> Ty -> Ty -- ^ function type
  deriving (Eq, Show)

-- | STLC terms
type Term :: Ty -> Type
data Term tyr where
  TFalse :: Term 'TyBool -- ^ false : Bool
  TTrue  :: Term 'TyBool -- ^ true : Bool
  TIf    :: Term 'TyBool -> Term tyr -> Term tyr -> Term tyr -- ^ if : Bool -> a -> a -> a

  TZ :: Term 'TyNat -- ^ zero : Nat
  TS :: Term 'TyNat -> Term 'TyNat -- ^ successor : Nat -> Nat
  TRec :: Term 'TyNat -> Term tyr -> Term ('TyArr tyr tyr) -> Term tyr -- ^ nat-rec : Nat -> a -> (a -> a) -> a

  TVar :: Text -> Term tyr -- ^ variable of any type
  TLam :: Text -> Ty -> Term tyr -> Term ('TyArr tyq tyr) -- ^ TyArr intro
  TApp :: Term ('TyArr tyq tyr) -> Term tyq -> Term tyr -- ^ TyArr elim
