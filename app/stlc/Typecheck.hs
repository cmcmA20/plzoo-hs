module Typecheck where

import           Control.Effect.Reader
import           Control.Effect.Throw
import           Control.Monad         (when)
import           Data.Coerce           (coerce)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.Text             (Text)
import           Printer
import           Syntax
import           Zoo

data TypedTerm where
  MkTT :: Term ty -> Ty -> TypedTerm

newtype TypeCtx = MkTypeCtx { unTypeCtx :: HashMap Text Ty }

check
  :: ( Has (Reader TypeCtx) sig m
     , Has (Throw LangError) sig m )
  => Ty
  -> Term ty
  -> m ()
check tNeed t = do
  tHas <- typeOf t
  when (tHas /= tNeed) $ throwError $ LEType $ locate Nothing $
    "Couldn't match expected type ‘" <> showType tNeed <> "’ with actual type ‘" <> showType tHas <> "’"

typeOf
  :: ( Has (Reader TypeCtx) sig m
     , Has (Throw LangError) sig m )
  => Term ty
  -> m Ty
typeOf TFalse = pure TyBool
typeOf TTrue  = pure TyBool
typeOf (TIf _ t f) = do
--   check TyBool cond
  tt <- typeOf t
  check tt f
  pure tt
typeOf TZ = pure TyNat
typeOf (TS _) = do
--   check TyNat n
  pure TyNat
typeOf (TRec _ z _) = do
--   check TyNat n
  tz <- typeOf z
--   check (TyArr tz tz) s
  pure tz
typeOf (TVar name) = do
  ctx <- asks @TypeCtx coerce
  maybeThrow (LEType $ locate Nothing $ "Variable not in scope: " <> name) $
    HM.lookup name ctx
typeOf (TLam name typ body) = do
  tb <- local @TypeCtx (coerce . HM.insert name typ . coerce) $ typeOf body
  pure $ TyArr typ tb
typeOf (TApp fun _) = do
  tf <- typeOf fun
  case tf of
    TyArr _ v -> do
--       check u arg
      pure v
    _         -> throwError (LEType $ locate Nothing "Never gonna give you up")
