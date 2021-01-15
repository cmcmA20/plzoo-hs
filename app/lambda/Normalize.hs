module Normalize where

import Control.Effect.Reader
import Control.Effect.Throw
import Data.Singletons (SingI)

import Context
import Syntax
import Zoo

data Depth = DShallow | DDeep
  deriving Show

data Energy = ELazy | EEager
  deriving Show

normalize'
  :: forall n sig m
  . ( SingI n
    , Has (Reader Depth) sig m
    , Has (Reader Energy) sig m
    , Has (Reader Ctx) sig m
    , Has (Throw LangError) sig m )
  => Term' n -> m (Term' n)
normalize' (TFree name) = do
  md <- lookupSafe name
  case md of
    Nothing                 ->
      throwError $ LERuntime $ locate Nothing "unknown identifier"
    Just DConst             -> pure $ TFree name
    Just (DTerm (MkTerm t)) -> weakest <$> normalize' t
normalize' (TBound k  ) = pure $ TBound k
normalize' (TLam body ) = do
  d <- ask @Depth
  case d of
    DShallow -> pure $ TLam body
    DDeep    -> TLam <$> normalize' body
normalize' (TApp s t  ) = do
  e <- ask @Energy
  t' <- case e of
    ELazy  -> pure t
    EEager -> normalize' t
  s' <- normalize' s
  case s' of
    TLam body -> pure $ substOut t' body
    _         -> pure $ TApp s' t'

normalize
  :: ( Has (Reader Depth) sig m
     , Has (Reader Energy) sig m
     , Has (Reader Ctx) sig m
     , Has (Throw LangError) sig m )
  => Term -> m Term
normalize (MkTerm t) = MkTerm <$> normalize' t
