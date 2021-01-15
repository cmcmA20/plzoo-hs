module Normalize where

import Control.Effect.Reader
import Data.Singletons (SingI)

import Syntax

data Depth = DShallow | DDeep
  deriving Show

data Energy = ELazy | EEager
  deriving Show

laConst :: Term' 'Z
laConst = TLam (TLam (TBound (FS FZ)))

laWhat :: Term' 'Z
laWhat = TFree "what"

laOmega :: Term' 'Z
laOmega = TLam $ TApp laHmm laHmm
  where
  laHmm :: Term' ('S 'Z)
  laHmm = TLam $ TApp (TBound (FS FZ)) $ TApp (TBound FZ) (TBound FZ)

laWtf :: Term' 'Z
laWtf = TLam $ TApp (TApp (TBound FZ) (TBound FZ)) (TBound FZ)

laDerp :: Term' 'Z
laDerp = TApp (TLam (TFree "derp")) laWtf

normalize'
  :: forall n sig m
  . ( SingI n
    , Has (Reader Depth) sig m
    , Has (Reader Energy) sig m )
  => Term' n -> m (Term' n)
normalize' (TFree name) = pure $ TFree name
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
     , Has (Reader Energy) sig m )
  => Term -> m Term
normalize (MkTerm t) = MkTerm <$> normalize' t
