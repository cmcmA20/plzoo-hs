module Language.Tartlet.Semantics where

import Control.Carrier.Fresh.Strict
import Control.Carrier.Reader
import Control.Carrier.Throw.Either

import Language.Tartlet.Env
import Language.Tartlet.Name
import Language.Tartlet.Syntax
import Zoo

data Value
  = VClosure (Env Value) !Name !Expr
  | VNeutral Neutral
  deriving Show

data Neutral
  = NVar !Name
  | NApp Neutral !Value
  deriving Show

eval
  :: ( Has (Reader (Env Value)) sig m
     , Has (Throw LangError) sig m )
  => Expr
  -> m Value
eval (Var x) = do
  env <- ask
  lookupVar env x
eval (Lam x body) = do
  env <- ask
  pure $ VClosure env x body
eval (App rator rand) = do
  fun <- eval rator
  arg <- eval rand
  doApply fun arg

doApply :: Has (Throw LangError) sig m => Value -> Value -> m Value
doApply (VClosure env x body) arg = runReader env $ local (extend x arg) $ eval body
doApply (VNeutral neu)        arg = pure $ VNeutral $ NApp neu arg

reify :: ( Has Fresh sig m, Has (Throw LangError) sig m ) => Value -> m Expr
reify (VNeutral (NVar x)) = pure $ Var x
reify (VNeutral (NApp fun arg)) = do
  rator <- reify $ VNeutral fun
  rand <- reify arg
  pure $ App rator rand
reify fun@VClosure{} = do
  x' <- toName <$> fresh
  bodyVal <- doApply fun $ VNeutral $ NVar x'
  bodyExpr <- reify bodyVal
  pure $ Lam x' bodyExpr

normalize :: Has (Throw LangError) sig m => Expr -> m Expr
normalize e = do
  val <- runReader (initEnv @Value) $ eval e
  evalFresh 0 $ reify val

runProgram :: Has (Throw LangError) sig m => [(Name, Expr)] -> Expr -> m Expr
runProgram defs e = do
  env <- addDefs (initEnv @Value) defs
  val <- runReader env $ eval e
  evalFresh 0 $ reify val

addDefs
  :: Has (Throw LangError) sig m
  => Env Value
  -> [(Name, Expr)]
  -> m (Env Value)
addDefs env [] = pure env
addDefs env (((x, e) : defs')) = do
  v <- runReader env $ eval e
  addDefs (extend x v env) defs'
