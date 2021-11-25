module Semantics where

import Control.Carrier.Fresh.Strict
import Control.Carrier.Reader
import Control.Carrier.Throw.Either
import Data.Char (chr, ord)
import Data.Text qualified as T

import Syntax
import Zoo

newtype Env a = MkEnv [(Name, a)]
  deriving (Functor, Show)

initEnv :: Env a
initEnv = MkEnv []

lookupVar :: Has (Throw LangError) sig m => Env a -> Name -> m a
lookupVar (MkEnv []) (MkName x) = throwError (LERuntime $ locate Nothing $ "Unknown identifier " <> x)
lookupVar (MkEnv ((y, val) : env')) x
  | y == x    = pure val
  | otherwise = lookupVar (MkEnv env') x

extend :: Name -> a -> Env a -> Env a
extend x v (MkEnv env) = MkEnv ((x, v) : env)

data Value
  = VClosure (Env Value) Name Expr
  | VNeutral Neutral
  deriving Show

data Neutral
  = NVar Name
  | NApp Neutral Value
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

toName :: Int -> Name
toName i
  | i < 0     = error "name generation"
  | otherwise = MkName $ T.singleton (chr (ord 'a' + i `mod` 26)) <> "#" <> T.pack (show $ i `div` 26)

reify :: ( Has Fresh sig m, Has (Throw LangError) sig m ) => Value -> m Expr
reify (VNeutral (NVar x)) = pure $ Var x
reify (VNeutral (NApp fun arg)) = do
  rator <- reify $ VNeutral fun
  rand <- reify arg
  pure $ App rator rand
reify fun@(VClosure _ _ _) = do
  x' <- toName <$> fresh
  bodyVal <- doApply fun $ VNeutral $ NVar x'
  bodyExpr <- reify bodyVal
  pure $ Lam x' bodyExpr

normalize :: Has (Throw LangError) sig m => Expr -> m Expr
normalize e = do
  val <- runReader (initEnv @Value) $ eval e
  evalFresh 0 $ reify val

runProgram :: Has (Throw LangError) sig m => Env Expr -> Expr -> m Expr
runProgram defs e = do
  env <- runReader (initEnv @Value) $ addDefs defs
  val <- runReader env $ eval e
  evalFresh 0 $ reify val

addDefs :: ( Has (Reader (Env Value)) sig m, Has (Throw LangError) sig m ) => Env Expr -> m (Env Value)
addDefs (MkEnv []) = ask
addDefs (MkEnv ((x, e) : defs')) = do
  v <- eval e
  local (extend x v) $ addDefs (MkEnv defs')

test :: Either LangError Expr
test = run $ runThrow $ runProgram churchDefs $
  App (App (Var (MkName "+")) (toChurch 2)) (toChurch 3)

toChurch :: Integer -> Expr
toChurch n
  | n <= 0    = Var (MkName "zero")
  | otherwise = App (Var $ MkName "suc") (toChurch $ n - 1)

churchDefs :: Env Expr
churchDefs = MkEnv $
  [ ( MkName "zero"
    , Lam (MkName "f") $ Lam (MkName "x") $ Var $ MkName "x" )
  , ( MkName "suc"
    , Lam (MkName "n") $ Lam (MkName "f") $ Lam (MkName "x") $
        App (Var $ MkName "f") $ App (App (Var $ MkName "n") (Var $ MkName "f")) $ Var $ MkName "x"
    )
  , ( MkName "+"
    , Lam (MkName "j") $ Lam (MkName "k") $ Lam (MkName "f") $ Lam (MkName "x") $
        App (App (Var $ MkName "j") $ Var $ MkName "f") $
          App (App (Var $ MkName "k") $ Var $ MkName "f") $ Var $ MkName "x"
    )
  ]
