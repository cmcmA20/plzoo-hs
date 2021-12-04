module Main where

import Control.Carrier.Fresh.Strict
import Control.Carrier.Reader
import Control.Carrier.Throw.Either

import Language.Tartlet.Env
import Language.Tartlet.Name
import Language.Tartlet.Semantics
import Language.Tartlet.Syntax
import Zoo

test :: Either LangError Expr
test = run $ runThrow $ runProgram churchDefs $
  App (App (Var (MkName "+")) (toChurch 2)) (toChurch 3)

toChurch :: Integer -> Expr
toChurch n
  | n <= 0    = Var (MkName "zero")
  | otherwise = App (Var $ MkName "suc") (toChurch $ n - 1)

churchDefs :: [(Name, Expr)]
churchDefs =
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

main :: IO ()
main = print "awsad"
