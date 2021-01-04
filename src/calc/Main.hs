module Main where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict

import Eval
import Syntax
import Zoo

calcStatic :: LangStatic (Maybe Integer) Exp
calcStatic = MkLangStatic
  { name = "calc"
  , options = []
  , initialEnvironment = Nothing
  , fileParser = Nothing
  , toplevelParser = Nothing
  , exec = const (Just . eval) -- \_ c -> Just (eval c)
  }

calcDynamic :: LangDynamic (Maybe Integer)
calcDynamic = MkLangDynamic
  { environment = Nothing
  , interactiveShell = True
  , wrapper = Nothing
  , files = []
  }

main :: IO ()
main
  = runM
  . runReader calcStatic
  . evalState calcDynamic
  $ mainPlan @(Maybe Integer) @Exp
