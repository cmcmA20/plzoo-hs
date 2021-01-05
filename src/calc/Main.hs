module Main where

import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Monad (void)
import Data.Either (fromRight)
import qualified Data.Text as T

import Eval
import Lexer
import Parser
import Syntax
import Zoo

calcStatic :: LangStatic (Maybe Integer) Exp
calcStatic = MkLangStatic
  { name = "calc"
  , options = []
  , fileParser = Nothing
  , toplevelParser = Just (\s -> fromRight undefined $ runAlex (T.unpack s) calcToplevel) -- FIXME
  , exec = const (Just . eval)
  , printer = T.pack . show
  }

calcDynamic :: LangDynamic (Maybe Integer)
calcDynamic = MkLangDynamic
  { environment = Nothing
  , interactiveShell = True
  , wrapper = Nothing -- FIXME
  , files = []
  }

main :: IO ()
main
  = void
  $ runM
  . runReader calcStatic
  . evalState calcDynamic
  . runError @PLZException
  $ mainPlan @(Maybe Integer) @Exp
