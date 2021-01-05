module Main where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T

import Eval
import Lexer
import Parser
import Syntax
import Zoo

type Env = Maybe Integer

showEnv :: Env -> Text
showEnv Nothing  = ""
showEnv (Just i) = T.pack $ show i

calcStatic :: LangStatic Env Exp
calcStatic = MkLangStatic
  { name = "calc"
  , options = []
  , fileParser = Nothing
  , toplevelParser = Just (\s -> fromRight (Numeral 0) $ runAlex (T.unpack s) calc) -- FIXME
  , exec = const (Just . eval)
  , printer = showEnv }

calcDynamic :: LangDynamic Env
calcDynamic = MkLangDynamic
  { environment = Nothing
  , interactiveShell = True
  , wrapper = Just ["rlwrap", "ledit"]
  , files = [] }

main :: IO ()
main
  = runM
  . runReader calcStatic
  . evalState calcDynamic
  $ mainPlan @Env @Exp
