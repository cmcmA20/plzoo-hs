module Main where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Data.Text (Text)
import qualified Data.Text as T

import Eval
import Lexer
import Parser
import Syntax
import Zoo

type Env = Maybe Integer

showEnv :: Env -> Text
showEnv = foldMap $ T.pack . show

top :: Text -> Exp
top t =
  let res = runAlex (T.unpack t) calc
   in case res of
     Left  err -> raiseErrorClassic EKSyntax LNowhere (T.pack err)
     Right x   -> x

calcStatic :: LangStatic Env Exp
calcStatic = MkLangStatic
  { name = "calc"
  , options = []
  , fileParser = Nothing
  , toplevelParser = Just top
  , exec = const (Just . eval)
  , prettyPrinter = showEnv }

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
