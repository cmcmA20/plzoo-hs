module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Data.Text (Text)
import qualified Data.Text                    as T

import qualified Eval   as E
import qualified Lexer  as L
import qualified Parser as P
import qualified Syntax as S
import           Zoo

type Env = Maybe Integer

showEnv :: Env -> Text
showEnv = foldMap $ T.pack . show

top :: Text -> S.Exp
top t =
  let res = L.runAlex (T.unpack t) P.toplevel
   in case res of
     Left  err -> raiseErrorClassic EKSyntax LNowhere (T.pack err)
     Right x   -> x

calcStatic :: LangStatic Env S.Exp
calcStatic = MkLangStatic
  { name = "calc"
  , options = []
  , fileParser = Nothing
  , toplevelParser = Just top
  , exec = const (Just . E.eval)
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
  $ mainPlan @Env @S.Exp
