module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Compile as C
import qualified Lexer   as L
import qualified Parser  as P
import qualified Syntax  as S
import           Zoo

file :: Text -> Either SyntaxError [S.Cmd]
file t =
  let res = L.runAlex (T.unpack t) P.file
   in case res of
     Left  err -> Left $ alexErrorToSyntaxError err
     Right x   -> Right x

top :: Text -> Either SyntaxError S.Cmd
top t =
  let res = L.runAlex (T.unpack t) P.program
   in case res of
     Left  err -> Left $ alexErrorToSyntaxError err
     Right x   -> Right x

static :: LangStatic C.Sem C.Ctx S.Cmd
static = MkLangStatic
  { name = "comm"
  , options = []
  , fileParser = Just file
  , toplevelParser = Just top
  , rts = \e _ -> (Left (LERuntime $ locate Nothing "NYI"), e)}

dynamic :: LangDynamic C.Sem C.Ctx
dynamic = MkLangDynamic
  { environment = mkRuntimeEnv ()
  , interactiveShell = True
  , wrapper = defaultWrapper
  , files = [] }

main :: IO ()
main
  = runM
  . runReader static
  . evalState dynamic
  $ mainPlan @C.Sem @C.Ctx @S.Cmd

