module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text                    as T

import qualified Eval   as E
import qualified Lexer  as L
import qualified Parser as P
import qualified Syntax as S
import           Zoo

top :: Text -> Either SyntaxError S.Cmd
top t =
  let res = L.runAlex (T.unpack t) P.toplevel
   in case res of
     Left  err -> Left $ MkSyntaxError $ locate Nothing $ T.pack err
     Right x   -> Right x

ex :: RuntimeEnv E.Sem E.Ctx -> S.Cmd -> (Either LangError (E.Sem, RuntimeAction), RuntimeEnv E.Sem E.Ctx)
ex env cmd = case E.eval cmd of
  Left  le -> (Left le, env)
  Right r  -> (Right (r, RANop), env & #replResult .~ Just r)

static :: LangStatic E.Sem E.Ctx S.Cmd
static = MkLangStatic
  { name = "calc"
  , options = []
  , fileParser = Nothing
  , toplevelParser = Just top
  , rts = ex }

dynamic :: LangDynamic E.Sem E.Ctx
dynamic = MkLangDynamic
  { environment = MkRuntimeEnv {context = (), replResult = Nothing}
  , interactiveShell = True
  , wrapper = Just ["rlwrap", "ledit"]
  , files = [] }

main :: IO ()
main
  = runM
  . runReader static
  . evalState dynamic
  $ mainPlan @E.Sem @E.Ctx @S.Cmd
