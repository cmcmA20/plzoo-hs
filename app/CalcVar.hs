module Main where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.Runtime.Pure
import Control.Effect.Throw
import Data.HashMap.Strict qualified as HM
import Data.Text (Text)
import Data.Text qualified as T

import Language.CalcVar.Eval qualified as E
import Language.CalcVar.Lexer qualified as L
import Language.CalcVar.Parser qualified as P
import Language.CalcVar.Syntax qualified as S
import Zoo

type Clo = ()

ln :: LangName
ln = MkLangName "calc_var"

opts :: LangOpts Clo
opts = MkLangOpts $ pure ()

ini :: LangInit Clo E.Ctx
ini = MkLangInit $ const HM.empty

toplevelParser :: Has (Throw SyntaxError) sig m => Text -> m S.Cmd
toplevelParser t = case L.runAlex (T.unpack t) P.toplevel of
  Left  err -> throwError $ alexErrorToSyntaxError err
  Right x   -> pure x

exec :: LangExec S.Cmd E.Sem E.Ctx
exec = MkLangExec E.evalCmd

pp :: LangPP E.Sem E.Ctx
pp = MkLangPP $ \i -> pure $ T.pack $ show i

main :: IO ()
main
  = withoutRuntime
  . runReader ln
  . runReader opts
  . runReader ini
  . runReader (Just $ MkLangParser toplevelParser)
  . runReader @(Maybe (LangParser [S.Cmd])) Nothing
  . runReader exec
  . runReader pp
  $ zooMain @Clo @S.Cmd @E.Sem @E.Ctx
