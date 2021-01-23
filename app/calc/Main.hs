module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.Runtime.Pure
import           Control.Effect.Throw
import           Data.Text                    (Text)
import qualified Data.Text                    as T

import qualified Eval                         as E
import qualified Lexer                        as L
import qualified Parser                       as P
import qualified Syntax                       as S
import           Zoo

type Clo = ()

ln :: LangName
ln = MkLangName "calc"

opts :: LangOpts Clo
opts = MkLangOpts $ pure ()

ini :: LangInit Clo E.Ctx
ini = MkLangInit id

toplevelParser :: Has (Throw SyntaxError) sig m => Text -> m S.Cmd
toplevelParser t = case L.runAlex (T.unpack t) P.toplevel of
  Left  err -> throwError $ alexErrorToSyntaxError err
  Right x   -> pure x

exec :: LangExec S.Cmd E.Sem E.Ctx
exec = MkLangExec E.eval

pp :: LangPP E.Sem E.Ctx
pp = MkLangPP $ \i -> pure $ T.pack $ show i

main :: IO ()
main
  = runM
  . runRuntimePureC
  . runReader ln
  . runReader opts
  . runReader ini
  . runReader (Just $ MkLangParser toplevelParser)
  . runReader @(Maybe (LangParser [S.Cmd])) Nothing
  . runReader exec
  . runReader pp
  $ zooMain @Clo @S.Cmd @E.Sem @E.Ctx
