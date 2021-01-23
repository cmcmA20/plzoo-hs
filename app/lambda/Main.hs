module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.Runtime.IO
import           Control.Effect.Throw
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import qualified Command                    as Cmd
import qualified Context                    as Con
import           Data.Nat
import qualified Lexer                      as L
import qualified Normalize                  as N
import qualified Parser                     as P
import qualified Printer                    as P
import           Zoo

type Clo = ()

ln :: LangName
ln = MkLangName "lambda"

opts :: LangOpts Clo
opts = MkLangOpts $ pure ()

ini :: LangInit Clo Cmd.ExCtx
ini = MkLangInit $ const $ Cmd.MkExCtx
  { Cmd.energy = N.ELazy, Cmd.depth = N.DShallow, Cmd.ctx = Con.empty }

toplevelParser :: Has (Throw SyntaxError) sig m => Text -> m Cmd.Cmd
toplevelParser t = do
  case L.runAlex (T.unpack t) L.alexScanAll of
    Left  _    -> throwError $ SELex LNowhere -- FIXME
    Right toks -> runParser Z toks (P.commandLine <* pEOF)

fileParser :: Has (Throw SyntaxError) sig m => Text -> m [Cmd.Cmd]
fileParser t = do
  case L.runAlex (T.unpack t) L.alexScanAll of
    Left  _    -> throwError $ SELex LNowhere -- FIXME
    Right toks -> runParser Z toks (P.file <* pEOF)

exec :: LangExec Cmd.Cmd Cmd.Sem Cmd.ExCtx
exec = MkLangExec Cmd.evalCmd

pp :: LangPP Cmd.Sem Cmd.ExCtx
pp = MkLangPP ppHelper
  where
  ppHelper :: Has (Reader Cmd.ExCtx) sig m => Cmd.Sem -> m Text
  ppHelper (Left  text) = pure text
  ppHelper (Right term) = pure $ P.showTerm term

main :: IO ()
main
  = runM
  . runRuntimeIOC
  . runReader ln
  . runReader opts
  . runReader ini
  . runReader (Just $ MkLangParser toplevelParser)
  . runReader (Just $ MkLangParser fileParser)
  . runReader exec
  . runReader pp
  $ zooMain @Clo @Cmd.Cmd @Cmd.Sem @Cmd.ExCtx
