module Main where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.Runtime.Pure
import Control.Effect.Throw
import Data.Text (Text)
import Data.Text qualified as T

import Data.Nat
import Language.UTLC.Command qualified as Cmd
import Language.UTLC.Context qualified as Con
import Language.UTLC.Lexer qualified as L
import Language.UTLC.Normalize qualified as N
import Language.UTLC.Parser qualified as P
import Language.UTLC.Printer qualified as P
import Zoo

type Clo = ()

ln :: LangName
ln = MkLangName "utlc"

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
  = withoutRuntime
  . runReader ln
  . runReader opts
  . runReader ini
  . runReader (Just $ MkLangParser toplevelParser)
  . runReader (Just $ MkLangParser fileParser)
  . runReader exec
  . runReader pp
  $ zooMain @Clo @Cmd.Cmd @Cmd.Sem @Cmd.ExCtx
