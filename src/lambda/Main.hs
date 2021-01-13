module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Effect.Throw
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Command as Cmd
import qualified Context as Con
import qualified Normalize as N
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
toplevelParser = undefined -- FIXME

exec :: LangExec Cmd.Cmd Cmd.Sem Cmd.ExCtx
exec = MkLangExec Cmd.evalCmd

pp :: LangPP Cmd.Sem Cmd.ExCtx
pp = MkLangPP ppHelper
  where
  ppHelper :: Has (Reader Cmd.ExCtx) sig m => Cmd.Sem -> m Text
  ppHelper (Left  text) = pure text
  ppHelper (Right term) = pure $ T.pack $ show term

main :: IO ()
main
  = runM
  . runRuntimeIOC
  . runReader ln
  . runReader opts
  . runReader ini
  . runReader (Just $ MkLangParser toplevelParser)
  . runReader @(Maybe (LangParser [Cmd.Cmd])) Nothing
  . runReader exec
  . runReader pp
  $ zooMain @Clo @Cmd.Cmd @Cmd.Sem @Cmd.ExCtx
