module Main where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.Runtime.Pure
import Control.Effect.Throw
import Data.Text (Text)
import Data.Text qualified as T

import Zoo

type Clo = ()

ln :: LangName
ln = MkLangName "tartlet"

opts :: LangOpts Clo
opts = MkLangOpts $ pure ()

ini :: LangInit Clo ()
ini = MkLangInit $ const ()

toplevelParser :: Has (Throw SyntaxError) sig m => Text -> m ()
toplevelParser t = undefined

fileParser :: Has (Throw SyntaxError) sig m => Text -> m [()]
fileParser t = undefined

exec :: LangExec () () ()
exec = MkLangExec undefined

pp :: LangPP () ()
pp = MkLangPP undefined

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
  $ zooMain @Clo @() @() @()
