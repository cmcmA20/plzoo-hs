module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.Runtime.IO
import           Control.Effect.Throw
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Zoo

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
  = runM
  . runRuntimeIOC
  . runReader ln
  . runReader opts
  . runReader ini
  . runReader (Just $ MkLangParser toplevelParser)
  . runReader (Just $ MkLangParser fileParser)
  . runReader exec
  . runReader pp
  $ zooMain @Clo @() @() @()
