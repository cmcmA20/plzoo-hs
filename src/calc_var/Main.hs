{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Lens
import qualified Data.HashMap.Strict          as HM
import           Data.Text (Text)
import qualified Data.Text                    as T
import           System.IO.Unsafe (unsafePerformIO)

import qualified Eval   as E
import qualified Lexer  as L
import qualified Parser as P
import qualified Syntax as S
import           Zoo

showEnv :: E.Env -> Text
showEnv = (^. #replResult . to (T.pack . show))

top :: Text -> S.Cmd
top t =
  let res = L.runAlex (T.unpack t) P.toplevel
   in case res of
     Left  err -> raiseErrorClassic EKSyntax LNowhere (T.pack err)
     Right x   -> x

ex :: (Has (State E.Env) sig m, Has (Lift IO) sig m ) => S.Cmd -> m ()
ex (S.Expression e  ) = do
  r <- E.eval e
  modify @E.Env (& #replResult ?~ r)

ex (S.Definition n e) = do
  r <- E.eval e
  modify @E.Env (& #context %~ HM.insert n r)

static :: LangStatic E.Env S.Cmd
static = MkLangStatic
  { name = "calc_var"
  , options = []
  , fileParser = Nothing
  , toplevelParser = Just top
  , exec = \e c -> unsafePerformIO $ runM $ execState e $ ex c
  , prettyPrinter = showEnv }

dynamic :: LangDynamic E.Env
dynamic = MkLangDynamic
  { environment = E.MkEnv {E.context = HM.empty, E.replResult = Nothing}
  , interactiveShell = True
  , wrapper = Just ["rlwrap", "ledit"]
  , files = [] }

main :: IO ()
main
  = runM
  . runReader static
  . evalState dynamic
  $ mainPlan @E.Env @S.Cmd
