module Main where

import           Control.Applicative
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.Resumable.Resume
import           Control.Carrier.State.Strict
import           Control.Carrier.Throw.Either
import           Data.Bifunctor (first)
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Compile as C
import qualified Lexer   as L
import qualified Machine as M
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

runMachine :: Evaluator M.Sem M.Ctx S.Cmd
runMachine e c = (result, e)
  where
    translateErrors :: M.MachineError -> LangError
    translateErrors = LERuntime . locate Nothing . T.pack . show

    result :: Either LangError M.Sem
    result = do
      is <- C.compile c
      let blankMachine = M.mkMachineState is 64
      first translateErrors $
        run $ evalState blankMachine $ runResumable @(Const M.MachineEffect) undefined $ runThrow @M.MachineError M.runProgram

static :: LangStatic M.Sem M.Ctx S.Cmd
static = MkLangStatic
  { name = "comm"
  , options = []
  , fileParser = Just file
  , toplevelParser = Just top
  , rts = liftToRTS runMachine (const RANop) }

dynamic :: LangDynamic M.Sem M.Ctx
dynamic = MkLangDynamic
  { environment = mkRuntimeEnv $ M.MkCtx 0 0
  , interactiveShell = True
  , wrapper = defaultWrapper
  , files = [] }

main :: IO ()
main
  = runM
  . runReader static
  . evalState dynamic
  $ mainPlan @M.Sem @M.Ctx @S.Cmd
