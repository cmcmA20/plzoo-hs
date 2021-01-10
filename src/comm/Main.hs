module Main where

import           Control.Applicative
import           Control.Carrier.Lift
import           Control.Carrier.Reader
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

type Clo = ()

ln :: LangName
ln = MkLangName "comm"

opts :: LangOpts Clo
opts = MkLangOpts $ pure ()

ini :: LangInit Clo M.Ctx
ini = MkLangInit id

toplevelParser :: Has (Throw SyntaxError) sig m => Text -> m S.Cmd
toplevelParser t = case L.runAlex (T.unpack t) P.program of
  Left  err -> throwError $ alexErrorToSyntaxError err
  Right x   -> pure x

fileParser :: Has (Throw SyntaxError) sig m => Text -> m [S.Cmd]
fileParser t = case L.runAlex (T.unpack t) P.file of
  Left  err -> throwError $ alexErrorToSyntaxError err
  Right x   -> pure x

exec :: LangExec S.Cmd M.Sem M.Ctx
exec = MkLangExec \c -> do
  p <- C.compile c
  let blankMachine = M.mkMachineState p 64 -- TODO RAM size
  z <- evalState blankMachine $ runThrow @M.MachineError $ M.runProgram
  liftEither $ first translateErrors z
  where
    translateErrors :: M.MachineError -> LangError
    translateErrors = LERuntime . locate Nothing . T.pack . show

pp :: LangPP M.Sem M.Ctx
pp = MkLangPP $ const $ pure ""

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
  $ zooMain @Clo @S.Cmd @M.Sem @M.Ctx
