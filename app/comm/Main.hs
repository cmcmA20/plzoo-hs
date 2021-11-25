module Main where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.Runtime.IO
import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Lens
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16)
import GHC.Generics (Generic)
import Options.Applicative

import Compile qualified as C
import Lexer qualified as L
import Machine qualified as M
import Parser qualified as P
import Syntax qualified as S
import Zoo

data Clo = MkClo
  { ram      :: !Word16
  , showCode :: !Bool }
--   , showMem  :: !Bool } NYI
  deriving (Generic, Show)

-- wonky
data Ctx = MkCtx
  { clo          :: !Clo
  , lastCompiled :: !(Maybe M.Program) }
  deriving (Generic, Show)

type Sem = ()

ln :: LangName
ln = MkLangName "comm"

opts :: LangOpts Clo
opts = MkLangOpts $ MkClo
  <$> option auto (long "ram"
    <> help "RAM size"
    <> showDefault
    <> value 64)
  <*> switch (long "code"
    <> help "Print compiled code")
--   <*> switch (long "mem"
--     <> help "Print memory layout")

ini :: LangInit Clo Ctx
ini = MkLangInit \o -> MkCtx
  { clo = o
  , lastCompiled = Nothing }

toplevelParser :: Has (Throw SyntaxError) sig m => Text -> m S.Cmd
toplevelParser t = case L.runAlex (T.unpack t) P.program of
  Left  err -> throwError $ alexErrorToSyntaxError err
  Right x   -> pure x

fileParser :: Has (Throw SyntaxError) sig m => Text -> m [S.Cmd]
fileParser t = case L.runAlex (T.unpack t) P.file of
  Left  err -> throwError $ alexErrorToSyntaxError err
  Right x   -> pure x

exec :: LangExec S.Cmd Sem Ctx
exec = MkLangExec \c -> do
  p <- C.compile c
  modify @Ctx (& #lastCompiled ?~ p)
  rs <- gets @Ctx $ ram . clo
  let blankMachine = M.mkMachineState p rs
  z <- evalState blankMachine $ runThrow @M.MachineError $ M.runProgram
  liftEither $ first translateErrors z
  where
    translateErrors :: M.MachineError -> LangError
    translateErrors = LERuntime . locate Nothing . T.pack . show

pp :: LangPP Sem Ctx
pp = MkLangPP $ const do
  sc <- asks $ showCode . clo
  lc <- asks lastCompiled
  pure if sc
    then maybe "" (T.pack . show) lc
    else ""

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
  $ zooMain @Clo @S.Cmd @Sem @Ctx
